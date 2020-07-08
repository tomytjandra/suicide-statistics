# -------------------- IMPORT PACKAGES
# Data Wrangling
library(dplyr)
library(tidyr)
library(lubridate)
library(padr)
library(zoo)
library(countrycode) # geocoding
library(stringr)

# Visualization
library(ggplot2)
library(plotly)
library(leaflet)
library(htmltools) # hover
library(tidytext)
library(scales)

# Dashboard
library(shinydashboard)
library(dashboardthemes)
library(shinycssloaders)
library(shinyjs)
library(DT)

# -------------------- DATA PREPARATION

suicide_raw <- read.csv("data/master.csv")

mapping_country <-
  data.frame(
    from = c("Bahamas", "Cabo Verde", "Macau", "Republic of Korea", "Russian Federation", "Saint Vincent and Grenadines", "Serbia", "United States"),
    to = c("The Bahamas", "Cape Verde", "Macao S.A.R", "South Korea", "Russia", "Saint Vincent and the Grenadines", "Republic of Serbia", "United States of America")
  )

suicide <-
  suicide_raw %>% 
  select(-X, -country.year) %>% 
  rename(gdp_for_year = gdp_for_year....,
         gdp_per_capita = gdp_per_capita....) %>%
  
  # coerce
  mutate(sex = as.factor(sex),
         age = gsub(" years", "", age),
         age = factor(as.factor(age),
                      levels = c("5-14", "15-24", "25-34", "35-54", "55-74", "75+")
         ),
         gdp_for_year = as.numeric(gsub(',', '', gdp_for_year))
  ) %>% 
  
  # change country name
  left_join(mapping_country,
            by = c("country" = "from")) %>% 
  mutate(country = coalesce(to, country)) %>% 
  select(-to) %>% 
  
  # group country to continent
  mutate(continent = countrycode(sourcevar = country,
                                 origin = "country.name",
                                 destination = "continent"))

# -------------------- LEAFLET

map_data <-
  suicide %>% 
  group_by(country) %>% 
  summarise(total_suicides = sum(suicides_no),
            total_pop = sum(population)) %>% 
  ungroup() %>% 
  mutate(rate = 100000*total_suicides/total_pop)

countries <- readRDS("data/countries.geojson.RDS")

countries@data <-
  countries@data %>% 
  left_join(map_data, by = c("ADMIN" = "country")) %>% 
  mutate(label_rate = paste0('<b>', ADMIN, '</b><br>',
                             ifelse(
                               is.na(total_suicides),
                               'Data Not Available',
                               paste0('Rate: ', round(rate, 2),
                                      ' per 100,000<br>',
                                      'Cases: ', format(total_suicides,
                                                        big.mark = ',')
                                      )
                               )
                             )
         )

pal <- colorBin(
  palette = "Reds",
  domain = map_data$rate,
  bins = 5,
  na.color = "#FFFFFF",
  reverse = FALSE
)

# -------------------- LINE PLOT

continent_worldwide_line <-
  suicide %>%
  group_by(year) %>% 
  summarise(rate = 100000*sum(suicides_no)/sum(population)) %>% 
  mutate(continent = "Worldwide") %>% 
  select(continent, everything())

continent_line_data <-
  suicide %>% 
  group_by(continent, year) %>% 
  summarise(rate = 100000*sum(suicides_no)/sum(population))%>% 
  ungroup() %>% 
  rbind(continent_worldwide_line) %>% 
  filter(rate != 0) %>% 
  mutate(label = paste0('<b>', continent, '</b>',
                        ' (', year, ')<br>',
                        'Rate: ', round(rate, 2), ' per 100,000'))

country_worldwide_line <-
  suicide %>%
  group_by(year) %>% 
  summarise(total_suicides = sum(suicides_no),
            total_pop = sum(population)) %>% 
  mutate(country = "Worldwide",
         rate = 100000*total_suicides/total_pop) %>% 
  select(country, everything())

line_data <-
  suicide %>% 
  group_by(country, year) %>% 
  summarise(total_suicides = sum(suicides_no),
            total_pop = sum(population)) %>% 
  ungroup() %>% 
  mutate(rate = 100000*total_suicides/total_pop) %>% 
  rbind(country_worldwide_line) %>% 
  mutate(label = paste0('<b>', country, '</b>',
                        ' (', year, ')<br>',
                        'Rate: ', round(rate, 2), ' per 100,000'))

# -------------------- BAR PLOT

worldwide_bar <-
  suicide %>%
  group_by(sex) %>% 
  summarise(rate = 100000*sum(suicides_no)/sum(population)) %>% 
  ungroup() %>% 
  mutate(continent = "Worldwide") %>% 
  select(continent, everything())

# -------------------- SCATTER PLOT

scatter_data <-
  suicide %>% 
  group_by(country, year) %>% 
  summarise(gdp_per_cap = gdp_for_year/sum(population),
            HDI = mean(HDI.for.year)) %>% 
  ungroup() %>% 
  unique() %>% 
  
  # join data
  left_join(
    line_data %>% 
      filter(country != "Worldwide") %>% 
      select(country, year, rate, total_pop),
    by = c("country", "year")) %>% 
  mutate(continent = countrycode(sourcevar = country,
                                 origin = "country.name",
                                 destination = "continent")) %>% 
  
  # padding
  mutate(year = as.Date(as.yearmon(year))) %>% 
  pad(group = "country") %>% 
  mutate(year = year(year)) %>% 
  fill(-c("HDI"), .direction = "down") %>% 
  
  mutate(label_gdp = paste0('<b>', country, '</b><br>',
                            'GDPPC: $', format(round(gdp_per_cap, 2),
                                               big.mark = ','), '<br>',
                            'Rate: ', round(rate, 2), ' per 100,000<br>',
                            'Population: ', format(total_pop, big.mark = ',')),
         label_hdi = paste0('<b>', country, '</b><br>',
                            'HDI: ', format(round(HDI, 2), big.mark = ','), '<br>',
                            'Rate: ', round(rate, 2), ' per 100,000<br>',
                            'Population: ', format(total_pop, big.mark = ',')))