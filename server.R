function(input, output, session) {
    
    # --------------------- PAGE 1: LINE PLOT
    
    output$lineContinent <- renderPlotly({
        continent_line_plot <-
            ggplot(data = continent_line_data,
                   aes(x = year, y = rate,
                       group = continent, color = continent,
                       text = label)) +
            geom_line(lwd = 0.75) +
            geom_point(size = 1) +
            scale_x_continuous(breaks = seq(1985, 2016, by = 5),
                               limits = c(1985, 2016)) +
            labs(x = "Year", y = "Suicide per 100,000 population",
                 title = "<b>Suicide Rates Over the Years</b>",
                 color = "") +
            theme_minimal()
        
        ggplotly(continent_line_plot, tooltip = "text")
    })
    
    # --------------------- PAGE 1: BAR PLOT
    
    output$barContinent <- renderPlotly({
        
        continent_bar_data <-
            suicide %>% 
            group_by(continent, sex) %>% 
            summarise(rate = 100000*sum(suicides_no)/sum(population)) %>% 
            ungroup() %>% 
            group_by(sex) %>% 
            rbind(worldwide_bar) %>% 
            mutate(
                sex = str_to_title(sex),
                rank = dense_rank(desc(rate)),
                label = paste0(
                    '<b>', continent ,'</b><br>',
                    'Rate: ', round(rate, 2), ' per 100,000'))
        
        continent_bar_plot <-
            ggplot(data = continent_bar_data,
                   aes(x = rate,
                       y = reorder_within(continent, -rank, rate),
                       text = label,
                       key = continent)) +
            geom_col(aes(fill = sex)) +
            scale_y_reordered() +
            labs(x = "Suicide per 100,000 population", y = "") +
            facet_wrap(~sex, ncol = 1, scales = "free") +
            theme_minimal() +
            theme(legend.position = "none")
        
        ggplotly(continent_bar_plot, tooltip = "text",
                 source = "barContinent") %>% 
            layout(margin = list(t = 60, b = 60),
                   title = list(x = 0,
                                y = 0.95,
                                text = paste0('<b>Continents by Suicide Rates</b>',
                                              '<br>',
                                              '<sup>',
                                              'Separated by Gender',
                                              '</sup>')))
    })
    
    click_data <- reactiveValues(continent = "Worldwide")
    
    observeEvent(event_data("plotly_hover", source = "barContinent"), {
        event <- event_data("plotly_hover", source = "barContinent")
        click_data$continent <- event$key
    })
    
    output$barCountry <- renderPlotly({
        select_continent <- click_data$continent
        
        sex_bar_data <-
            suicide %>%
            {if(select_continent != "Worldwide")
                filter(., continent == select_continent)
                else .} %>% 
            group_by(country, sex) %>% 
            summarise(rate = 100000*sum(suicides_no)/sum(population)) %>% 
            ungroup() %>% 
            group_by(sex) %>% 
            mutate(
                sex = str_to_title(sex),
                rank = dense_rank(desc(rate)),
                label = paste0(
                    '<b>', country ,'</b><br>',
                    'Rate: ', round(rate, 2), ' per 100,000')) %>% 
            filter(rank <= ifelse(max(rank) < 10, max(rank), 10))
        
        sex_bar_plot <-
            ggplot(data = sex_bar_data,
                   aes(x = rate,
                       y = reorder_within(country, -rank, rate),
                       text = label)) +
            geom_col(aes(fill = sex)) +
            scale_y_reordered() +
            labs(x = "Suicide per 100,000 population", y = "") +
            facet_wrap(~sex, ncol = 1, scales = "free") +
            theme_minimal() +
            theme(legend.position = "none")
        
        ggplotly(sex_bar_plot, tooltip = "text") %>% 
            layout(margin = list(t = 60, b = 60),
                   title = list(x = 0,
                                y = 0.95,
                                text = paste0('<b>Top ', max(sex_bar_data$rank) ,' Countries ', 
                                              {if(select_continent != "Worldwide") 'in '}, select_continent,
                                              '</b>',
                                              '<br>',
                                              '<sup>',
                                              'Separated by Gender',
                                              '</sup>')))
    })
    
    
    
    # -------------------- PAGE 2: LEAFLET
    
    output$leaflet <- renderLeaflet({
        leaflet(countries,
                options = tileOptions(minZoom = 1, maxZoom = 4)) %>%
            addTiles() %>% 
            addProviderTiles("CartoDB.Voyager") %>%
            setView(lng = 0, lat = 25, zoom = 1) %>% 
            setMaxBounds(lng1 = -100, lat1 = -90,
                         lng2 = 80, lat2 = 90) %>%  
            addPolygons(stroke = FALSE,
                        smoothFactor = 0.5,
                        fillOpacity = 0.7,
                        color = ~pal(rate),
                        label = lapply(as.list(countries@data$label_rate), 
                                       HTML),
                        layerId = ~label_rate) %>% 
            addLegend("bottomleft",
                      pal = pal,
                      values = ~rate,
                      title = "Suicide Rates")
    })
    
    observeEvent(input$leaflet_shape_click, {
        event <- input$leaflet_shape_click
        
        # get country name
        split <- str_split(event$id, "<br>")[[1]]
        country_name <- gsub(x = split[1], pattern = "<b>|</b>", replacement = '')
        
        # update select input if data is available
        if(!(country_name %in% input$chooseCountry)){
            if(split[2] != "Data Not Available"){
                showModal(modalDialog(title = "Plot Updated!",
                                      paste("Country: ", country_name)))
                updateSelectInput(session,
                                  inputId = "chooseCountry",
                                  selected = c("Worldwide", country_name))
            }
        }
    })
    
    # -------------------- PAGE 2: LINE PLOT
    
    output$line <- renderPlotly({
        line_plot <-
            ggplot(data = line_data %>% 
                       filter(country %in% input$chooseCountry),
                   aes(x = year, y = rate,
                       group = country, color = country,
                       text = label)) +
            geom_line(lwd = 0.75) +
            geom_point(size = 1) +
            scale_x_continuous(breaks = seq(1985, 2016, by = 5),
                               limits = c(1985, 2016)) +
            labs(x = "Year", y = "Suicide per 100,000 population",
                 title = "<b>Suicide Rates Over the Years</b>",
                 color = "Country") +
            theme_minimal()
        
        ggplotly(line_plot, tooltip = "text") %>% 
            layout(legend = list(orientation = "h",
                                 xanchor = "center",
                                 x = 0.5,
                                 y = -0.25))
    })
    
    # -------------------- PAGE 2: BAR PLOT
    
    output$age_bar <- renderPlotly({
        select_country <- tail(input$chooseCountry, n=1)
        
        age_bar_data <-
            suicide %>% 
            {if(select_country != "Worldwide")
                filter(., country == select_country)
                else .} %>% 
            group_by(age, sex) %>% 
            summarise(total_suicides = sum(suicides_no),
                      total_pop = sum(population)) %>% 
            ungroup() %>% 
            mutate(rate = 100000*total_suicides/total_pop) %>%
            mutate(sex = str_to_title(sex),
                   label = paste0(
                       '<b>', sex, ' (', age, ' Years)</b><br>',
                       'Rate: ', round(rate, 2), ' per 100,000'))
        
        age_bar_plot <-
            ggplot(data = age_bar_data,
                   aes(x = age, y = rate, text = label)) +
            geom_col(aes(fill = sex), position = "dodge") +
            labs(x = "Age Group (Years)",
                 y = "Suicide per 100,000 population",
                 fill = "") +
            theme_minimal()
        
        ggplotly(age_bar_plot, tooltip = "text") %>%
            layout(legend = list(orientation = "h",
                                 xanchor = "center",
                                 x = 0.5,
                                 y = -0.25),
                   margin = list(t = 70),
                   title = list(x = 0,
                                y = 0.95,
                                text = paste0('<b>Suicide Rates by Age and Gender</b><br>',
                                              '<sup>',
                                              select_country,
                                              '</sup>')))
    })
    
    
    
    # -------------------- PAGE 3: SCATTER PLOT
    
    output$scatterGDP <- renderPlotly({
        scatter_gdp_plot <- 
            ggplot(data = scatter_data,
                   aes(x = gdp_per_cap, y = rate,
                       frame = year, text = label_gdp)) +
            geom_point(aes(size = total_pop,
                           color = continent), alpha = 0.5) +
            scale_x_continuous(labels = dollar_format(prefix = '$')) +
            labs(x = "GDPPC",
                 y = "Suicide per 100,000 population",
                 title = "<b>Suicide Rates vs GDPPC Over the Years</b>") +
            guides(size = FALSE) +
            theme_minimal()
        
        ggplotly(scatter_gdp_plot, tooltip = "text") %>%
            layout(legend = list(orientation = "h",
                                 xanchor = "center",
                                 x = 0.5,
                                 y = 1.2),
                   margin = list(t = 70),
                   title = list(x = 0,
                                y = 0.99)) %>% 
            animation_opts() %>% 
            animation_slider(
                currentvalue = list(prefix = "Year ")
            )
    })
    

    output$scatterHDI <- renderPlotly({
        scatter_hdi_plot <-
            ggplot(data = scatter_data %>% filter(!is.na(HDI)),
                   aes(x = HDI, y = rate,
                       frame = year, text = label_hdi)) +
            geom_point(aes(size = total_pop,
                           color = continent), alpha = 0.5) +
            scale_x_continuous(labels = comma) +
            labs(x = "HDI",
                 y = "Suicide per 100,000 population",
                 title = "<b>Suicide Rates vs HDI Over the Years</b>") +
            guides(size = FALSE) +
            theme_minimal()
        
        ggplotly(scatter_hdi_plot, tooltip = "text") %>%
            layout(legend = list(orientation = "h",
                                 xanchor = "center",
                                 x = 0.5,
                                 y = 1.2),
                   margin = list(t = 70),
                   title = list(x = 0,
                                y = 0.99)) %>% 
            animation_opts() %>% 
            animation_slider(
                currentvalue = list(prefix = "Year ")
            )
    })
    
    # -------------------- PAGE 4: DATA TABLE
    
    output$dataTable = DT::renderDataTable({
        DT::datatable(suicide,
                      style = 'bootstrap',
                      class = 'table-bordered table-condensed',
                      options = list(scrollX = TRUE,
                                     dom = 'frtip',
                                     columnDefs = list(list(className = 'dt-center',
                                                            targets = '_all')
                                                       )
                                     )
                      )
    })
}