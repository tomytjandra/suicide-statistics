

header <- dashboardHeader(
    title = "Suicide Statistics",
    titleWidth = 150
)

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem(text = "Overview", tabName = "overview", icon = icon("lightbulb-o")),
        menuItem(text = "Map", tabName = "map", icon = icon("map")),
        menuItem(text = "Socio-Economic", tabName = "socio-eco", icon = icon("users")),
        menuItem(text = "Data", tabName = "data", icon = icon("table"))
    ),
    width = 150
)

body <- dashboardBody(
    useShinyjs(), # set up shinyjs
    
    tabItems(
        
        # -------------------- PAGE 1
        
        tabItem(
            tabName = "overview",
            
            # -------------------- VALUE BOXES
            
            fluidRow(
                valueBox(format(sum(suicide$suicides_no),
                                big.mark = ','), 
                         subtitle = "Total Cases",
                         icon = icon("user")),
                valueBox(round(100000*sum(suicide$suicides_no)/sum(suicide$population), 2), 
                         subtitle = "Suicide Rates (per 100K)",
                         icon = icon("line-chart")),
                valueBox(length(unique(suicide$country)), 
                         subtitle = "Countries",
                         icon = icon("globe"))
            ),
            
            br(),
            
            # --------------------- LINE PLOT
            
            fluidRow(
                column(
                    width = 12,
                    plotlyOutput(outputId = "lineContinent") %>% 
                        withSpinner(color = "red")
                )
            ),
            
            br(),
            br(),
            
            # --------------------- BAR PLOT
            
            fluidRow(
                column(
                    width = 6,
                    plotlyOutput(outputId = "barContinent") %>% 
                        withSpinner(color = "red")
                ),
                column(
                    width = 6,
                    plotlyOutput(outputId = "barCountry") %>% 
                        withSpinner(color = "red")
                )
            )
        ),
        
        # -------------------- PAGE 2
        
        tabItem(
            tabName = "map",
            
            fluidRow(
                column(
                    width = 12,
                    leafletOutput(outputId = "leaflet", height = 250) %>% 
                        withSpinner(color = "red")
                )
            ),
            
            br(),
            
            fluidRow(
                column(
                    width = 12,
                    actionButton(inputId = "reset", label = "Reset")
                )
            ),
            
            br(),
            
            fluidRow(
                column(
                    width = 6,
                    plotlyOutput(outputId = "line")
                ),
                column(
                    width = 6,
                    plotlyOutput(outputId = "age_bar")
                )
            ),
            
            fluidRow(
                hidden(selectizeInput(inputId = "chooseCountry",
                                      label = "Choose Country",
                                      choices = c("Worldwide", sort(unique(suicide$country))),
                                      selected = "Worldwide",
                                      multiple = TRUE,
                                      options = list(maxItems = 2))
                )
            )
        ),
        
        # -------------------- PAGE 3
        
        tabItem(
            tabName = "socio-eco",
            fluidRow(
                column(
                    width = 9,
                    plotlyOutput(outputId = "scatterGDP") %>% 
                        withSpinner(color = "red")
                ),
                box(
                    width = 3,
                    title = p("GDPPC", style = "font-weight:bold; font-size: 30px;"),
                    background = "aqua",
                    tagList(
                        a("Gross Domestic Product per Capita",
                          href = "https://www.who.int/data/gho/indicator-metadata-registry/imr-details/1145",
                          target = "_blank",
                          style = "color:blue; text-decoration:underline; font-weight:bold; font-size: 18px;"),
                        p("is the average per capita market value of the sum of gross values added of all resident institutional units engaged in production, for a given national economy, at a given period in time, usually a year, expressed in international dollars using purchasing power parity rates.",
                          style = "font-size: 18px;"),
                    )
                )
            ),
            
            br(),
            br(),
            
            fluidRow(
                box(
                    width = 3,
                    title = p("HDI", style = "font-weight:bold; font-size: 30px;"),
                    background = "aqua",
                    tagList(
                        a("Human Development Index",
                          href = "http://hdr.undp.org/en/content/human-development-index-hdi",
                          target = "_blank",
                          style = "color:blue; text-decoration:underline; font-weight:bold; font-size: 18px;"),
                        p("is a summary measure of average achievement in key dimensions of human development: a long and healthy life, being knowledgeable and have a decent standard of living. The HDI is the geometric mean of normalized indices for each of the three dimensions.",
                          style = "font-size: 18px;"),
                    )
                ),
                column(
                    width = 9,
                    plotlyOutput(outputId = "scatterHDI") %>% 
                        withSpinner(color = "red")
                )
            )
        ),
        
        # -------------------- PAGE 4
        
        tabItem(
            tabName = "data",
            fluidRow(
                box(
                    width = 12,
                    DT::dataTableOutput("dataTable") %>% 
                        withSpinner(color = "red")
                )
            )
        )
        
    ),
    shinyDashboardThemes(
        theme = "flat_red"
    ),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    )
)

dashboardPage(header = header, sidebar = sidebar, body = body)