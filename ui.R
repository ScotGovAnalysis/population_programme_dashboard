includeCSS("www/style.css")
# tags$style("")
# tags$style("")
ui <- navbarPage(tagList(a(img(src = "saltire_logo.PNG",
                               height = 30),
                           href = "https://www.gov.scot/",
                           target="_blank")),
                 
                 
                 windowTitle = "Population Programme",
                 tabPanel("Indicators",
                          
                          mainPanel(
                            h1("Population Programme"),
                            uiOutput("subheader"),
                            hr(),
                            fluidRow(column(5,
                                            h4("Select 2 council areas to compare:")),
                                     
                                     column(3,
                                            selectInput(
                                              "council_1",
                                              label = NULL,
                                              choices = council_areas$area,
                                              selected = "Aberdeen City")),
                                     column(3,
                                            selectInput(
                                              "council_2",
                                              label = NULL,
                                              choices = council_areas$area,
                                              selected = "Aberdeenshire")
                                     )),
                            hr(),
                            
                            htmlwidgets::getDependency('sparkline'),
                            dataTableOutput("table1"),
                            br(),
                            br()
                          )),
                 
                 tabPanel(
                   "Criteria",
                   h3("Population Structure"),
                   h4("Population size"),
                   uiOutput("pop_size"),
                   h4("Healthy life expectancy"),
                   uiOutput("healthy_life_expectancy"),
                   h4("Active dependency ratio"),
                   uiOutput("active_dependency_ratio"),
                   h3("Population Distribution"),
                   h4("Population change by council"),
                   uiOutput("pop_change_by_council"),
                   h4("Population change within councils"),
                   uiOutput("pop_change_within_council")
                 ),
                 
                 tabPanel("About",
                          h1("Lorem Ipsum"),
                          uiOutput("about")))