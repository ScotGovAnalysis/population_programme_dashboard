
##################################################################
##                              UI                              ##
##################################################################

ui <- fluidPage(
  
  includeCSS("www/style.css"),

  titlePanel(title = div(tagList(a(img(src = "saltire_logo.PNG", 
                           height = 50),
                           href = "https://www.gov.scot/", 
                           target="_blank")), 
                         br(),
                         br(),
             "Population Programme"), 
             windowTitle = "Population Programme"),
  
  uiOutput("subheader"),
  
  br(),

  mainPanel(tabsetPanel(
    type = "tabs",

    tabPanel(
      "Indicators",
      fluidRow(column(6),

               column(3,
                 selectInput(
                   "council_1",
                   label = "Select Council Area",
                   choices = council_areas$area,
                   selected = "City of Edinburgh")),

               column(3,
                 selectInput(
                   "council_2",
                   label = "Select Council Area",
                   choices = council_areas$area,
                   selected = "Glasgow City")
                 )
               ),
      
      htmlwidgets::getDependency('sparkline'),
      dataTableOutput("table1"),
    #  dataTableOutput("table2"),
      br(),
      br()
    ),

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
             uiOutput("about"))
    )
  )
)
