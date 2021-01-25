
##################################################################
##                              UI                              ##
##################################################################

ui <- fluidPage(
  titlePanel("Population Programme"),
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
                   label = "Select Coucil Area",
                   choices = area_names$area,
                   selected = "City of Edinburgh")),

               column(3,
                 selectInput(
                   "council_2",
                   label = "Select Coucil Area",
                   choices = area_names$area,
                   selected = "Glasgow City")
                 )
               ),

      htmlOutput("table1"),
      htmlOutput("table2"),
      htmlOutput("table3")
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
