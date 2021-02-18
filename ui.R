ui <- navbarPage(
  # Sourcing custom CSS ----
  tags$head(includeCSS("www/style.css")),
  
  windowTitle = "Population Programme",
  tabPanel(
    "Home",
    icon = icon("home"),
    
    #mainPanel(
    fluidRow(column(2),
             column(
               10,
               h1("Population Programme"),
               uiOutput("subheader")
             )),
    hr(),
    fluidRow(
      column(2),
      column(4,
             h4("Select 2 council areas to compare:")),
      column(
        2,
        selectInput(
          "council_1",
          label = NULL,
          choices = council_areas$area,
          selected = "Aberdeen City"
        )
      ),
      column(
        2,
        selectInput(
          "council_2",
          label = NULL,
          choices = council_areas$area,
          selected = "Aberdeenshire"
        )
      ),
      column(2)
    ),
    
    htmlwidgets::getDependency('sparkline'),
    
    fluidRow(
      column(2),
      column(8,
             align = "center",
             dataTableOutput("table1")),
      column(2)),
    
    fluidRow(align = "center",
             uiOutput("key"),
             br(),
             br())),
  
  tabPanel(
    "Narrative",
    icon = icon("bar-chart-o"),
    selectInput(
      "indicator",
      label = NULL,
      choices = indicator_order,
      selected = "Population Structure"
    ),
    h3("Under construction: Interactive detailed charts")
  ),
  
  tabPanel(
    "About",
    icon = icon("search"),
    column(
      6,
      h2("Definitions"),
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
    column(6,
           h2("About"),
           uiOutput("about"))
  )
  # tabPanel(tags$a(img(src = "scotgovlogo.svg",
  #                        height = 30),
  #          href = "https://www.gov.scot/", target="_blank"))
)