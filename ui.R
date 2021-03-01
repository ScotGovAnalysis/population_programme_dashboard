ui <- navbarPage(  
                   
  # Sourcing custom CSS ----
  tags$head(includeCSS("www/style.css")),
  

  windowTitle = "Population Dashboard",
  tabPanel(
    "Home",
    tags$html(lang = "en"),
    icon = icon("home"), 
    HTML("<script src='https://cc.cdn.civiccomputing.com/8/cookieControl-8.x.min.js'></script>"),
    HTML("<script async src='https://www.googletagmanager.com/gtag/js?id=UA-91956629-1'></script>"),
    tags$script(src = "cookie_control_config.js"),
    
    #mainPanel(
    fluidRow(column(2),
             column(
               10,
               h1("Population Programme"),
               uiOutput("subheader")
             )),
    hr(),
    fluidRow(
      column(6),
      column(
        2,
        selectInput(
          "council_1",
          label = "Council area selection 1:",
          choices = council_areas$area,
          selected = "Aberdeen City"
        )
      ),
      column(
        2,
        selectInput(
          "council_2",
          label = "Council area selection 2:",
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
  
  # tabPanel(
  #   "Narrative",
  #   icon = icon("bar-chart-o"),
  #   selectInput(
  #     "indicator",
  #     label = NULL,
  #     choices = indicator_order,
  #     selected = "Population Structure"
  #   ),
  #   h3("Under construction: Interactive detailed charts")
  # ),
  
  tabPanel(
    "About",
    icon = icon("info-circle"),
    
    column(2),
    column(8,
           uiOutput("about")),
    column(2)
  ),
  
  tabPanel(
    "Accessibility",
    icon = icon("universal-access"),
    column(2),
  column(8,
         uiOutput("accessibility")),
   column(2)
))


# tabPanel(tags$a(img(src = "scotgovlogo.svg",
#                        height = 30),
#          href = "https://www.gov.scot/", target="_blank"))