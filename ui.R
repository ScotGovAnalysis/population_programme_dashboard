ui <- navbarPage(position = "fixed-top",  
                   
  # Sourcing custom CSS ----
  tags$head(includeCSS("www/style.css")),
  

  windowTitle = "Population Dashboard",
  
  tabPanel(
    "Home",
    useShinyalert(),
    tags$html(lang = "en"),
    icon = icon("home"), 
    HTML("<script src='https://cc.cdn.civiccomputing.com/8/cookieControl-8.x.min.js'></script>"),
    HTML("<script async src='https://www.googletagmanager.com/gtag/js?id=UA-91956629-1'></script>"),
    tags$script(src = "cookie_control_config.js"),
    
    #mainPanel(
    fluidRow(column(1),
             column(8,
               h1("Population Programme"),
               uiOutput("subheader")),
             column(2, #align = "right",
                    style = "margin-top: 25px;",
                    tags$a(img(src = "scotgovlogo.svg",
                               height = 60,
                               alt = "Scottish Government Logo"), 
                           href = "https://www.gov.scot/")),
             column(1)),
    hr(),
    fluidRow(
      column(1),
      column(6, 
             uiOutput("key")),
      column(4,
             fluidRow(column(6,
                    style = "margin-top: 100px;",
        selectInput(
          "council_1",
          label = "Select council area 1:",
          choices = council_areas$area,
          selected = "Aberdeen City")),
        column(6,
             style = "margin-top: 100px;",
        selectInput(
          "council_2",
          label = "Select council area 2:",
          choices = council_areas$area,
          selected = "Aberdeenshire")))),
      column(1)), 
    
    htmlwidgets::getDependency('sparkline'),
    fluidRow(
      column(1),
      column(10,
             align = "center",
             dataTableOutput("table1")),
      column(1)),
    br(),
    br()),
  
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