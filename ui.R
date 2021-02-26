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
  HTML("<h1>Accessibility</h1>
    
<p>Last updated: <b>15 March 2021</b></p>

<p>This website is run by the Scottish Government.</p>

<p>We want it to be accessible and usable for as many people as possible.</p>

<h2>Feedback</h2>  

<a href = 'https://scotland.shinyapps.io/population_programme/#Contact'><u>Contact us</u></a> if you:  

<li>Cannot access any part of this site or want to report an accessibility problem.</li>
<li>Need information on this website in a different format. Such as accessible PDF, large print, easy read, audio recording or braille. We’ll consider your request and get back to you as soon as we can.</li>  

<h3>Enforcement</h3>

<p>If you’re not happy with how we respond to your feedback, contact the <a href = 'https://www.equalityadvisoryservice.com/'><u>Equality Advisory and Support Service</u></a>.
They are an independent advice service. They will advise you on what to do next.</p>

<h2>Compliance</h2>

<p>Scottish Government is committed to making its websites accessible, in accordance with the Public Sector Bodies (Websites and Mobile Applications) (No. 2) 
<a href = 'http://www.legislation.gov.uk/uksi/2018/952/regulation/4/made'><u>Accessibility Regulations 2018</u></a>.</p>

<p>This accessibility statement applies to the 
<a href = 'https://scotland.shinyapps.io/population_dashboard/'><u>Population Programme</u></a> data website. </p>

<h3>Compliance status</h3>

<p>This website is partially compliant with the regulations, due to the non-compliances listed below.</p>
<h3>Non-accessible content</h3>

The content listed below is not-compliant with the regulations:  
<li>The small charts in the Home page table are not accessible for screen readers.</li>
<li>Some text and shading does not appear in high contrast mode - mostly tooltips within the table.</li>

<h3>Preparation of statement</h3>

<p>This statement was prepared on <b>15 March 2021</b>.</p>

<p>These web pages were reviewed <b>February 2021</b>.</p>")),
   column(2),
#  uiOutput("accessibility")
))


# tabPanel(tags$a(img(src = "scotgovlogo.svg",
#                        height = 30),
#          href = "https://www.gov.scot/", target="_blank"))