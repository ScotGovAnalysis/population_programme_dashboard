##################################################################
##                            Server                            ##
##################################################################

server <- function(input, output, session) {
  
  # 
  # output$about <- about_page 
  # #output$accessibility <- accessibility_page 
  # 
  url <- a(HTML("<u>Population Taskforce</u>"), 
           href = "https://www.gov.scot/groups/population-task-force/")
  
  output$subheader <- renderUI({
    tagList(
      "This dashboard supports the",
      url,
      "to understand the demographic challenges in Scotland."
    )
  })
  output$subheader1 <- renderUI({
    tagList(
      "This dashboard supports the",
      url,
      "to understand the demographic challenges in Scotland."
    )
  })
  output$subheader2 <- renderUI({
    tagList(
      "This dashboard supports the",
      url,
      "to understand the demographic challenges in Scotland."
    )
  })
  
#################################################################
##                      Reactive Datasets                      ##
#################################################################
  net_within_scot_reactive <- reactive({
    
    council1 <- input$council_1
    council2 <- input$council_2
    
    scotland_symbol <- tibble(variable = paste("Within Scotland", as.character(icon("info-sign", lib = "glyphicon"))),
                              icon = "")
    
    council1_symbol <- create_symbols_council1(net_within_scotland,
                                               council1)
    
    council2_symbol <- create_symbols_council2(net_within_scotland,
                                               council2)
    
    combine_columns_and_symbols_within_scot(net_within_scotland, 
                                council1, 
                                council2, 
                                scotland_symbol, 
                                council1_symbol, 
                                council2_symbol,
                                "bar")
  })


  
# Population Structure ----------------------------------------------------

  pop_structure_age_reactive <- reactive({
    
    council1 <- input$council_1
    council2 <- input$council_2
    
    scotland_symbol <- create_symbols_scotland(pop_structure_age)
    
    council1_symbol <- create_symbols_council1(pop_structure_age,
                                               council1)
    
    council2_symbol <- create_symbols_council2(pop_structure_age,
                                               council2)
    
    combine_columns_and_symbols(pop_structure_age, 
                               council1, 
                               council2, 
                               scotland_symbol, 
                               council1_symbol, 
                               council2_symbol,
                               "line")
  })
  
  
# Active Dependency Ratio ----------------------------------------------------

  active_dependency_ratio_reactive <- reactive({
    
    council1 <- input$council_1
    council2 <- input$council_2
    
    scotland_symbol <- create_symbols_scotland(active_dependency_ratio)
    
    council1_symbol <- create_symbols_council1(active_dependency_ratio,
                                               council1)
    
    council2_symbol <- create_symbols_council2(active_dependency_ratio,
                                               council2)
    
    combine_columns_and_symbols(active_dependency_ratio, 
                               council1, 
                               council2, 
                               scotland_symbol, 
                               council1_symbol, 
                               council2_symbol,
                               "line")
  })
  
# Life expectency ----------------------------------------------------

  life_expectancies_reactive <- reactive({

    council1 <- input$council_1
    council2 <- input$council_2

    scotland_symbol <- create_symbols_scotland(life_expectancies)

    council1_symbol <- create_symbols_council1(life_expectancies,
                                               council1)

    council2_symbol <- create_symbols_council2(life_expectancies,
                                               council2)

    combine_columns_and_symbols(life_expectancies,
                               council1,
                               council2,
                               scotland_symbol,
                               council1_symbol,
                               council2_symbol,
                               "line")
  })

# Migration ---------------------------------------------------------------
  migration_reactive <- reactive({
    
    council1 <- input$council_1
    council2 <- input$council_2
    
    scotland_symbol <- create_symbols_scotland(migration_datasets)
    
    council1_symbol <- create_symbols_council1(migration_datasets,
                                               council1)
    
    council2_symbol <- create_symbols_council2(migration_datasets,
                                               council2)
    
    combine_columns_and_symbols(migration_datasets, 
                                council1, 
                                council2, 
                                scotland_symbol, 
                                council1_symbol, 
                                council2_symbol,
                                "bar")
    })
  
  
# Migration ---------------------------------------------------------------
  natural_change_reactive <- reactive({
    
    council1 <- input$council_1
    council2 <- input$council_2
    
    scotland_symbol <- create_symbols_scotland(natural_change)
    
    council1_symbol <- create_symbols_council1(natural_change,
                                               council1)
    
    council2_symbol <- create_symbols_council2(natural_change,
                                               council2)
    
    combine_columns_and_symbols(natural_change, 
                                council1, 
                                council2, 
                                scotland_symbol, 
                                council1_symbol, 
                                council2_symbol,
                                "bar")
  })
  
  # All other datasets -------------------------------------------
  
  pop_change_reactive <- reactive({
    
    council1 <- input$council_1
    council2 <- input$council_2
    
    scotland_symbol <- create_symbols_scotland(pop_change)
    
    council1_symbol <- create_symbols_council1(pop_change,
                                               council1)
    
    council2_symbol <- create_symbols_council2(pop_change,
                                               council2)
    
    combine_columns_and_symbols(pop_change, 
                                council1, 
                                council2, 
                                scotland_symbol, 
                                council1_symbol, 
                                council2_symbol,
                                "line")
  })

##################################################################
##                            Tables                            ##
##################################################################

  output$table1 <- renderDataTable({
    
    cb <- htmlwidgets::JS('function(){debugger;HTMLWidgets.staticRender();}')
    
    data <- pop_structure_age_reactive() %>% 
      rbind(active_dependency_ratio_reactive(),
            life_expectancies_reactive(),
            pop_change_reactive(),
            natural_change_reactive(),
            net_within_scot_reactive(),
            migration_reactive())
    
    colnames(data)[c(1,2, 4, 6, 8)] <- paste0('<span style="color:',"white",'">',
                                              colnames(data)[c(1,2, 4, 6, 8)],'</span>')
    
    dtable <- datatable(data,
      escape = FALSE,
      class = 'row-border',
      rownames = FALSE,
      selection = 'none',
      options = list(
        rowsGroup = list(0),
        drawCallback =  cb,
        rowCallback = JS(rcb),
        columnDefs = list(
          list( # sparkline columns
          className = 'dt-right',
          width = '100',
          targets = c(2, 4, 6)),
          list( # arrow columns
            className = 'dt-left',
            width = '1',
            targets = c(3, 5, 7))),
        dom = 'ft',
        lengthChange = FALSE,
        bInfo = FALSE,
        bPaginate = FALSE,
        bSort = FALSE,
        bFilter = FALSE
      ))
    
   # Merge the Indicators column
   # https://stackoverflow.com/questions/39484118/shiny-merge-cells-in-dtdatatable
    dep <- htmltools::htmlDependency(
      "RowsGroup", "2.0.0",
      "www/", script = "dataTables.rowsGroup.js")

    dtable$dependencies <- c(dtable$dependencies, list(dep))
   
   dtable
   })
  
  
#################################################################
##                             Key                             ##
#################################################################
  
  
  output$key <- renderText({
paste(as.character(icon("arrow-up", lib = "glyphicon")), "Increasing,",
       as.character(icon("arrow-down", lib = "glyphicon")), "Decreasing,",
       as.character(icon("minus", lib = "glyphicon")), "Maintaining", br(),
      "The arrows show the latest year vs the previous year", br(),
       "<b>", "*Data range is", current_year-12, "-", current_year-2, "\b"
       )
    
  })
#################################################################
##                            Plots                            ##
#################################################################
  
  output$pop_structure_plot <- plotly::renderPlotly(
    
    plotly::ggplotly(migration_datasets %>% 
                       filter(area == "Scotland") %>% 
               ggplot(aes(x = as.factor(period), y = value, group = 3, colour = variable)) +
               geom_line()
  )
  )
  
  
  
  output$about <- renderUI(
    HTML(
      "<h1>About</h1>
      <p>A <a href = 'https://www.gov.scot/groups/population-task-force/'><u>Population Taskforce</u></a> 
      and Population Programme were established in June 2019 by the Scottish Government to consider 
      Scotland’s future population challenges.</p>
      
      <p>The <a href = 'https://nationalperformance.gov.scot/'><u>National Performance Framework</u></a> 
      (NPF) includes an indicator on Scotland’s population under the National Outcome “we are open, 
      connected and make a positive contribution internationally”. This dashboard includes the NPF 
      indicator as well as a number of other indicators to monitor progress and understand the 
      challenges facing the population programme. These are structured around population structure 
      and distribution, priority areas agreed by the Population Taskforce.</p>
      
      <p>As agreed by the Population Taskforce, the Scottish Government’s aim is to make communities 
      across Scotland attractive places to live, work, bring up families and to move to; so that 
      Scotland’s population profile improves sustainable and inclusive economic growth and wellbeing.</p> 
      
      <h2>Contact</h2>
      
      <p>We welcome your feedback to improve this website:</p>
        
      <ul><li>Email: <u>joe.bloggs@gov.scot</li></u>
      <li>Phone: 0300 244 4000 (Scottish Government central enquiry unit)</li>
      <li>Scottish Government <a href = 'https://www.gov.scot/about/contact-information/'><u>general enquiries</u></a></ul></li>  
        
      <h2>Definitions</h2>
      <h3>Population structure</h3>  
      <p>Proportion of children, people aged 16 - 64, and people 65 and over.</p>
      
      <h3>Active dependency ratio</h3>  
      <p>Number of economically inactive people aged 16 and over that are economically inactive per 1,000 economically active:</p>  
        
      <ul><li>Performance improving if ratio has increased.</li>  
      <li>Performance maintaining if ratio has not changed.</li>  
      <li>Performance worsening if ratio has decreased.</ul></li>  
      
      <h3>Life expectancy</h3>  
      <p>Average number of years a new born baby could be expected to live:</p>
        
      <ul><li>Performance improving if both male and female LE has significantly increased or if LE of one sex has significantly increased and there hasn’t been a significant change in the other.</li>  
      <li>Performance maintaining if male and female LE has significantly changed in opposite directions or if there hasn’t been a significant change for males and females.</li>  
      <li>Performance worsening if both male and female LE has significantly decreased or if LE of one sex has significantly decreased and there hasn’t been a significant change in the other.</ul></li>  
      
      <h3>Healthy life expectancy</h3> 
      <p>Average number of years a new born baby could be expected to live in ‘good’ or ‘very good’ health:</p>
        
      <ul><li>Performance improving if both male and female HLE has significantly increased or if HLE of one sex has significantly increased and there hasn’t been a significant change in the other.</li>
      <li>Performance maintaining if male and female HLE has significantly changed in opposite directions or if there hasn’t been a significant change for males and females.</li>
      <li>Performance worsening if both male and female HLE has significantly decreased or if HLE of one sex has significantly decreased and there hasn’t been a significant change in the other.</ul></li> 
      
      <h3>Population change</h3>  
      <h4>Council area</h4>
      <p>Number of councils experiencing population increase or decline:</p>
        
      <ul><li>Performance improving if number of councils increasing has increased.</li>  
      <li>Performance improving if number of councils decreasing has decreased.</li>  
      <li>Performance maintaining if number of councils has not changed.</li>  
      <li>Performance worsening if number of councils decreasing has increased.</li>  
      <li>Performance worsening if number of councils increasing has decreased.</ul></li>  
      
      <h4>Data zone</h4>
      <p>Percentage of datazones (small areas) experiencing population increase or decline:</p>  
        
      <ul><li>Performance improving if percentage of datazones increasing has increased.</li>  
      <li>Performance improving if percentage of datazones decreasing has decreased.</li>  
      <li>Performance maintaining if percentage of datazones has not changed.</li>  
      <li>Performance worsening if percentage of datazones decreasing has increased.</li>  
      <li>Performance worsening if percentage of datazones increasing has decreased.</li></ul>
      
      <h4>Natural Change</h4>
      <p>The number of births minus the number of deaths:</p>
        
      <ul><li>Performance improving if natural change has increased.</li>  
      <li>Performance maintaining if natural change has not changed.</li>  
      <li>Performance worsening if natural change has decreased.</ul></li>  
      
      <h3>Net Migration</h3>
      <h4>Within Scotland</h4>
      <p>Net migration from other areas within Scotland:</p>  
        
      <ul><li>Performance improving if within Scotland migration has increased.</li>  
      <li>Performance maintaining if within Scotland migration has not changed.</li>  
      <li>Performance worsening if within Scotland migration has decreased.</ul></li>  
      
      <h4>Rest of the UK</h4> 
      <p>Net migration from the rest of the UK:</p>
        
      <ul><li>Performance improving if rest of the UK migration has increased.</li>  
      <li>Performance maintaining if rest of the UK migration has not changed.</li>  
      <li>Performance worsening if rest of the UK migration has decreased.</ul></li>  
      
      <h4>Overseas</h4>  
      <p>Net migration from outside the UK:</p>
        
      <ul><li>Performance improving if overseas migration has increased.</li>  
      <li>Performance maintaining if overseas migration has not changed.</li>  
      <li>Performance worsening if overseas migration has decreased.</ul></li>  
      
      <h4>Total</h4>  
      <p>Net migration from other areas within Scotland and areas outwith Scotland:</p>  
        
      <ul><li>Performance improving if total migration has increased.</li>  
      <li>Performance maintaining if total migration has not changed.</li>  
      <li>Performance worsening if total migration has decreased.</ul></li>"
    )
  )
  
  
  output$accessibility <- renderUI(
  HTML("<h1>Accessibility</h1>
    
        <p>Last updated: <b>15 March 2021</b></p>
        
        <p>This website is run by the Scottish Government.</p>
        
        <p>We want it to be accessible and usable for as many people as possible.</p>
        
        <h2>Feedback</h2>  
        
        <a href = 'https://scotland.shinyapps.io/population_programme/#Contact'><u>Contact us</u></a> if you:  
        
        <ul><li>Cannot access any part of this site or want to report an accessibility problem.</li>
        <li>Need information on this website in a different format. Such as accessible PDF, large print, 
        easy read, audio recording or braille. We’ll consider your request and get back to you as soon as we can.</li></ul>  
        
        <h3>Enforcement</h3>
        
        <p>If you’re not happy with how we respond to your feedback, contact the 
        <a href = 'https://www.equalityadvisoryservice.com/'><u>Equality Advisory and Support Service</u></a>.
        They are an independent advice service. They will advise you on what to do next.</p>
        
        <h2>Compliance</h2>
        
        <p>Scottish Government is committed to making its websites accessible, in accordance with the 
        Public Sector Bodies (Websites and Mobile Applications) (No. 2) 
        <a href = 'http://www.legislation.gov.uk/uksi/2018/952/regulation/4/made'><u>Accessibility 
        Regulations 2018</u></a>.</p>
        
        <p>This accessibility statement applies to the 
        <a href = 'https://scotland.shinyapps.io/population_dashboard/'><u>Population 
        Programme</u></a> data website. </p>
        
        <h3>Compliance status</h3>
        
        <p>This website is partially compliant with the regulations, due to the non-compliances listed below.</p>
        <h3>Non-accessible content</h3>
        
        The content listed below is not-compliant with the regulations:  
        <ul><li>The small charts in the Home page table are not accessible for screen readers.</li>
        <li>Some text and shading does not appear in high contrast mode - mostly tooltips within the table.</li></ul>
        
        <h3>Preparation of statement</h3>
        
        <p>This statement was prepared on <b>15 March 2021</b>.</p>
        
        <p>These web pages were reviewed <b>February 2021</b>.</p>"))
          
  
  
  
}

