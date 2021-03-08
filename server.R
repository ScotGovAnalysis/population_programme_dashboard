##################################################################
##                            Server                            ##
##################################################################

server <- function(input, output, session) {
  
  # 
  # output$about <- about_page 
  # #output$accessibility <- accessibility_page 
  # 
  url <- a(HTML("Population Taskforce"), 
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

    scotland_symbol <- create_LE_symbols_scotland(life_expectancies)

    council1_symbol <- create_LE_symbols_council1(life_expectancies,
                                               council1)

    council2_symbol <- create_LE_symbols_council2(life_expectancies,
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
        rowCallback = JS(tooltips),
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
paste0("<h2>How to read this chart</h2>",
      "<p>These charts give an overall picture. More detailed versions will be added in time.</p>",
      "<p>Most charts go from ", current_year-12, " - ", current_year-2, ". All the charts use the same horizontal scale</p>",
      as.character(icon("arrow-up", lib = "glyphicon")), " Increase from last year", br(),
      as.character(icon("arrow-down", lib = "glyphicon")), " Decrease from last year", br(),
      as.character(icon("minus", lib = "glyphicon")), " No change from last year</p>",
      "The arrows for <b> life expectancy</b> and <b> healthy life expectancy</b> are based on significant change."
       )
    
  })
#################################################################
##                            Plots                            ##
#################################################################
  
  output$pop_structure_plot <- plotly::renderPlotly(plotly::ggplotly(
    migration_datasets %>%
      filter(
        area %in% c("Scotland", "City of Edinburgh", "Glasgow City"),
        variable == "Overseas <i class=\"glyphicon glyphicon-info-sign\"></i>"
      ) %>%
      ggplot(aes(
        x = as.factor(period),
        y = value,
        group = 3,
        colour = area
      )) +
      geom_line()
  ))
  
  
  
  output$about <- renderUI(
    HTML(
      "<h1>About</h1>
      <p>A <a href = 'https://www.gov.scot/groups/population-task-force/'>Population Taskforce</a> 
      and Population Programme were established in June 2019 by the Scottish Government to consider 
      Scotland’s future population challenges.</p>
      
      <p>The <a href = 'https://nationalperformance.gov.scot/'>National Performance Framework</a> 
      (NPF) includes an indicator on Scotland’s population under the National Outcome “we are open, 
      connected and make a positive contribution internationally”. This dashboard includes the NPF 
      indicator as well as a number of other indicators which report on Scotland's changing population
      to capture the multi-dimensional nature of population challenges.</p>
      
      <p>As agreed by the Population Taskforce, the Scottish Government’s aim is to make communities 
      across Scotland attractive places to live, work, bring up families and to move to; so that 
      Scotland’s population profile improves sustainable and inclusive economic growth and wellbeing.</p> 
      
      <h2>Contact</h2>
      
      <p>We welcome your feedback to improve this website:</p>
        
      <ul><li>Email: <a href = 'mailto:population@gov.scot'>population@gov.scot</a></li>
      <li>Phone: 0300 244 4000 (Scottish Government central enquiry unit)</li>
      <li>Scottish Government <a href = 
          'https://www.gov.scot/about/contact-information/'>general enquiries</a></ul></li>  
        
      <h2>Data</h2>
      <h3>Population structure</h3>  
      <p>Proportion of the total population who are children (ages 0 to 15), working age (aged 16 to 64), 
      and pensionable age (aged 65 and over).</p>
      <p>Source: <a href = 
          'https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/mid-year-population-estimates'>
      Mid-year Population Estimates, 
      National Records of Scotland</a></p>
      <p>Linked data: <a href = 
          'https://statistics.gov.scot/resource?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fpopulation-estimates-2011-datazone-linked-dataset'>
      SG open data platform</a></p>
      
      <h3>Active dependency ratio</h3>  
      <p>Number of people aged 16 and over that are economically inactive per 1,000 economically active</p>  
      <p>Sources: 
      <ul><li><a href = 
          'https://statistics.gov.scot/resource?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Feconomic-activity'>
      Economic Activity, Annual Population Survey, Scottish Government</a></li>
      <li><a href = 
          'https://statistics.gov.scot/resource?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Feconomic-inactivity'>
      Economic Inactivity, Annual Population Survey, Scottish Government</a></p></ul></li>

      <h3>Life expectancy</h3>  
      <p>Average number of years a newborn baby could be expected to live. Data are based on calendar years and 
      are published as 3 year rolling averages. Dashboard arrow direction based on significant change.</p>
      <p>Source: <a href = 
          'https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/life-expectancy/life-expectancy-at-scotland-level'>
      Life Expectancy in Scotland, National Records of Scotland</a></p>
      <p>Linked data: <a href = 
          'https://statistics.gov.scot/resource?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2FLife-Expectancy'>
      SG open data platform</a></p>

      <h3>Healthy life expectancy</h3> 
      <p>Average number of years a new born baby could be expected to live in ‘good’ or ‘very good’ health. Data are based on 
      calendar years and are published as 3 year rolling averages. Dashboard arrow direction based on significant change.</p>
      <p>The National Performance Framework includes an indicator on Healthy Life Expectancy.</p>
      <p>Source: <a href = 
          'https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/life-expectancy/healthy-life-expectancy-in-scotland'>
      Healthy Life Expectancy in Scotland, National Records of Scotland</p></a>
      <p>Linked data: <a href = 
          'https://statistics.gov.scot/resource?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fhealthy-life-expectancy'>
      SG open data platform</p></a>

      <h3>Population change by council area</h3>  
      <p>Number of council areas experiencing an increase or decrease in total population.</p>
      <p>The National Performance Framework Population indicator is the number of council areas experiencing population decline. </p>
      <p>Source: <a href = 
          'https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/mid-year-population-estimates'>
      Mid-year Population Estimates, National Records of Scotland</a></p>
      <p>Linked data: <a href = 
          'https://statistics.gov.scot/resource?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fpopulation-estimates-2011-datazone-linked-dataset'>
      SG open data platform</a></p>
      
      <h3>Population change by data zone</h3>
      <p>The percentage of data zones (small areas with a population of approximately 500 to 1,000 residents) 
      within a council area which have experienced an increase or decrease in population.</p>  
      <p>Source: <a href = 
          'https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/2011-based-special-area-population-estimates/small-area-population-estimates'>
      Small Area Population Estimates, National Records of Scotland</a></p>
      <p>Linked data: <a href = 
          'https://statistics.gov.scot/resource?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fpopulation-estimates-2011-datazone-linked-dataset'>
      SG open data platform</a></p>
      
      <h3>Natural Change</h3>
      <p>The number of births minus the number of deaths.</p> 
      <p>Source: <a href = 
          'https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/mid-year-population-estimates'>
      Mid-year Population Estimates, National Records of Scotland</a></p>

      <h3>Net Migration</h3>
      <p>Net migration is inward migration minus outward migration.</p>
      <p>Within Scotland migration refers to migration from other areas in Scotland.</p>
      <p>Migration from the rest of the UK refers to migration from England, Northern Ireland and Wales.</p>
      <p>Migration from overseas refers to migration from areas outside of the UK.</p>
      <p>Total net migration refers to migration from within in Scotland, the rest of the UK and overseas.</p>
      <p>Source: <a href = 
          'https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/migration/migration-statistics/migration-flows'>
      Migration Flows, National Records of Scotland</p>"
    )
  )
  
  
  output$accessibility <- renderUI(
  HTML("<h1>Accessibility</h1>
    
        <p>Last updated: <b>15 March 2021</b></p>
        
        <p>This website is run by the Scottish Government.</p>
        
        <p>We want it to be accessible and usable for as many people as possible.</p>
        
        <h2>Feedback</h2>  
        
        <a href = 'https://scotland.shinyapps.io/population_programme/#Contact'>Contact us</a> if you:  
        
        <ul>
        <li>Cannot access any part of this site or want to report an accessibility problem.</li>
        <li>Need information on this website in a different format. Such as accessible PDF, large print, 
        easy read, audio recording or braille. We’ll consider your request and get back to you as soon as we can.</li>
        </ul>  
        
        <h3>Enforcement</h3>
        
        <p>If you’re not happy with how we respond to your feedback, contact the 
        <a href = 'https://www.equalityadvisoryservice.com/'>Equality Advisory and Support Service</a>.
        They are an independent advice service. They will advise you on what to do next.</p>
        
        <h2>Compliance</h2>
        
        <p>Scottish Government is committed to making its websites accessible, in accordance with the 
        Public Sector Bodies (Websites and Mobile Applications) (No. 2) 
        <a href = 'http://www.legislation.gov.uk/uksi/2018/952/regulation/4/made'>Accessibility 
        Regulations 2018</a>.</p>
        
        <p>This accessibility statement applies to the 
        <a href = 'https://scotland.shinyapps.io/population_dashboard/'>Population 
        Programme</a> data website. </p>
        
        <h3>Compliance status</h3>
        
        <p>This website is partially compliant with the regulations, due to the non-compliances listed below.</p>
        <h3>Non-accessible content</h3>
        
        The content listed below is not-compliant with the regulations:  
        <ul>
        <li>The small charts in the Home page table are not accessible for screen readers.</li>
        <li>Some text and shading does not appear in high contrast mode - mostly tooltips within the table.</li>
        <li>The icons do not currently have alternative text.</li>
        <li>The tooltips are not accessible for screen readers (these are detailed in the about page).</li>
        </ul>
        
        <h3>Preparation of statement</h3>
        
        <p>This statement was prepared on <b>15 March 2021</b>.</p>
        
        <p>These web pages were reviewed <b>February 2021</b>.</p>"))
        
}
