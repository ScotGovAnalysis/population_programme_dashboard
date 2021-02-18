##################################################################
##                            Server                            ##
##################################################################

server <- function(input, output, session) {
  
  url <- a("Population Taskforce", 
           href = "https://www.gov.scot/groups/population-task-force/", 
           target="_blank")
  
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

# Population Structure ----------------------------------------------------

  pop_structure_age_reactive <- reactive({
    
    council1 <- input$council_1
    council2 <- input$council_2
    
    scotland_symbol <- create_symbols_scotland(pop_structure_age,
                                               "arrow-up",
                                               "arrow-down")
    
    council1_symbol <- create_symbols_council1(pop_structure_age,
                                               council1,
                                               "arrow-up",
                                               "arrow-down")
    
    council2_symbol <- create_symbols_council2(pop_structure_age,
                                               council2,
                                               "arrow-up",
                                               "arrow-down")
    
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
    
    scotland_symbol <- create_symbols_scotland(active_dependency_ratio,
                                               "thumbs-down",
                                               "thumbs-up")
    
    council1_symbol <- create_symbols_council1(active_dependency_ratio,
                                               council1,
                                               "thumbs-down",
                                               "thumbs-up")
    
    council2_symbol <- create_symbols_council2(active_dependency_ratio,
                                               council2,
                                               "thumbs-down",
                                               "thumbs-up")
    
    combine_columns_and_symbols(active_dependency_ratio, 
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
    
    scotland_symbol <- create_symbols_scotland(migration_datasets,
                                               "thumbs-up",
                                               "thumbs-down")
    
    council1_symbol <- create_symbols_council1(migration_datasets,
                                               council1,
                                               "thumbs-up",
                                               "thumbs-down")
    
    council2_symbol <- create_symbols_council2(migration_datasets,
                                               council2,
                                               "thumbs-up",
                                               "thumbs-down")
    
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
    
    scotland_symbol <- create_symbols_scotland(natural_change,
                                               "thumbs-up",
                                               "thumbs-down")
    
    council1_symbol <- create_symbols_council1(natural_change,
                                               council1,
                                               "thumbs-up",
                                               "thumbs-down")
    
    council2_symbol <- create_symbols_council2(natural_change,
                                               council2,
                                               "thumbs-up",
                                               "thumbs-down")
    
    combine_columns_and_symbols(natural_change, 
                                council1, 
                                council2, 
                                scotland_symbol, 
                                council1_symbol, 
                                council2_symbol,
                                "bar")
  })
  
  # All other datasets -------------------------------------------
  
  combined_datasets_reactive <- reactive({
    
    council1 <- input$council_1
    council2 <- input$council_2
    
    scotland_symbol <- create_symbols_scotland(combined_datasets,
                                               "thumbs-up",
                                               "thumbs-down")
    
    council1_symbol <- create_symbols_council1(combined_datasets,
                                               council1,
                                               "thumbs-up",
                                               "thumbs-down")
    
    council2_symbol <- create_symbols_council2(combined_datasets,
                                               council2,
                                               "thumbs-up",
                                               "thumbs-down")
    
    combine_columns_and_symbols(combined_datasets, 
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
            combined_datasets_reactive(),
            natural_change_reactive(),
            migration_reactive())
    
    colnames(data)[c(1,2, 4, 6, 8)] <- paste0('<span style="color:',"white",'">',colnames(data)[c(1,2, 4, 6, 8)],'</span>')
    
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
       as.character(icon("thumbs-up", lib = "glyphicon")), "Improving,",
       as.character(icon("thumbs-down", lib = "glyphicon")), "Worsening,",
       as.character(icon("minus", lib = "glyphicon")), "Maintaining", br(),
       "<b>*Data range is 2009-2019\b"
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

##################################################################
##                           Criteria                           ##
##################################################################
  
  output$pop_size <- renderUI(
    HTML(
      "Population aged under 16, population aged 16 to 64 and population
      aged 65 and over:
    <ul><li>Population increasing if proportion of the population has
    increased.</li>
    <li>Population maintaining if proportion of the population has not
    changed.</li>
    <li>Population decreasing if proportion of the population has
      decreased.</ul>"
    )
  )
  
  output$healthy_life_expectancy <- renderUI(
    HTML(
      "Average number of years a new born baby could be expected to live in
      ‘good’ or ‘very good’ health:
    <ul><li>Performance improving if both male and female HLE has significantly
increased or if HLE of one sex has significantly increased and there hasn’t
been a significant change in the other.</li>
    <li>Performance maintaining if male and female HLE has significantly changed
in opposite directions or if there hasn’t been a significant change for males
and females.</li>
    <li>Performance worsening if both male and female HLE has significantly
decreased or if HLE of one sex has significantly decreased and there
hasn’t been a significant change in the other.</ul>"
    )
  )
  
  output$active_dependency_ratio <- renderUI(
    HTML(
      "Number of economically inactive people (aged 16 and over) per 1,000
economically active people (aged 16 and over):
    <ul><li>Performance improving if ratio has increased.</li>
    <li>Performance maintaining if ratio has not changed.</li>
    <li>Performance worsening if ratio has decreased.</ul>"
    )
  )
  
  output$pop_change_by_council <- renderUI(
    HTML(
      "Number of councils experiencing population decline:
    <ul><li>Performance improving if number of councils has decreased.</li>
    <li>Performance maintaining if number of councils has not changed.</li>
    <li>Performance worsening if number of councils has increased.</ul>"
    )
  )
  
#################################################################
##                          About Tab                          ##
#################################################################
  
  
  output$pop_change_within_council <- renderUI(
    HTML(
      "Percentage of data zones (small areas) within each council experiencing
population decline:
    <ul><li>Performance improving if percentage of data zones has
    decreased.</li>
    <li>Performance maintaining if percentage of data zones has not
    changed.</li>
    <li>Performance worsening if percentage of data zones has increased.</ul>"
    )
  )
  
  output$about <- renderUI(
    HTML(
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit,
            sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
            Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris
            nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in
            reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla
            pariatur. Excepteur sint occaecat cupidatat non proident, sunt in
            culpa qui officia deserunt mollit anim id est laborum."
    )
  )
  
}
