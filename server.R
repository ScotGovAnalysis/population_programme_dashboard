##################################################################
##                            Server                            ##
##################################################################

server <- function(input, output, session) {
  
  
  output$about <- about_page 
  #output$accessibility <- accessibility_page 
  
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
  
  
}

