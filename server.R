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
    
    scotland_arrows <- create_symbols_scotland(pop_structure_age,
                                               "arrow-up",
                                               "arrow-down")
    
    council1_arrows <- create_symbols_council1(pop_structure_age,
                                               council1,
                                               "arrow-up",
                                               "arrow-down")
    
    council2_arrows <- create_symbols_council2(pop_structure_age,
                                               council2,
                                               "arrow-up",
                                               "arrow-down")
    
    pop_structure_age %>%
      filter(area == "Scotland") %>%
      arrange(period) %>%
      group_by(indicator, variable) %>%
      summarise(
        # Sparkline for Scotland
        "Scotland" = sparkline_format(value, 
                                      period)
      ) %>%
      left_join(
        pop_structure_age %>%
          filter(area %in% council1) %>%
          arrange(period) %>%
          group_by(indicator, variable) %>%
          # Sparkline for Council area input 1
          summarise({{council1}} := sparkline_format(value, 
                                                     period))) %>%
      left_join(
        pop_structure_age %>%
          filter(area %in% council2) %>%
          arrange(period) %>%
          group_by(indicator, variable) %>%
          # Sparkline for Council area input 2
          summarise({{council2}} := sparkline_format(value, 
                                                     period))
      ) %>%
      arrange(match(variable, variable_order)) %>%
      left_join(scotland_arrows) %>%
      relocate(arrow, .after = Scotland) %>% 
      left_join(council1_arrows) %>%
      relocate(arrow1, .after = 5) %>% 
      left_join(council2_arrows) %>%
      relocate(arrow2, .after = 7) %>% 
      # Hide column names
      rename("     " = indicator,
             "    " = variable,
             "   " = arrow,
             "  " = arrow1,
             " " = arrow2)
  })

# Migration ---------------------------------------------------------------
  migration_reactive <- reactive({
    
    council1 <- input$council_1
    council2 <- input$council_2
    
    scotland_arrows <- create_symbols_scotland(migration_datasets,
                                               "thumbs-down",
                                               "thumbs-up")
    
    council1_arrows <- create_symbols_council1(migration_datasets,
                                               council1,
                                               "thumbs-up",
                                               "thumbs-down")
    
    council2_arrows <- create_symbols_council2(migration_datasets,
                                               council2,
                                               "thumbs-up",
                                               "thumbs-down")
    
    migration_datasets %>%
      filter(area == "Scotland") %>%
      arrange(period) %>%
      group_by(indicator, variable) %>%
      summarise(
        # Sparkline for Scotland
        "Scotland" = sparkbar_format(value, 
                                      period)
      ) %>%
      left_join(
        migration_datasets %>%
          filter(area %in% council1) %>%
          arrange(period) %>%
          group_by(indicator, variable) %>%
          # Sparkline for Council area input 1
          summarise({{council1}} := sparkbar_format(value, 
                                                     period))) %>%
      left_join(
        migration_datasets %>%
          filter(area %in% council2) %>%
          arrange(period) %>%
          group_by(indicator, variable) %>%
          # Sparkline for Council area input 2
          summarise({{council2}} := sparkbar_format(value, 
                                                     period))
      ) %>%
      arrange(match(variable, variable_order)) %>%
      left_join(scotland_arrows) %>%
      relocate(arrow, .after = Scotland) %>% 
      left_join(council1_arrows) %>%
      relocate(arrow1, .after = 5) %>% 
      left_join(council2_arrows) %>%
      relocate(arrow2, .after = 7) %>% 
      # Hide column names
      rename("     " = indicator,
             "    " = variable,
             "   " = arrow,
             "  " = arrow1,
             " " = arrow2)
  })

  

# Healthy Life Expectancy -------------------------------------------------



  healthy_life_expectancy_reactive <- reactive({

    council1 <- input$council_1
    council2 <- input$council_2

    scotland_arrows <- healthy_life_expectancy %>%
      filter(area == "Scotland") %>%
      group_by(indicator, variable) %>%
      arrange(desc(period)) %>%
      filter(!is.na(value)) %>% 
      slice_max(period, n = 2) %>%
      mutate(change = ifelse(value - lag(value) > 0, 1, ifelse(value - lag(value) < 0, 2, 0))) %>%
      filter(!is.na(change)) %>%
      mutate(arrow = ifelse(
        change == 1,
        as.character(icon("thumbs-down", 
                          lib = "glyphicon")),
        ifelse(change == 2, 
               as.character(icon("thumbs-up", 
                                 lib = "glyphicon")),
               as.character(icon("minus", 
                                 lib = "glyphicon"))))) %>%
      ungroup() %>%
      select(variable, arrow)
    
    council1_arrows <- healthy_life_expectancy %>%
      filter(area %in% council1) %>%
      group_by(indicator, variable) %>%
      arrange(desc(period)) %>%
      filter(!is.na(value)) %>% 
      slice_max(period, n = 2) %>%
      mutate(change = ifelse(value-lag(value) > 0, 1, 
                             ifelse(value-lag(value) < 0, 2, 0))) %>%
      filter(!is.na(change)) %>%
      mutate(arrow1 = ifelse(change == 1, 
                             as.character(icon("thumbs-down", 
                                               lib = "glyphicon")),
                            ifelse(change == 2, 
                                   as.character(icon("thumbs-up", 
                                                     lib = "glyphicon")), 
                                   as.character(icon("minus", 
                                                     lib = "glyphicon"))))) %>%
      ungroup() %>%
      select(variable, arrow1)
    
    council2_arrows <- healthy_life_expectancy %>%
      filter(area %in% council2) %>%
      group_by(indicator, variable) %>%
      arrange(desc(period)) %>%
      filter(!is.na(value)) %>% 
      slice_max(period, n = 2) %>%
      mutate(change = ifelse(value-lag(value) > 0, 1, ifelse(value-lag(value) < 0, 2, 0))) %>%
      filter(!is.na(change)) %>%
      mutate(arrow2 = ifelse(change == 1, as.character(icon("thumbs-down", lib = "glyphicon")),
                            ifelse(change == 2, as.character(icon("thumbs-up", lib = "glyphicon")), 
                                   as.character(icon("minus", lib = "glyphicon"))))) %>%
      ungroup() %>%
      select(variable, arrow2)
    
    healthy_life_expectancy %>%
      filter(area == "Scotland") %>%
       arrange(period) %>%
      group_by(indicator, variable) %>%
      summarise(
        # Sparkline for Scotland
        "Scotland" = sparkline_format(value, period)
        ) %>%
      left_join(
        healthy_life_expectancy %>%
          filter(area %in% council1) %>%
           arrange(period) %>%
          group_by(indicator, variable) %>%
          # Sparkline for Council area input 1
          summarise({{council1}} := sparkline_format(value, 
                                                     period))) %>%
      left_join(
        healthy_life_expectancy %>%
          filter(area %in% council2) %>%
           arrange(period) %>%
          group_by(indicator, variable) %>%
          # Sparkline for Council area input 2
          summarise({{council2}} := sparkline_format(value, period))
      ) %>%
      arrange(match(variable, variable_order)) %>%
      left_join(scotland_arrows) %>%
      relocate(arrow, .after = Scotland) %>% 
      left_join(council1_arrows) %>%
      relocate(arrow1, .after = 5) %>% 
      left_join(council2_arrows) %>%
      relocate(arrow2, .after = 7) %>% 
      # Hide column names
      rename("     " = indicator,
             "    " = variable,
             "   " = arrow,
             "  " = arrow1,
             " " = arrow2)
})
  
  # All other datasets -------------------------------------------
  
  combined_datasets_reactive <- reactive({
    
    council1 <- input$council_1
    council2 <- input$council_2
    
    scotland_arrows <- create_symbols_scotland(combined_datasets,
                                               "thumbs-up",
                                               "thumbs-down")
    
    council1_arrows <- create_symbols_council1(combined_datasets,
                                               council1,
                                               "thumbs-up",
                                               "thumbs-down")
    
    council2_arrows <- create_symbols_council2(combined_datasets,
                                               council2,
                                               "thumbs-up",
                                               "thumbs-down")
    
    combined_datasets %>%
      filter(area == "Scotland") %>%
      arrange(period) %>%
      group_by(indicator, variable) %>%
      summarise(
        # Sparkline for Scotland
        "Scotland" = sparkline_format(value, period)
      ) %>%
      left_join(
        combined_datasets %>%
          filter(area %in% council1) %>%
          arrange(period) %>%
          group_by(indicator, variable) %>%
          # Sparkline for Council area input 1
          summarise({{council1}} := sparkline_format(value, 
                                                     period))) %>%
      left_join(
        combined_datasets %>%
          filter(area %in% council2) %>%
          arrange(period) %>%
          group_by(indicator, variable) %>%
          # Sparkline for Council area input 2
          summarise({{council2}} := sparkline_format(value, period))
      ) %>%
      arrange(match(variable, variable_order)) %>%
      left_join(scotland_arrows) %>%
      relocate(arrow, .after = Scotland) %>% 
      left_join(council1_arrows) %>%
      relocate(arrow1, .after = 5) %>% 
      left_join(council2_arrows) %>%
      relocate(arrow2, .after = 7) %>% 
      # Hide column names
      rename("     " = indicator,
             "    " = variable,
             "   " = arrow,
             "  " = arrow1,
             " " = arrow2)
  })

##################################################################
##                            Tables                            ##
##################################################################
#TODO Combine these into one table   

  output$table1 <- renderDataTable({
    
    cb <- htmlwidgets::JS('function(){debugger;HTMLWidgets.staticRender();}')
    
    dtable <- datatable(pop_structure_age_reactive() %>% 
                          rbind(healthy_life_expectancy_reactive(),
                                combined_datasets_reactive(),
                                migration_reactive(),
                                ),
      escape = FALSE,
      class = 'row-border',
      rownames = FALSE,
      options = list(
        rowsGroup = list(0),
        drawCallback =  cb,
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
