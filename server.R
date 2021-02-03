##################################################################
##                            Server                            ##
##################################################################

server <- function(input, output, session) {
  
  
  url <- a("Population Taskforce", href = "https://www.google.com/")
  
  output$subheader <- renderUI({
    tagList(
      "This dashboard supports the",
      url,
      "to understand the demographic challenges in Scotland."
    )
  })
  
#################################################################
##                      Reactive Datasets                      ##
#################################################################

# All other datasets -------------------------------------------

  combined_datasets_reactive <- reactive({

    council1 <- input$council_1
    council2 <- input$council_2

    combined_datasets %>%
      filter(area == "Scotland") %>%
       arrange(period) %>%
      group_by(indicator, variable) %>%
      summarise(
        # Sparkline for Scotland
        "Scotland" = spk_chr(
          values = c(value),
          type = "line",
          xvalues = period,
          fillColor = F,
          lineColor = "#2DA197",
          spotColor = "#92208f",
          minSpotColor = "#92208f",
          maxSpotColor = "#92208f",
          lineWidth = 2,
          spotRadius = 2,
          tooltipFormat = '{{x}}: {{y}}'
        )) %>%
      left_join(
        combined_datasets %>%
          filter(area %in% council1) %>%
           arrange(period) %>%
          group_by(indicator, variable) %>%
          # Sparkline for Council area input 1
          summarise({{council1}} := spk_chr(
            c(value),
            xvalues = period,
            fillColor = F,
            lineColor = "#2DA197",
            spotColor = "#92208f",
            minSpotColor = "#92208f",
            maxSpotColor = "#92208f",
            lineWidth = 2,
            spotRadius = 2,
            tooltipFormat = '{{x}}: {{y}}'
          )
          )
      ) %>%
      left_join(
        combined_datasets %>%
          filter(area %in% council2) %>%
           arrange(period) %>%
          group_by(indicator, variable) %>%
          # Sparkline for Council area input 2
          summarise({{council2}} := spk_chr(
            c(value),
            xvalues = period,
            fillColor = F,
            lineColor = "#2DA197",
            spotColor = "#92208f",
            minSpotColor = "#92208f",
            maxSpotColor = "#92208f",
            lineWidth = 2,
            spotRadius = 2,
            tooltipFormat = '{{x}}: {{y}}'
          ))
      ) %>%
      arrange(match(indicator, Indicator_order)) %>%
      left_join(scotland_arrows) %>%
      relocate(arrow, .after = Scotland) %>% 
      # Hide column names
      rename(" " = indicator,
             "  " = variable,
             "   " = arrow)
})

##################################################################
##                            Tables                            ##
##################################################################
#TODO Combine these into one table   

  output$table1 <- renderDataTable({
    
    cb <- htmlwidgets::JS('function(){debugger;HTMLWidgets.staticRender();}')
    
    dtable <- datatable(combined_datasets_reactive(),
      escape = FALSE,
      class = 'row-border',
      rownames = FALSE,
      options = list(
        rowsGroup = list(0),
        drawCallback =  cb,
        columnDefs = list(
          list(
          className = 'dt-center',
          width = '100',
          targets = c(4, 5)),
          list(
            className = 'dt-left',
            width = '1',
            targets = 3),
          list(
            className = 'dt-right',
            targets = 2)),
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
