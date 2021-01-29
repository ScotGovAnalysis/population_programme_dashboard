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
  
  cb <- htmlwidgets::JS('function(){debugger;HTMLWidgets.staticRender();}')
  
#################################################################
##                      Reactive Datasets                      ##
#################################################################


# All other datasets -------------------------------------------

  combined_datasets_reactive <- reactive({
    
    council1 <- input$council_1
    council2 <- input$council_2
    
  combined_datasets %>%
      filter(area == "Scotland") %>%
      group_by(`  `, ` `) %>%
      summarise("Scotland" = spk_chr(values = c(value),
                                     type = "line",
                                    fillColor = F,
                                    xvalues = period,
                                   lineColor = "#2DA197",
                                   spotColor = "#92208f",
                                   minSpotColor = "#92208f",
                                   maxSpotColor = "#92208f",
                                   lineWidth = 4,
                                   spotRadius = 3,
                                   tooltipFormat = '{{x}}: {{y}}')) %>%
      left_join(combined_datasets %>%
      filter(area %in% council1) %>%
      group_by(`  `, ` `) %>%
      summarise({{council1}} := spk_chr(c(value),
                                        fillColor = F,
                                        xvalues = period,
                                   lineColor = "#2DA197",
                                   spotColor = "#92208f",
                                   minSpotColor = "#92208f",
                                   maxSpotColor = "#92208f",
                                   lineWidth = 4,
                                   spotRadius = 3,
                                   tooltipFormat = '{{x}}: {{y}}'
                                   ))) %>% 
      left_join(combined_datasets %>%
      filter(area %in% council2) %>% 
      group_by(`  `, ` `) %>% 
      summarise({{council2}} := spk_chr(c(value), 
                                        fillColor = F,
                                        xvalues = period,
                                   lineColor = "#2DA197",
                                   spotColor = "#92208f",
                                   minSpotColor = "#92208f",
                                   maxSpotColor = "#92208f",
                                   lineWidth = 4,
                                   spotRadius = 3,
                                   tooltipFormat = '{{x}}: {{y}}'
                               ))) %>% 
    arrange(match(`  `, Indicator_order))
})

  
# Components of Change ----------------------------------------------------

components_of_change_reactive <- reactive({
  
  council1 <- input$council_1
  council2 <- input$council_2
  
    components_of_change %>%
      filter(area == "Scotland") %>% 
      group_by(`  `, period) %>% 
      summarise("Scotland" = spk_chr(c(natural_change, net_migration, other_changes),
                               type = "bar",
                               barColor = "#2DA197",
                               negBarColor = "#92208f",
                               barWidth = 12,
                               tooltipFormat = '{{value}}')) %>%
    left_join(components_of_change %>%
      filter(area %in% council1) %>% 
      group_by(`  `, period) %>% 
      summarise({{council1}} := spk_chr(c(natural_change, net_migration, other_changes),
                               type = "bar",
                               barColor = "#2DA197",
                               negBarColor = "#92208f",
                               barWidth = 12,
                               tooltipFormat = '{{value}}'))) %>%
    left_join(components_of_change %>%
      filter(area %in% council2) %>% 
      group_by(`  `, period) %>% 
      summarise({{council2}} := spk_chr(c(natural_change, net_migration, other_changes),
                               type = "bar",
                               barColor = "#2DA197",
                               negBarColor = "#92208f",
                               barWidth = 12,
                               tooltipFormat = '{{value}}')))
  })
  


##################################################################
##                            Tables                            ##
##################################################################
#TODO Combine these into one table   

  output$table1 <- renderDataTable({
    
    dtable <- datatable(
      combined_datasets_reactive(),
      escape = FALSE,
      class = 'row-border',
      rownames = FALSE,
      options = list(
        rowsGroup = list(0),
        drawCallback =  cb,
        columnDefs = list(list(
          className = 'dt-center',
          width = '125',
          targets = 2:4
        )),
        dom = 'ft',
        lengthChange = FALSE,
        bInfo = FALSE,
        bPaginate = FALSE,
        bSort = FALSE,
        bFilter = FALSE
      ))
    
    # for merging the Indicators 
    # https://stackoverflow.com/questions/39484118/shiny-merge-cells-in-dtdatatable
    dep <- htmltools::htmlDependency(
      "RowsGroup", "2.0.0", 
      "www/", script = "dataTables.rowsGroup.js")
    
    dtable$dependencies <- c(dtable$dependencies, list(dep))
    dtable
    
  })


  output$table2 <- renderDataTable(
    expr = components_of_change_reactive(),
    escape = FALSE,
    class = 'row-border',
    rownames = FALSE,
    options = list(
      drawCallback =  cb,
      columnDefs = list(list(
        className = 'dt-center',
        width = '125',
        targets = "_all"
      )),
      dom = 'ft',
      lengthChange = FALSE,
      bInfo = FALSE,
      bPaginate = FALSE,
      bSort = FALSE,
      bFilter = FALSE
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