##################################################################
##                            Server                            ##
##################################################################

server <- function(input, output) {
  
  url <- a("Population Taskforce", href = "https://www.google.com/")
  
  output$subheader <- renderUI({
    tagList(
      "This dashboard supports the",
      url,
      "to understand the demographic challenges in Scotland"
    )
  })
  
#################################################################
##                      Reactive Datasets                      ##
#################################################################
  
  # This is where I'll generate the datasets that respond to the 
  # council area inputs and relevant filtering which can then be 
  # removed from the table renders below
  
  # population_structure_age <- reactive({
  #   pop_structure %>% 
  #     filter(area == "Scotland",
  #            age != "All") %>% 
  #     group_by(indicator, age)
  # })
  
  # active_dependency_ratio <- reactive({  })
  # healthy_life_expectancy
  # decreasing_pop_council
  # decreasing_pop_datazones
  # mig_net_within_scotland
  # mig_net_rest_of_uk
  # mig_net_overseas
  # mig_net_change
  # components_of_change
  
##################################################################
##                            Tables                            ##
##################################################################
#TODO Combine these into one table   

  output$table1 <- renderUI({
    pop_structure %>%
      filter(area == "Scotland",
             age != "All") %>%
      group_by(indicator, age) %>%
      summarise(Scotland = spk_chr(value,
                                   fillColor = F,
                                   lineWidth = 3
                                   )) %>%
      formattable(align = c("l", "l", "r")) %>%
      formattable::as.htmlwidget() %>%
      spk_add_deps() %>%
      {column(width = 12, .)}
  })
  
  output$table2 <- renderUI({
    adr %>%
      filter(refArea == "S92000003") %>% 
      group_by(indicator) %>% 
      summarise(Scotland = spk_chr(c(value), 
                                   fillColor = F, 
                                   lineWidth = 3)) %>%
      formattable(align = c("l", "r")) %>% 
      formattable::as.htmlwidget() %>%
      spk_add_deps() %>%
      {column(width = 12, .)}
  })
  
  output$table3 <- renderUI({
    healthy_life_expectancy %>%
      filter(area == "Scotland") %>% 
      group_by(indicator, sex) %>% 
      summarise(Scotland = spk_chr(c(value), 
                                   fillColor = F, 
                                   lineWidth = 3)) %>%
      formattable(align = c("l", "l", "r")) %>% 
      formattable::as.htmlwidget() %>%
      spk_add_deps() %>%
      {column(width = 12, .)}
  })
  
  output$table4 <- renderUI({
    migration_datasets %>%
      filter(area == "Scotland") %>% 
      group_by(indicator) %>% 
      summarise(Scotland = spk_chr(c(value), 
                                   fillColor = F, 
                                   lineWidth = 3)) %>%
      formattable(align = c("l", "r")) %>% 
      formattable::as.htmlwidget() %>%
      spk_add_deps() %>%
      {column(width = 12, .)}
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