

significant_change_calulator <- function(estimate,
                                         confidence_interval,
                                         limit) {
  if (limit == "upper") {
    (estimate - lag(estimate)) + 1.96 * sqrt((confidence_interval / 1.96) ^ 2 + (lag(confidence_interval) / 1.96) ^ 2)
  } else {
    (estimate - lag(estimate)) - 1.96 * sqrt((confidence_interval / 1.96) ^ 2 + (lag(confidence_interval) / 1.96) ^ 2)
  }
}






# TODO 6 different conditions for 3 icons 

# Both increase = Improve
# 1 increase 1 maintain = Improve

# Neither change = maintain
# 1 increase & 1 worsen = maintain

# Both decrease = Worsen
# 1 decrease & 1 maintain = Worsen


#################################################################
##                      Sparkline Formats                      ##
#################################################################
smile <- "<span style='font-size:100px;'>&#9785;</span>"
# Format for LINE sparklines -----------------------------------------------
sparkline_format <- function(type, y, x) {
  
  if(type == "line"){
    spk_chr(
      values = y,
      xvalues = x,
      type = "line",
      numberDigitGroupSep = "",
      # Remove Comma from tooltip
      fillColor = F,
      # Remove shaded area under the line
      lineColor = "#0065bd",
      spotColor = F, #"#0065bd",
      highlightSpotColor = "#fdd522",
      minSpotColor = F,
      maxSpotColor = F,
      lineWidth = 2,
      spotRadius = 3,
      tooltipFormat = '{{x}}: {{y}}')
    
  } else {
    
    spk_chr(
      values = y,
      type = "bar",
      numberDigitGroupSep = "",
      # Remove comma from tooltip
      barColor = "#0065bd",
      negBarColor = "red",
      barWidth = 5,
      tooltipFormat = '* {{value}}'
    )
  }
}


##################################################################
##                   Create indicator symbols                   ##
##################################################################

# Symbols for Scotland ---------------------------------------------------

create_symbols_scotland <- function(data, symbol_up, symbol_down){
  data %>%
    filter(area == "Scotland") %>%
    group_by(indicator, variable) %>%
    arrange(desc(period)) %>%
    filter(!is.na(value)) %>% 
    slice_max(period, n = 2) %>%
    mutate(change = ifelse(value - lag(value) > 0, 1, 
                           ifelse(value - lag(value) < 0, 2, 0))) %>%
    filter(!is.na(change)) %>%
    mutate(icon = ifelse(change == 1,
      as.character(icon(symbol_down, lib = "glyphicon")),
      ifelse(change == 2, 
             as.character(icon(symbol_up,lib = "glyphicon")),
             as.character(icon("minus", lib = "glyphicon"))))) %>%
    ungroup() %>%
    select(variable, icon)
}

# Symbols for council 1 ---------------------------------------------------

create_symbols_council1 <- function(data, council, symbol_up, symbol_down){
  
  data %>%
    filter(area %in% council) %>%
    group_by(indicator, variable) %>%
    arrange(desc(period)) %>%
    filter(!is.na(value)) %>%
    slice_max(period, n = 2) %>%
    mutate(change = ifelse(value - lag(value) > 0, 1,
                           ifelse(value - lag(value) < 0, 2, 0))) %>%
    filter(!is.na(change)) %>%
    mutate(icon1 = ifelse(change == 1,
      as.character(icon(symbol_down,lib = "glyphicon")),
      ifelse(change == 2,
             as.character(icon(symbol_up,lib = "glyphicon")),
             as.character(icon("minus",lib = "glyphicon"))))) %>%
    ungroup() %>%
    select(variable, icon1)
}


# Symbols for council 2 ---------------------------------------------------

create_symbols_council2 <- function(data, council, symbol_up, symbol_down){

  data %>%
    filter(area %in% council) %>%
    group_by(indicator, variable) %>%
    arrange(desc(period)) %>%
    filter(!is.na(value)) %>%   
    slice_max(period, n = 2) %>%
    mutate(change = ifelse(value - lag(value) > 0, 1, 
                           ifelse(value - lag(value) < 0, 2, 0))) %>%
    filter(!is.na(change)) %>%
    mutate(icon2 = ifelse(
      change == 1,
      as.character(icon(symbol_down, lib = "glyphicon")),
      ifelse(change == 2, as.character(icon(symbol_up, lib = "glyphicon")),
             as.character(icon("minus", lib = "glyphicon"))))) %>%
    ungroup() %>%
    select(variable, icon2)
}



##################################################################
##       Create data with sparklines and join the symbols       ##
##################################################################

combine_columns_and_symbols <- function(data, x, y, a, b, c, type){
  # Take data and create sparkline for just Scotland
  data %>%
  filter(area == "Scotland") %>%
  arrange(period) %>%
  group_by(indicator, variable) %>%
  summarise(
    # Sparkline for Scotland
    "Scotland" = sparkline_format(type, value, period)) %>%
  # Create and join sparkline for just user input 1
  left_join(
    data %>%
      filter(area %in% x) %>%
      arrange(period) %>%
      group_by(indicator, variable) %>%
      # Sparkline for Council area input 1 (y)
      summarise({{x}} := sparkline_format(type, value, period))) %>%
  # Create and join sparkline for just user input 2
  left_join(
    data %>%
      filter(area %in% y) %>%
      arrange(period) %>%
      group_by(indicator, variable) %>%
      # Sparkline for Council area input 2 (y)
      summarise({{y}} := sparkline_format(type, value, period))) %>%
  arrange(match(variable, variable_order)) %>%
  # Join all the symbol columns
  left_join(a) %>%
  relocate(icon, .after = Scotland) %>% 
  left_join(b) %>%
  relocate(icon1, .after = 5) %>% 
  left_join(c) %>%
  relocate(icon2, .after = 7)
}

# For within scotland
combine_columns_and_symbols_within_scot <- function(data, x, y, a, b, c, type){
  # Take data and create sparkline for just Scotland
  data %>%
  filter(area == "Scotland") %>%
  arrange(period) %>%
  group_by(indicator, variable) %>%
  summarise(
    # Sparkline for Scotland
    "Scotland" = HTML("")) %>%
  # Create and join sparkline for just user input 1
  left_join(
    data %>%
      filter(area %in% x) %>%
      arrange(period) %>%
      group_by(indicator, variable) %>%
      # Sparkline for Council area input 1 (y)
      summarise({{x}} := sparkline_format(type, value, period))) %>%
  # Create and join sparkline for just user input 2
  left_join(
    data %>%
      filter(area %in% y) %>%
      arrange(period) %>%
      group_by(indicator, variable) %>%
      # Sparkline for Council area input 2 (y)
      summarise({{y}} := sparkline_format(type, value, period))) %>%
  arrange(match(variable, variable_order)) %>%
  # Join all the symbol columns
  left_join(a) %>%
  relocate(icon, .after = Scotland) %>% 
  left_join(b) %>%
  relocate(icon1, .after = 5) %>% 
  left_join(c) %>%
  relocate(icon2, .after = 7)
}

# Tooltips for the table
rcb <- c(
  "function(row, data, num, index){",
  # Row 0 
        "   if(index === 0){",
  "    $('td:eq(0)', row).attr('title', 'Proportion of children, people aged 16-64 and people aged 65 and over.');",
  # Row 3
  "  }else if(index === 3){",
  "    $('td:eq(0)', row).attr('title', 'Number of people aged 16 and over that are economically inactive per 1,000 economically active.');",
  # Row 4
  "  }else if(index === 4){",
  "    $('td:eq(0)', row).attr('title', 'Average number of years a new born baby could be expected to live in ‘good’ or ‘very good’ health. Figures based on 3-year ranges.');",
  # Row 6
  "  }else if(index === 6){",
  "    $('td:eq(0)', row).attr('title', 'Average number of years a new born baby could be expected to live. Figures showing mid-year based on 3-year ranges.');",
  # Row 6
  "  }else if(index === 8){",
  "    $('td:eq(0)', row).attr('title', 'Population Change');",
  "    $('td:eq(1)', row).attr('title', 'The proportion of datazones experiencing population increase');",
  # Row 8
  "  }else if(index === 9){",
  "    $('td:eq(1)', row).attr('title', 'The proportion of datazones experiencing population decline.');",
  # Row 9
  "  }else if(index === 10){",
  "    $('td:eq(1)', row).attr('title', 'The number of councils experiencing population increase.');",
  # Row 10
  "  }else if(index === 11){",
  "    $('td:eq(1)', row).attr('title', 'The number of councils experiencing population decline.');",
  # Row 11
  "  }else if(index === 12){",
  "    $('td:eq(1)', row).attr('title', 'Number of births minus deaths');",
  # Row 7
  "  }else if(index === 13){",
  "    $('td:eq(0)', row).attr('title', 'Inward minus outward migration');",
  "    $('td:eq(1)', row).attr('title', 'Net Migration from other areas within Scotland');",
  # Row 12
  "  }else if(index === 14){",
  "    $('td:eq(1)', row).attr('title', 'Net migration from the rest of the UK');",
  # Row 13
  "  }else if(index === 15){",
  "    $('td:eq(1)', row).attr('title', 'Net migration from outside the UK');",
  # Row 14
  "  }else if(index === 16){",
  "    $('td:eq(1)', row).attr('title', 'Net migration from other areas within Scotland and areas outwith Scotland');",
  "  }}")

