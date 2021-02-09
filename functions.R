# Format the sparklines
sparkline_format <- function(x, y) {spk_chr(
  values = x,
  type = "line",
  numberDigitGroupSep = "",
  xvalues = y,
  fillColor = F,
  lineColor = "#0065bd",
  spotColor = "#0065bd",
  highlightSpotColor = "#fdd522",
  minSpotColor = "#0065bd",
  maxSpotColor = "#0065bd",
  lineWidth = 2,
  spotRadius = 3,
  tooltipFormat = '{{x}}: {{y}}'
)}

sparkbar_format <- function(value, period) {spk_chr(
  values = value,
  type = "bar",
  numberDigitGroupSep = "",
  barColor = "#0065bd",
  negBarColor = "red",
  barWidth = 5
)}

sparkline_format_below_0 <- function(x, y) {spk_chr(
  values = x,
  type = "line",
  numberDigitGroupSep = "",
  xvalues = y,
  #fillColor = F,
  zeroAxis = T,
  lineColor = "#0065bd",
  spotColor = "#0065bd",
  highlightSpotColor = "#fdd522",
  minSpotColor = "#0065bd",
  maxSpotColor = "#0065bd",
  lineWidth = 2,
  spotRadius = 3,
  tooltipFormat = '{{x}}: {{y}}'
)}



create_symbols_scotland <- function(data, symbol_up, symbol_down){
  data %>%
    filter(area == "Scotland") %>%
    group_by(indicator, variable) %>%
    arrange(desc(period)) %>%
    filter(!is.na(value)) %>% 
    slice_max(period, n = 2) %>%
    mutate(change = ifelse(value - lag(value) > 0, 1, ifelse(value - lag(value) < 0, 2, 0))) %>%
    filter(!is.na(change)) %>%
    mutate(arrow = ifelse(
      change == 1,
      as.character(icon(symbol_down, 
                        lib = "glyphicon")),
      ifelse(change == 2, 
             as.character(icon(symbol_up, 
                               lib = "glyphicon")),
             as.character(icon("minus", 
                               lib = "glyphicon"))))) %>%
    ungroup() %>%
    select(variable, arrow)
}

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
    mutate(arrow1 = ifelse(
      change == 1,
      as.character(icon(symbol_down,
                        lib = "glyphicon")),
      ifelse(change == 2,
             as.character(icon(symbol_up,
                               lib = "glyphicon")),
             as.character(icon("minus",
                               lib = "glyphicon")))
    )) %>%
    ungroup() %>%
    select(variable, arrow1)
}

create_symbols_council2 <- function(data, council, symbol_up, symbol_down){

  
  data %>%
    filter(area %in% council) %>%
    group_by(indicator, variable) %>%
    arrange(desc(period)) %>%
    filter(!is.na(value)) %>% 
    slice_max(period, n = 2) %>%
    mutate(change = ifelse(value - lag(value) > 0, 1, ifelse(value - lag(value) < 0, 2, 0))) %>%
    filter(!is.na(change)) %>%
    mutate(arrow2 = ifelse(
      change == 1,
      as.character(icon(symbol_down, 
                        lib = "glyphicon")),
      ifelse(change == 2, 
             as.character(icon(symbol_up, 
                               lib = "glyphicon")),
             as.character(icon("minus", 
                               lib = "glyphicon"))))) %>%
    ungroup() %>%
    select(variable, arrow2)
}
