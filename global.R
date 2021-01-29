#################################################################
##                          Libraries                          ##
#################################################################

library(shiny)
library(dplyr)
library(formattable)
library(sparkline)
library(SPARQL)
library(DT)

#################################################################
##                          Variables                          ##
#################################################################

# Read in static data files

#Healthy life expectancy
# This file has been modified to ad NA's to all the years with 
# missing values for the coucnil areas
file_path_hle <- "data/HLE.xlsx"
# Net Migration within Scotland
file_path_net_with_scot <- "data/migflow-ca-01-latest-tab1.xlsx"
# Net Migration Overseas
file_path_net_overseas <- "data/mig-overseas-admin-sex-tab1.xlsx"
# Net Migration - Rest of the UK
file_path_ruk <- "data/mig-uk-admin-sex-91-latest-tab2.xlsx"
# Components of change
file_path_components_of_change_19 <- "data/mid-year-pop-est-19-data.xlsx"
file_path_components_of_change_18 <- "data/mid-year-pop-est-18-tabs.xlsx"
# Area name lookups
area_name_lookup <- read.csv("data/area_codes.csv")
data_zone_lookup <- read.csv("data/Datazone2011lookup.csv")

# Assign endpoint for SPARQL queries
endpoint <- "https://statistics.gov.scot/sparql"

# Get current year for calculating most recent range of data sets
current_year <- lubridate::year(lubridate::today())

# Generate years in quarters for current range.
# e.g. "2009-Q1", "2009-Q2", "2009-Q3", "2009-Q4"
current_quarter <- as.character(zoo::as.yearqtr(lubridate::today())) %>% 
  stringr::str_sub(start = -2)

year_quarters <- paste0(rep(as.character(c((current_year - 12):(current_year))),
                            each = length(current_quarter)),"-",
                           current_quarter)

Indicator_order <- c("Population Structure",
                     "Active Dependency Ratio",
                     "Healthy Life Expectancy",
                     "pop_change_by_council_area",
                     "pop_change_by_data_zone",
                     "Net Migration - Rest of UK",
                     "Total Net Migration", 
                     "Net Overseas",
                     "Net within Scotland")

source("SPARQL_queries.R")
##################################################################
##                       Reading Raw Data                       ##
##################################################################

##----------------------------------------------------------------
##                     Population Structure                     --
##----------------------------------------------------------------

pop_structure <- opendatascot:::ods_query_database(endpoint, pop_structure_query) %>%
  mutate("  " = "Population Structure",
         sex = "")

# Population Change by Council area -------------------------------

pop_change_by_council_area <- pop_structure %>% 
  # Remove scotland and calculate number of decreased population areas
  filter(age == "All",
         area != "Scotland") %>%
  group_by(area) %>% 
  arrange(period) %>% 
  mutate(change = ifelse(value-lag(value) < 0, 1, 0),
         " " = "Decreased council areas") %>% 
  # remove 2008 - It was needed to calculate 2009)
  filter(period >= current_year-12) %>% 
  group_by(period, ` `) %>% 
  # Sum the number of decreased areas for Scotland's total
  summarise(value = sum(change)) %>% 
  mutate(area = "Scotland") %>% 
  rbind(pop_structure %>% 
          # Remove scotland and calculate number of increased population areas
          filter(age == "All",
                 area != "Scotland") %>%
          group_by(area) %>% 
          arrange(period) %>% 
          mutate(change = ifelse(value-lag(value) < 0, 0, 1),
                 " " = "Increased council areas") %>% 
          filter(period >= current_year-12) %>% 
          group_by(period, ` `) %>% 
          # Sum the number of increased areas for Scotland's total
          summarise(value = sum(change)) %>% 
          mutate(area = "Scotland")) %>% 
  mutate("  " = "Population Change")


## ---------------------------------------------------------------
##                   Active Dependency Ratio                   --
## ---------------------------------------------------------------

# Get economic INACTIVITY
adr <- opendatascot::ods_dataset(
  "economic-inactivity",
  measureType = "count",
  gender = "all",
  refPeriod = year_quarters
) %>%
  group_by(refArea, refPeriod) %>%
  summarise(inactivity = sum(as.numeric(value))) %>%
# Join economic ACTIVITY
  inner_join(opendatascot::ods_dataset(
    "economic-activity",
    measureType = "count",
    gender = "all",
    refPeriod = year_quarters
  ) %>%
    # Remove the "-QX" to make it numeric 
    # mutate(refPeriod = factor(refPeriod,
    #                           levels = year_quarters,
    #                           ordered = T)) %>%
    group_by(refArea, refPeriod) %>%
    summarise(activity = sum(as.numeric(value)))) %>%
  mutate(period = as.numeric(gsub("-.*", "", refPeriod))) %>% 
 # calculate ADR with inactivity/activity multiplied by 1000
  mutate(
    value = round((inactivity / activity) * 1000, digits = 2),
    "  " = "Active Dependency Ratio",
  ) %>% 
  left_join(area_name_lookup, by = c("refArea" = "area_code")) %>% 
  ungroup() %>% 
  select(area, period, value, "  ") %>% 
  mutate(sex = "",
         age = "")


## ---------------------------------------------------------------
##                   Healthy life expectancy                   --
## ---------------------------------------------------------------


hle <- opendatascot:::ods_query_database(endpoint, hle_query) %>%
  mutate("  " = "Healthy Life Expectancy")

# Join static healthy life expectancy data to API datset 
    # Statistics.gov.scot only has > 2015 - 2016
healthy_life_expectancy <- readxl::read_xlsx(file_path_hle) %>% 
  select("area" = Area_name,
         "period" = Period,
         "age" = `Age group`,
         "value" = `Healthy Life Expectancy (HLE) _`,
         "sex" = Sex
         ) %>%
  mutate("  " = "Healthy Life Expectancy",
         value = round(value, digits = 2),
         sex = gsub('s', '', sex)) %>% 
  # Remove any dates that are already in stats.gov.scot dataset
  filter(!(period %in% hle$period)) %>% 
  rbind(hle) %>% 
  mutate(age = "",
         period = as.numeric(gsub("-.*", "", period)))

## ---------------------------------------------------------------
##               Population Change - Data Zones               --
## ---------------------------------------------------------------
# Too large for one call
# Build as list then unlist to speed up?

pop_estimates_datazones <- opendatascot::ods_dataset(
  "population-estimates-2011-datazone-linked-dataset",
  geography = "dz",
  sex = "all",
  age = "all",
  refPeriod = as.character(c((current_year - 13):(current_year - 10)))
) %>%
  rbind(opendatascot::ods_dataset(
    "population-estimates-2011-datazone-linked-dataset",
    geography = "dz",
    sex = "all",
    age = "all",
    refPeriod = as.character(c((current_year - 9):(current_year - 6)))
  )) %>%
  rbind(opendatascot::ods_dataset(
    "population-estimates-2011-datazone-linked-dataset",
    geography = "dz",
    sex = "all",
    age = "all",
    refPeriod = as.character(c((current_year - 5):current_year))
  )) %>%
  mutate("  " = "Population Change") %>%
  select(-measureType) %>%
  rename("zone" = refArea,
         "period" = refPeriod) 


pop_change_by_data_zone <- pop_estimates_datazones %>% 
  group_by(zone) %>% 
  arrange(period) %>% 
  mutate(value = as.numeric(value),
         period = as.numeric(period),
         change = ifelse(value-lag(value) < 0, 1, 0),
         " " = "% Decreased data zones") %>% 
  filter(period != 2008) %>% 
  
  left_join(data_zone_lookup, by = c("zone" = "DZ2011_Code")) %>%  
  rename("area" = LA_Name) %>% 
  group_by(area, period, ` `) %>% 
  summarise(value = sum(change)) %>% 
  
  rbind(pop_estimates_datazones %>% 
          group_by(zone) %>% 
          arrange(period) %>% 
          mutate(value = as.numeric(value),
                 change = ifelse(value-lag(value) < 0, 1, 0),
                 " " = "% Decreased data zones") %>% 
          filter(period != 2008) %>% 
          group_by(period, ` `) %>% 
          summarise(value = sum(change)) %>% 
  mutate(area = "Scotland",
         "period" = as.numeric(period))) %>% 
  
  rbind(pop_estimates_datazones %>% 
          group_by(zone) %>% 
          arrange(period) %>% 
          mutate(value = as.numeric(value),
                 period = as.numeric(period),
                 change = ifelse(value-lag(value) < 0, 0, 1),
                 " " = "% Increased data zones") %>% 
          filter(period != 2008) %>% 
          
          left_join(data_zone_lookup, by = c("zone" = "DZ2011_Code")) %>%  
          rename("area" = LA_Name) %>% 
          group_by(area, period, ` `) %>% 
          summarise(value = sum(change)) %>% 
          
          rbind(pop_estimates_datazones %>% 
                  group_by(zone) %>% 
                  arrange(period) %>% 
                  mutate(value = as.numeric(value),
                         change = ifelse(value-lag(value) < 0, 0, 1),
                         " " = "% Increased data zones") %>% 
                  filter(period != 2008) %>% 
                  group_by(period, ` `) %>% 
                  summarise(value = sum(change)) %>% 
                  mutate(area = "Scotland",
                         "period" = as.numeric(period)))) %>% 
  group_by(area, period,) %>% 
  mutate(value = round((value/sum(value))*100, digits = 2),
         "  " = "Population Change")

## ---------------------------------------------------------------
##                     Net Within Scotland                     --
## ---------------------------------------------------------------

net_within_scotland <- readxl::read_excel(
  file_path_net_with_scot,
  sheet = "TS - Internal Migration",
  range = "B5:T38"
) %>% 
  tidyr::pivot_longer(2:19, names_to = "period", values_to = "value") %>% 
  rename("area" = `...1`) %>% 
mutate(area = gsub("Total Moves within Scotland3", "Scotland", area),
       " " = "Within Scotland",
       "  " = "Net Migration",
       period = as.numeric(gsub("-.*","",period))) %>% 
  filter(period >= 2009)

## ----------------------------------------------------------------
##                      Net rest of the UK                      --
## ----------------------------------------------------------------

# net_ruk_scotland <- opendatascot:::ods_query_database(endpoint, net_ruk_query) %>% 
#   mutate("  " = "Net Migration - Rest of UK")


net_ruk <- readxl::read_xlsx(file_path_ruk,
                                   sheet = "Net-Council-Sex (2001-)",
                                  range = "A5:T38") %>% 
  tidyr::pivot_longer(3:20, names_to = "period", values_to = "value") %>% 
  select("area" = `...2`, 
         period, 
         value) %>% 
  mutate(" " = "Rest of the UK",
         "  " = "Net Migration",
         area = gsub('SCOTLAND', 'Scotland', area),
         period = as.numeric(gsub("-.*","",period)),
  ) %>% 
  filter(period >= 2009)

## ----------------------------------------------------------------
##                         Net Overseas                         --
## ----------------------------------------------------------------

net_overseas <- readxl::read_excel(file_path_net_overseas,
                                   sheet = "Net-Council Area-Sex",
                                   range = c("B5:T38")
) %>% 
  tidyr::pivot_longer(2:19, names_to = "period", values_to = "value") %>% 
  select("area" = `...1`, 
         period, 
         value) %>% 
  mutate(" " = "Overseas",
         "  " = "Net Migration",
         area = gsub('SCOTLAND', 'Scotland', area),
         period = as.numeric(gsub("-.*","",period)),
         ) %>% 
  filter(period >= 2009)



## ----------------------------------------------------------------
##                     Total Net Migration                      --
## ----------------------------------------------------------------


total_net_migration <- opendatascot:::ods_query_database(endpoint, 
                                                         net_migration_query) %>%
  mutate(" " = "Total",
         "  " = "Net Migration") %>% 
  select(-c(sex, age))

## ----------------------------------------------------------------
##                     Components of Change                     --
## ----------------------------------------------------------------
## Natural change + net migration + other changes
## one year - potentially show 2 years

components_of_change <- readxl::read_excel(
  file_path_components_of_change_19,
  range = "A3:K39",
  sheet = "Table 4") %>%
  janitor::clean_names() %>%
  filter(!(is.na(area_code1_2))) %>% 
  select("area" = area_name,
         "other_changes" = other_changes4,
         "net_migration" = estimated_net_civilian_migration3,
         natural_change
         ) %>% 
  mutate(period = 2019) %>% 
  # Join 2018 data
  rbind(readxl::read_excel(
  file_path_components_of_change_18,
  range = "A3:K39",
  sheet = "Table 4") %>%
  janitor::clean_names() %>%
  filter(!(is.na(area_code1))) %>%  
  select("area" = area2,
         "other_changes" = other_changes4,
         "net_migration" = estimated_net_civilian_migration3,
         natural_change
         ) %>% 
  mutate(period = 2018)) %>% 
  mutate("  " = "Components of Change")

##################################################################
##                         Combine Data                         ##
##################################################################
 
combined_datasets <- pop_structure %>% 
  filter(age != "All",
         period >= (current_year - 12)) %>% 
  rbind(adr,
        healthy_life_expectancy) %>% 
  mutate(" " = paste(age, sex)) %>% 
  select(-c(age, sex)) %>%
  rbind(net_ruk,
        total_net_migration,
        net_overseas,
        net_within_scotland,
        pop_change_by_council_area,
        pop_change_by_data_zone)



