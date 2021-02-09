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
# This file has been modified to add NA's to all the years with 
# missing values for the council areas
file_path_hle <- "data/HLE.xlsx"
# Net Migration within Scotland
file_path_net_with_scot <- "data/migflow-ca-01-latest-tab1.xlsx"
# Net Migration Overseas
file_path_net_overseas <- "data/mig-overseas-admin-sex-tab1.xlsx"
# Net Migration - Rest of the UK
file_path_ruk <- "data/mig-uk-admin-sex-91-latest-tab2.xlsx"
# Components of change
file_path_natural_change <- "data/Natural change - 2009-2019.xlsx"
# Area name lookups
area_name_lookup <- read.csv("data/area_codes.csv")
council_areas <- area_name_lookup %>% filter(area != "Scotland")
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

indicator_order <- c("Population Structure",
                     "Active Dependency Ratio",
                     "Healthy Life Expectancy",
                     "Population Change",
                     "Net Migration")

variable_order <- c("Children (under 16 years)",
                    "Working Age (16 - 64)",
                    "Pensionable Age (65 and over)",
                    "",
                    "Female",
                    "Male",
                    "Natural Change",
                    "% Increased data zones",
                    "% Decreased data zones",
                    "Increased council areas",
                    "Decreased council areas",
                    "Within Scotland",
                    "Rest of the UK",
                    "Overseas",
                    "Total")

source("SPARQL_queries.R")
source("functions.R")
##################################################################
##                       Reading Raw Data                       ##
##################################################################

##----------------------------------------------------------------
##                     Population Structure                     --
##----------------------------------------------------------------

pop_structure <- opendatascot:::ods_query_database(endpoint, pop_structure_query) %>%
  mutate("indicator" = "Population Structure",
         sex = "")

# Population Change by Council area -------------------------------

pop_change_by_council_area <- pop_structure %>% 
  # Remove scotland and calculate number of decreased population areas
  filter(age == "All",
         area != "Scotland") %>%
  group_by(area) %>% 
  arrange(period) %>% 
  mutate(change = ifelse(value-lag(value) < 0, 1, 0),
         "variable" = "Decreased council areas") %>% 
  # remove 2008 - It was needed to calculate 2009)
  filter(period >= current_year-12) %>% 
  group_by(period, `variable`) %>% 
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
                 "variable" = "Increased council areas") %>% 
          filter(period >= current_year-12) %>% 
          group_by(period, `variable`) %>% 
          # Sum the number of increased areas for Scotland's total
          summarise(value = sum(change)) %>% 
          mutate(area = "Scotland")) %>% 
  mutate("indicator" = "Population Change")


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
    group_by(refArea, refPeriod) %>%
    summarise(activity = sum(as.numeric(value)))) %>%
  # Remove the "-QX" to make it numeric 
  mutate(period = as.numeric(gsub("-.*", "", refPeriod)),
         # calculate ADR with inactivity/activity multiplied by 1000
         value = round((inactivity / activity) * 1000, digits = 2),
         "indicator" = "Active Dependency Ratio") %>% 
  left_join(area_name_lookup, by = c("refArea" = "area_code")) %>% 
  ungroup() %>% 
  select(area, period, value, indicator) %>% 
  mutate(sex = "",
         age = "")


## ---------------------------------------------------------------
##                   Healthy life expectancy                   --
## ---------------------------------------------------------------

# hle <- opendatascot:::ods_query_database(endpoint, hle_query) %>%
#   mutate("  " = "Healthy Life Expectancy",
#          period = as.numeric(gsub("-.*", "", period)))

# Join static healthy life expectancy data to API datset 
    # Statistics.gov.scot only has > 2015 - 2016
healthy_life_expectancy <- readxl::read_xlsx(file_path_hle) %>% 
  select("area" = Area_name,
         "period" = Period,
         "age" = `Age group`,
         "value" = `Healthy Life Expectancy (HLE) _`,
         "sex" = Sex
         ) %>%
  mutate("indicator" = "Healthy Life Expectancy",
         value = round(value, digits = 2),
         sex = gsub('s', '', sex),
         age = "",
         period = as.numeric(gsub("-.*", "", period)))
#%>% 
  # Remove any dates that are already in stats.gov.scot dataset
 # filter(!(period %in% hle$period)) %>% 
 # rbind(hle) %>% 
  

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
  mutate("indicator" = "Population Change") %>%
  select(-measureType) %>%
  rename("zone" = refArea,
         "period" = refPeriod) 


pop_change_by_data_zone <- pop_estimates_datazones %>% 
  group_by(zone) %>% 
  arrange(period) %>% 
  mutate(value = as.numeric(value),
         period = as.numeric(period),
         change = ifelse(value-lag(value) < 0, 1, 0),
         "variable" = "% Decreased data zones") %>% 
  filter(period != 2008) %>% 
  
  left_join(data_zone_lookup, by = c("zone" = "DZ2011_Code")) %>%  
  rename("area" = LA_Name) %>% 
  group_by(area, period, variable) %>% 
  summarise(value = sum(change)) %>% 
  
  rbind(pop_estimates_datazones %>% 
          group_by(zone) %>% 
          arrange(period) %>% 
          mutate(value = as.numeric(value),
                 change = ifelse(value-lag(value) < 0, 1, 0),
                 "variable" = "% Decreased data zones") %>% 
          filter(period != 2008) %>% 
          group_by(period, variable) %>% 
          summarise(value = sum(change)) %>% 
  mutate(area = "Scotland",
         "period" = as.numeric(period))) %>% 
  
  rbind(pop_estimates_datazones %>% 
          group_by(zone) %>% 
          arrange(period) %>% 
          mutate(value = as.numeric(value),
                 period = as.numeric(period),
                 change = ifelse(value-lag(value) < 0, 0, 1),
                 "variable" = "% Increased data zones") %>% 
          filter(period != 2008) %>% 
          
          left_join(data_zone_lookup, by = c("zone" = "DZ2011_Code")) %>%  
          rename("area" = LA_Name) %>% 
          group_by(area, period, variable) %>% 
          summarise(value = sum(change)) %>% 
          
          rbind(pop_estimates_datazones %>% 
                  group_by(zone) %>% 
                  arrange(period) %>% 
                  mutate(value = as.numeric(value),
                         change = ifelse(value-lag(value) < 0, 0, 1),
                         "variable" = "% Increased data zones") %>% 
                  filter(period != 2008) %>% 
                  group_by(period, variable) %>% 
                  summarise(value = sum(change)) %>% 
                  mutate(area = "Scotland",
                         "period" = as.numeric(period)))) %>% 
  group_by(area, period,) %>% 
  mutate(value = round((value/sum(value))*100, digits = 2),
         "indicator" = "Population Change")

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
       "variable" = "Within Scotland",
       "indicator" = "Net Migration",
       period = as.numeric(gsub("-.*","",period))+1) %>% 
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
  mutate("variable" = "Rest of the UK",
         "indicator" = "Net Migration",
         area = gsub('SCOTLAND', 'Scotland', area),
         period = as.numeric(gsub("-.*","",period))+1) %>% 
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
  mutate("variable" = "Overseas",
         "indicator" = "Net Migration",
         area = gsub('SCOTLAND', 'Scotland', area),
         # +1 is to pick the later of the year range
         period = as.numeric(gsub("-.*","",period))+1,
         ) %>% 
  filter(period >= 2009)



## ----------------------------------------------------------------
##                     Total Net Migration                      --
## ----------------------------------------------------------------


total_net_migration <- opendatascot:::ods_query_database(endpoint, 
                                                         net_migration_query) %>%
  mutate("variable" = "Total",
         "indicator" = "Net Migration") %>% 
  select(-c(sex, age))

## ----------------------------------------------------------------
##                     Components of Change                     --
## ----------------------------------------------------------------
## Natural change + net migration + other changes
## one year - potentially show 2 years

natural_change <- readxl::read_excel(file_path_natural_change) %>% 
  select(period = Year,
         area = Area,
         value = `Natural Change`)  %>%
  mutate("variable" = "Natural Change",
         "indicator" = "Population Change")

##################################################################
##                         Combine Data                         ##
##################################################################
 
combined_datasets <- adr %>% 
  mutate("variable" = paste(age, sex)) %>% 
  select(-c(age, sex)) %>%
  rbind(pop_change_by_council_area,
        pop_change_by_data_zone,
        natural_change)

healthy_life_expectancy <- healthy_life_expectancy %>% 
  mutate("variable" = paste(age, sex)) %>% 
  select(-c(age, sex))


pop_structure <- pop_structure %>% 
  filter(age != "All",
         period >= (current_year - 12)) %>% 
  mutate("variable" = paste(age, sex)) %>% 
  select(-c(age, sex))

migration_datasets <-  net_ruk %>%
  rbind(total_net_migration,
        net_overseas,
        net_within_scotland)