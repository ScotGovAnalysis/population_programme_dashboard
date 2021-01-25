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

file_path_hle <- "data/HLE.xlsx"
file_path_net_with_scot <- "data/migflow-ca-01-latest-tab1.xlsx"
file_path_net_overseas <- "data/mig-overseas-admin-sex-tab1.xlsx"
file_path_components_of_change_19 <- "data/mid-year-pop-est-19-data.xlsx"
file_path_components_of_change_18 <- "data/mid-year-pop-est-18-tabs.xlsx"
file_path_ruk <- "data/mig-uk-admin-sex-91-latest-tab2.xlsx"
area_names <- readxl::read_xlsx("data/area_codes.xlsx")
data_zone_lookup <- read.csv("data/Datazone2011lookup.csv")

# Assign endpoint for SPARQL queries
endpoint <- "https://statistics.gov.scot/sparql"

# Get current year for calculating most recent range of data sets
current_year <- lubridate::year(lubridate::today())

# Generate years in quarters for current range.
# e.g. "2009-Q1", "2009-Q2", "2009-Q3", "2009-Q4"
quarters <- c("-Q1", "-Q2", "-Q3", "-Q4")
year_quarters <- paste0(rep(as.character(c((current_year - 12):(current_year))),
                            each = length(quarters)),
                        quarters)

source("SPARQL_queries.R")
##################################################################
##                       Reading Raw Data                       ##
##################################################################

##----------------------------------------------------------------
##                     Population Structure                     --
##----------------------------------------------------------------

pop_structure <- opendatascot:::ods_query_database(endpoint, pop_structure_query) %>%
  mutate(indicator = "Population Structure",
         sex = NA)

# Decreasing Population ---------------------------------------------------

decreasing_pop_by_council_area <- pop_structure %>% 
  filter(age == "All",
         area != "Scotland") %>%
  group_by(area) %>% 
  arrange(period) %>% 
  mutate(change = ifelse(value-lag(value) < 0, 1, 0),
         indicator = "Decreasing Population by council area") %>% 
  filter(period != 2008) %>% 
  group_by(period, indicator) %>% 
  summarise(decreased = sum(change)) %>% 
  mutate(area = "Scotland")


## ---------------------------------------------------------------
##                   Active Dependency Ratio                   --
## ---------------------------------------------------------------
# using opendatascot to deal with the quarters & join the data sets
## Need area names

# Get economic INACTIVITY
adr <- opendatascot::ods_dataset(
  "economic-inactivity",
  measureType = "count",
  gender = "all",
  refPeriod = year_quarters
) %>%
  rename("sex" = gender) %>%
  group_by(refArea, refPeriod) %>%
  summarise(inactivity = sum(as.numeric(value))) %>%
# Join economic ACTIVITY
  inner_join(opendatascot::ods_dataset(
    "economic-activity",
    measureType = "count",
    gender = "all",
    refPeriod = year_quarters
  ) %>%
    rename("sex" = gender) %>%
    mutate(refPeriod = factor(refPeriod,
                              levels = year_quarters)) %>%
    group_by(refArea, refPeriod) %>%
    summarise(activity = sum(as.numeric(value)))) %>%
 # calculate ADR with inactivity/activity multiplied by 1000
  mutate(
    value = (inactivity / activity) * 1000,
    indicator = "Active Dependency Ratio",
  ) %>% 
  left_join(area_names, by = c("refArea" = "area_code")) %>% 
  ungroup() %>% 
  select(area, period = "refPeriod", value, indicator) %>% 
  mutate(sex = NA,
         age = NA)


## ---------------------------------------------------------------
##                   Healthy life expectancy                   --
## ---------------------------------------------------------------


hle <- opendatascot:::ods_query_database(endpoint, hle_query) %>%
  mutate(indicator = "Healthy Life Expectancy")

# Join static healthy life expectancy data to API datset 
    # Statistics.gov.scot only has > 2015 - 2016
healthy_life_expectancy <- readxl::read_xlsx(file_path_hle) %>% 
  select("area" = Area_name,
         "period" = Period,
         "age" = `Age group`,
         "value" = `Healthy Life Expectancy (HLE) _`,
         "sex" = Sex
         ) %>%
  mutate(indicator = "Healthy Life Expectancy",
         # Match static file date format to stats.gov.scot dataset
         period = gsub('-', '-20', period),
         sex = gsub('s', '', sex),
         age = "0 years") %>% 
  # Remove any dates that are already in stats.gov.scot dataset
  filter(!(period %in% hle$period)) %>% 
  rbind(hle) %>% 
  arrange(period) %>% 
  mutate(age = NA)

## ---------------------------------------------------------------
##               Population Decline - Data Zones               --
## ---------------------------------------------------------------
# Too large for one call

# Build as list then unlist to speed up

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
  mutate(indicator = "Population Data Zones") %>%
  select(-measureType) %>%
  rename("zone" = refArea) 

decreasing_pop_by_data_zone <- pop_estimates_datazones %>% 
  group_by(zone) %>% 
  arrange(refPeriod) %>% 
  mutate(value = as.numeric(value),
         change = ifelse(value-lag(value) < 0, 1, 0),
         indicator = "Decreasing Population by data zone") %>% 
  filter(refPeriod != 2008) %>% 
  left_join(data_zone_lookup, by = c("zone" = "DZ2011_Code")) %>%  
  rename("area" = LA_Name) %>% 
  group_by(area, refPeriod, indicator) %>% 
  summarise(decreased = sum(change)) %>% 
  rbind(pop_estimates_datazones %>% 
          group_by(zone) %>% 
          arrange(refPeriod) %>% 
          mutate(value = as.numeric(value),
                 change = ifelse(value-lag(value) < 0, 1, 0),
                 indicator = "Decreasing Population by data zone") %>% 
          filter(refPeriod != 2008) %>% 
          group_by(refPeriod, indicator) %>% 
          summarise(decreased = sum(change)) %>% 
  mutate(area = "Scotland",
         "period" = as.numeric(refPeriod)) %>% 
  select(-refPeriod))
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
       "sex" = NA,
       "age" = NA, 
       indicator = "Net within Scotland")



## ----------------------------------------------------------------
##                      Net rest of the UK                      --
## ----------------------------------------------------------------

# net_ruk_scotland <- opendatascot:::ods_query_database(endpoint, net_ruk_query) %>% 
#   mutate(indicator = "Net Migration - Rest of UK")


net_ruk <- readxl::read_xlsx(file_path_ruk,
                                   sheet = "Net-Council-Sex (2001-)",
                                  range = "A5:T38") %>% 
  tidyr::pivot_longer(3:20, names_to = "period", values_to = "value") %>% 
  select("area" = `...2`, 
         period, 
         value) %>% 
  mutate("sex" = NA, 
         "age" = NA,
         indicator = "Net Migration - Rest of UK",
         area = gsub('SCOTLAND', 'Scotland', area))


## ----------------------------------------------------------------
##                         Net Overseas                         --
## ----------------------------------------------------------------

net_overseas <- readxl::read_excel(file_path_net_overseas,
                                   sheet = "Net-Council Area-Sex",
                                   range = "A5:T38"
) %>% 
  tidyr::pivot_longer(3:20, names_to = "period", values_to = "value") %>% 
  select("area" = `...2`, 
         period, 
         value) %>% 
  mutate("sex" = NA, 
         "age" = NA,
         indicator = "Net Overseas",
         area = gsub('SCOTLAND', 'Scotland', area))



## ----------------------------------------------------------------
##                     Total Net Migration                      --
## ----------------------------------------------------------------


total_net_migration <- opendatascot:::ods_query_database(endpoint, 
                                                         net_migration_query) %>%
  mutate(indicator = "Total Net Migration",
         age = NA,
         sex = NA)

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
  mutate(indicator = "Components of Change")

##################################################################
##                         Combine Data                         ##
##################################################################


combined_datasets <- pop_structure %>% 
  filter(age != "All") %>% 
  rbind(adr,
        healthy_life_expectancy,
        net_ruk,
        total_net_migration,
        net_overseas,
        net_within_scotland)

decreasing_pop <-  decreasing_pop_by_council_area %>% 
  rbind(decreasing_pop_by_data_zone)



