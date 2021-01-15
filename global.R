#################################################################
##                          Libraries                          ##
#################################################################

library(shiny)
library(dplyr)
library(formattable)
library(sparkline)
library(SPARQL)

#################################################################
##                          Variables                          ##
#################################################################

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

##################################################################
##                       Reading Raw Data                       ##
##################################################################

##----------------------------------------------------------------
##                     Population Structure                     --
##----------------------------------------------------------------

# population structure by age
# decreasing population by council areas
pop_structure_query <- paste0("PREFIX qb: <http://purl.org/linked-data/cube#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
select   ?area ?period ?value ?age ?sex ?value
where { ?data qb:dataSet
  <http://statistics.gov.scot/data/population-estimates-2011-datazone-linked-dataset>.
  ?data <http://purl.org/linked-data/sdmx/2009/dimension#refArea> ?refArea.
  ?data <http://purl.org/linked-data/sdmx/2009/dimension#refPeriod> ?refPeriod.
  ?data <http://statistics.gov.scot/def/measure-properties/count> ?value.
  ?data <http://statistics.gov.scot/def/dimension/age> ?ageuri.
  ?data <http://statistics.gov.scot/def/dimension/sex> ?sexuri.
  ?refArea rdfs:label ?area .
  ?refPeriod rdfs:label ?period .
  ?ageuri rdfs:label ?age .
  ?sexuri rdfs:label ?sex .
  filter (strstarts(strafter(str(?refArea),
        'http://statistics.gov.scot/id/statistical-geography/'),'S12')
      ||strstarts(strafter(str(?refArea),
        'http://statistics.gov.scot/id/statistical-geography/'),'S92')).
  filter (regex(str(?sexuri ), 'all$'))
  filter (regex(str(?refPeriod ),'", (current_year - 12), "$')
        ||regex(str(?refPeriod ),'", (current_year - 11), "$')
        ||regex(str(?refPeriod ),'", (current_year - 10), "$')
        ||regex(str(?refPeriod ),'", (current_year - 9), "$')
        ||regex(str(?refPeriod ),'", (current_year - 8), "$')
        ||regex(str(?refPeriod ),'", (current_year - 7), "$')
        ||regex(str(?refPeriod ),'", (current_year - 6), "$')
        ||regex(str(?refPeriod ),'", (current_year - 5), "$')
        ||regex(str(?refPeriod ),'", (current_year - 4), "$')
        ||regex(str(?refPeriod ),'", (current_year - 3), "$')
        ||regex(str(?refPeriod ),'", (current_year - 2), "$')
        ||regex(str(?refPeriod ),'", (current_year - 1), "$')
        ||regex(str(?refPeriod ),'", (current_year), "$'))
}order by ?refPeriod")
pop_structure <- SPARQL(url = endpoint,
                        query = pop_structure_query)$results %>%
  mutate(indicator = "Population Structure")



## ---------------------------------------------------------------
##                   Active Dependency Ratio                   --
## ---------------------------------------------------------------
#

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
    indicator = "Active Dependency Ratio"
  )


## ---------------------------------------------------------------
##                   Healthy life expectancy                   --
## ---------------------------------------------------------------

# TODO Only has 2015-2017 & 2016-2018 - Use static for earlier years
hle_query <- "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?area ?period ?age ?value ?sex
WHERE {
  ?obs <http://purl.org/linked-data/cube#dataSet>
  <http://statistics.gov.scot/data/healthy-life-expectancy> .
 ?obs <http://purl.org/linked-data/sdmx/2009/dimension#refArea> ?areauri .
 ?obs <http://purl.org/linked-data/sdmx/2009/dimension#refPeriod> ?perioduri .
 ?obs <http://statistics.gov.scot/def/measure-properties/count> ?value .
 ?obs <http://statistics.gov.scot/def/dimension/age> ?ageuri .
 ?obs <http://statistics.gov.scot/def/dimension/sex> ?sexuri .
 ?areauri rdfs:label ?area .
 ?perioduri rdfs:label ?period .
 ?ageuri rdfs:label ?age .
 ?sexuri rdfs:label ?sex .
 FILTER regex(?age,'^0 years') .
} order by ?period"
healthy_life_expectancy <- SPARQL(url = endpoint, query = hle_query)$results %>%
  mutate(indicator = "Healthy Life Expectancy")


## ---------------------------------------------------------------
##               Population Decline - Data Zones               --
## ---------------------------------------------------------------

# Use OpenDataScot to more easily break up the data calls
# otherwise dataset is too large
pop_estimates_datazones <- opendatascot::ods_dataset(
  "population-estimates-2011-datazone-linked-dataset",
  geography = "dz",
  sex = "all",
  age = "all",
  refPeriod = as.character(c((current_year - 12):(current_year - 9)))
) %>%
  rbind(opendatascot::ods_dataset(
    "population-estimates-2011-datazone-linked-dataset",
    geography = "dz",
    sex = "all",
    age = "all",
    refPeriod = as.character(c((current_year - 8):(current_year - 5)))
  )) %>%
  rbind(opendatascot::ods_dataset(
    "population-estimates-2011-datazone-linked-dataset",
    geography = "dz",
    sex = "all",
    age = "all",
    refPeriod = as.character(c((current_year - 4):current_year))
  )) %>%
  mutate(indicator = "Population Data Zones") %>%
  select(-measureType) %>%
  rename("area" = refArea) %>%
  # Make data set wider by period to calculate no. decreasing each year
  tidyr::pivot_wider(names_from = refPeriod, values_from = value)


## ---------------------------------------------------------------
##                     Net Within Scotland                     --
## ---------------------------------------------------------------

net_within_scotland <- readxl::read_excel(
  "data/migflow-ca-01-latest-tab1.xlsx",
  sheet = "TS - Internal Migration",
  range = "A5:T38"
) %>%
  rename(area_code = `...1`,
         area_name = `...2`)
# Fix unnamed first row
net_within_scotland[[1]][1] <- "S92000003"
net_within_scotland[[2]][1] <- "Scotland"


## ----------------------------------------------------------------
##                      Net rest of the UK                      --
## ----------------------------------------------------------------
net_ruk_query <- "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?area ?period ?age ?value ?sex
WHERE {
  ?obs <http://purl.org/linked-data/cube#dataSet>
  <http://statistics.gov.scot/data/migration-to-and-from-scotland> .
  ?obs <http://purl.org/linked-data/sdmx/2009/dimension#refArea> ?areauri .
  ?obs <http://purl.org/linked-data/sdmx/2009/dimension#refPeriod> ?perioduri .
  ?obs <http://statistics.gov.scot/def/measure-properties/count> ?value .
  ?obs <http://statistics.gov.scot/def/dimension/age> ?ageuri .
  ?obs <http://statistics.gov.scot/def/dimension/sex> ?sexuri .
  ?obs <http://statistics.gov.scot/def/dimension/migrationType> ?typeuri .
  ?obs <http://statistics.gov.scot/def/dimension/migrationSource> ?sourceuri .
  ?areauri rdfs:label ?area .
  ?perioduri rdfs:label ?period .
  ?ageuri rdfs:label ?age .
  ?sexuri rdfs:label ?sex .
  ?typeuri rdfs:label ?type .
  ?sourceuri rdfs:label ?source .
  FILTER regex(?age, 'All') .
  FILTER regex(?sex, 'All') .
  FILTER regex(?type, 'Net') .
  FILTER (?period > 2008) .
  FILTER regex(?source, 'To-from Rest of UK') .
}"
net_ruk_scotland <- SPARQL(url = endpoint, query = net_ruk_query)$results %>%
  mutate(indicator = "Net Migration - rUK")

## ----------------------------------------------------------------
##                         Net Overseas                         --
## ----------------------------------------------------------------

net_overseas <- readxl::read_excel("data/mig-overseas-admin-sex-tab1.xlsx",
                                   sheet = "Net-Council Area-Sex",
                                   range = "A5:T38"
)


## ----------------------------------------------------------------
##                     Total Net Migration                      --
## ----------------------------------------------------------------

net_migration_query <- "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?area ?period ?age ?value ?sex
WHERE {
  ?obs <http://purl.org/linked-data/cube#dataSet>
  <http://statistics.gov.scot/data/net-migration> .
  ?obs <http://purl.org/linked-data/sdmx/2009/dimension#refArea> ?areauri .
  ?obs <http://purl.org/linked-data/sdmx/2009/dimension#refPeriod> ?perioduri .
  ?obs <http://statistics.gov.scot/def/measure-properties/count> ?value .
  ?obs <http://statistics.gov.scot/def/dimension/age> ?ageuri .
  ?obs <http://statistics.gov.scot/def/dimension/sex> ?sexuri .
  ?areauri rdfs:label ?area .
  ?perioduri rdfs:label ?period .
  ?ageuri rdfs:label ?age .
  ?sexuri rdfs:label ?sex .
  FILTER regex(?age, 'All') .
  FILTER regex(?sex, 'All') .
  FILTER (?period > 2008) .
}"

total_net_migration <- SPARQL(url = endpoint,
                              query = net_migration_query)$results %>%
  mutate(indicator = "Total Net Migration")


## ----------------------------------------------------------------
##                     Components of Change                     --
## ----------------------------------------------------------------
## Natural change + net migration + other changes
## one year - potentially show 2 years

components_of_change <- readxl::read_excel(
  "data/mid-year-pop-est-19-data.xlsx",
  range = "A3:K39",
  sheet = "Table 4") %>%
  janitor::clean_names() %>%
  filter(!(is.na(area_code1_2)))






##################################################################
##                         Combine Data                         ##
##################################################################

migration_datasets <- net_ruk_scotland %>% rbind(total_net_migration)