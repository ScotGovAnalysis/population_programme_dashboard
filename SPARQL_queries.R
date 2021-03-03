
# Population Structure   --------------------------------------------------

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
  filter (regex(str(?refPeriod ),'", (current_year - 13), "$')
        ||regex(str(?refPeriod ),'", (current_year - 12), "$')
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


# Life expectancy confidence intervals -------------------------------------------------

le_query_ci <- "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?area ?period ?sex ?lower_ci ?upper_ci
WHERE {
  ?obs <http://purl.org/linked-data/cube#dataSet>
  <http://statistics.gov.scot/data/Life-Expectancy> .
 ?obs <http://purl.org/linked-data/sdmx/2009/dimension#refArea> ?areauri .
 ?obs <http://purl.org/linked-data/sdmx/2009/dimension#refPeriod> ?perioduri .
 ?obs <http://statistics.gov.scot/def/measure-properties/95-lower-confidence-limit> ?lower_ci .
 ?obs <http://statistics.gov.scot/def/measure-properties/95-lower-confidence-limit> ?upper_ci .
 ?obs <http://statistics.gov.scot/def/dimension/urbanRuralClassification> ?urbanRuralClassification .
 ?obs <http://statistics.gov.scot/def/dimension/simdQuintiles> ?simdQuintiles .
 ?obs <http://statistics.gov.scot/def/dimension/age> ?ageuri .
 ?obs <http://statistics.gov.scot/def/dimension/sex> ?sexuri .
 ?areauri rdfs:label ?area .
 ?perioduri rdfs:label ?period .
 ?ageuri rdfs:label ?age .
 ?sexuri rdfs:label ?sex .
 FILTER (strstarts(strafter(str(?areauri),
        'http://statistics.gov.scot/id/statistical-geography/'),'S12')
      ||strstarts(strafter(str(?areauri),
        'http://statistics.gov.scot/id/statistical-geography/'),'S92')). 
 FILTER regex(?age,'^0 years') .
 FILTER (regex(str(?urbanRuralClassification ), 'all$')) .
 FILTER (regex(str(?simdQuintiles ), 'all$')) .
} order by ?period"

# Life expectancy -------------------------------------------------

le_query <- "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?area ?period ?sex ?value
WHERE {
  ?obs <http://purl.org/linked-data/cube#dataSet>
  <http://statistics.gov.scot/data/Life-Expectancy> .
 ?obs <http://purl.org/linked-data/sdmx/2009/dimension#refArea> ?areauri .
 ?obs <http://purl.org/linked-data/sdmx/2009/dimension#refPeriod> ?perioduri .
 ?obs <http://statistics.gov.scot/def/measure-properties/count> ?value .
 ?obs <http://statistics.gov.scot/def/dimension/urbanRuralClassification> ?urbanRuralClassification .
 ?obs <http://statistics.gov.scot/def/dimension/simdQuintiles> ?simdQuintiles .
 ?obs <http://statistics.gov.scot/def/dimension/age> ?ageuri .
 ?obs <http://statistics.gov.scot/def/dimension/sex> ?sexuri .
 ?areauri rdfs:label ?area .
 ?perioduri rdfs:label ?period .
 ?ageuri rdfs:label ?age .
 ?sexuri rdfs:label ?sex .
 FILTER (strstarts(strafter(str(?areauri),
        'http://statistics.gov.scot/id/statistical-geography/'),'S12')
      ||strstarts(strafter(str(?areauri),
        'http://statistics.gov.scot/id/statistical-geography/'),'S92')). 
 FILTER regex(?age,'^0 years') .
 FILTER (regex(str(?urbanRuralClassification ), 'all$')) .
 FILTER (regex(str(?simdQuintiles ), 'all$')) .
} order by ?period"

# ?obs <http://statistics.gov.scot/def/measure-properties/count> ?value .
# ?obs <http://statistics.gov.scot/def/measure-properties/95-lower-confidence-limit> ?lower_ci .
# ?obs <http://statistics.gov.scot/def/measure-properties/95-lower-confidence-limit> ?upper_ci .
# Healthy life expectancy -------------------------------------------------

# Using SPARQL to join the area names

hle_query <- "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?area ?period ?value ?sex
WHERE {
  ?obs <http://purl.org/linked-data/cube#dataSet>
  <http://statistics.gov.scot/data/healthy-life-expectancy> .
 ?obs <http://purl.org/linked-data/sdmx/2009/dimension#refArea> ?areauri .
 ?obs <http://purl.org/linked-data/sdmx/2009/dimension#refPeriod> ?perioduri .
 ?obs <http://statistics.gov.scot/def/measure-properties/count> ?value .
 ?obs <http://statistics.gov.scot/def/dimension/urbanRuralClassification> ?urbanRuralClassification .
 ?obs <http://statistics.gov.scot/def/dimension/simdQuintiles> ?simdQuintiles .
 ?obs <http://statistics.gov.scot/def/dimension/age> ?ageuri .
 ?obs <http://statistics.gov.scot/def/dimension/sex> ?sexuri .
 ?areauri rdfs:label ?area .
 ?perioduri rdfs:label ?period .
 ?ageuri rdfs:label ?age .
 ?sexuri rdfs:label ?sex .
 FILTER (strstarts(strafter(str(?areauri),
        'http://statistics.gov.scot/id/statistical-geography/'),'S12')
      ||strstarts(strafter(str(?areauri),
        'http://statistics.gov.scot/id/statistical-geography/'),'S92')). 
 FILTER regex(?age,'^0 years') .
 FILTER (regex(str(?urbanRuralClassification ), 'all$')) .
 FILTER (regex(str(?simdQuintiles ), 'all$')) .
} order by ?period"


#  Net rest of the UK   ---------------------------------------------------


# Using SPARQL for area names 

# net_ruk_query <- "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
# SELECT ?area ?period ?age ?value ?sex
# WHERE {
#   ?obs <http://purl.org/linked-data/cube#dataSet>
#   <http://statistics.gov.scot/data/migration-to-and-from-scotland> .
#   ?obs <http://purl.org/linked-data/sdmx/2009/dimension#refArea> ?areauri .
#   ?obs <http://purl.org/linked-data/sdmx/2009/dimension#refPeriod> ?perioduri .
#   ?obs <http://statistics.gov.scot/def/measure-properties/count> ?value .
#   ?obs <http://statistics.gov.scot/def/dimension/age> ?ageuri .
#   ?obs <http://statistics.gov.scot/def/dimension/sex> ?sexuri .
#   ?obs <http://statistics.gov.scot/def/dimension/migrationType> ?typeuri .
#   ?obs <http://statistics.gov.scot/def/dimension/migrationSource> ?sourceuri .
#   ?areauri rdfs:label ?area .
#   ?perioduri rdfs:label ?period .
#   ?ageuri rdfs:label ?age .
#   ?sexuri rdfs:label ?sex .
#   ?typeuri rdfs:label ?type .
#   ?sourceuri rdfs:label ?source .
#   FILTER regex(?age, 'All') .
#   FILTER regex(?sex, 'All') .
#   FILTER regex(?type, 'Net') .
#   FILTER (?period > 2008) .
#   FILTER regex(?source, 'To-from Rest of UK') .
# }"



# Total Net Migration  ----------------------------------------------------


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
  FILTER (?period > 2009) .
}"
