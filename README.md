# [Population Programme Dashboard](https://scotland.shinyapps.io/population_dashboard/)
An R Shiny app showing the population trends from the last 10 years of published data in small multiples. 
This dashboard supports the [Population Taskforce](https://www.gov.scot/groups/population-task-force/) to understand the demographic challenges in Scotland.
  
![Screenshot of the population dashboard shiny
app](https://github.com/DataScienceScotland/population_programme_dashboard/blob/master/www/homepage_screenshot.png)
  

## How it works

Pulls the lastest data from the [SG open data platform](https://statistics.gov.scot/) and [NRS statistics](https://www.nrscotland.gov.uk/statistics-and-data/statistics/)

## How to update
1. Add new static data to data folder and update path files for static data in [`global.R`](https://github.com/DataScienceScotland/population_programme_dashboard/blob/master/global.R)
2. Update  "<b>Last updated: DD Month YYYY</b>" if the <b>About</b> page or <b>Accessibility</b> page have been update (in [`server.R`](https://github.com/DataScienceScotland/population_programme_dashboard/blob/master/server.R)).
3. Deploy to [shinyapps.io](https://shiny.rstudio.com/articles/shinyapps.html)

## How it works

## Licence
