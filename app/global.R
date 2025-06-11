#setwd("/Users/amaancharaniya/Desktop/pailim_app_final1")

library(leaflet)
library(tidyverse)
library(shinycssloaders)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(plotly)
library(scales)
library(shinyjs)
library(rsconnect)
library(quarto)

options(shiny.sanitize.errors = FALSE)

# load data for shiny
load('data.RData')
#load('data_subset.RData')

countries = iconList(
  Israel = makeIcon("www/flag-of-Israel.png", "www/flag-of-Israel.png",iconWidth=20, iconHeight=15),
  Palestine = makeIcon("www/flag-of-Palestine.png", "www/flag-of-Palestine.png",iconWidth=20, iconHeight=15)
)

months = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')


# available plot types for selected actions
maptype_both = sort(c('Events', 'Casualties'))
maptype_pa = sort(c('Events', 'Casualties','Rockets','Incendiary Balloons','Riots'))
maptype_il = sort(c('Events', 'Casualties', 'Detentions'))




# shinyApp(ui, server)
# moved deploy call to deploy.R and secrets.
# showLogs()
