require(tidyr)
require(glue)
require(shinyjs)
require(data.table)
require(leaflet)
require(dplyr)
require(shinycssloaders)
require(shiny)
require(shinydashboard)
require(shinyWidgets)
require(DT)
require(plotly)
require(scales)
require(qs2)
require(stringr)
require(magrittr)
require(easyr)

# load data for shiny
load('data.RData')
#load('data_subset.RData')

countries = iconList(
  Israel = makeIcon("www/flag-of-Israel.png", "www/flag-of-Israel.png",iconWidth=20, iconHeight=15),
  Palestine = makeIcon("www/flag-of-Palestine.png", "www/flag-of-Palestine.png",iconWidth=20, iconHeight=15)
)

months = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')


# available plot types for selected actions
chooseData_both = sort(c('Events', 'Casualties'))
chooseData_pa = sort(c('Events', 'Casualties','Rockets','Incendiary Balloons','Riots'))
chooseData_il = sort(c('Events', 'Casualties', 'Detentions'))
