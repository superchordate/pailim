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

qs_readm('data.qs2')

countries = iconList(
  Israel = makeIcon("www/flag-of-Israel.png", "www/flag-of-Israel.png",iconWidth=20, iconHeight=15),
  Palestine = makeIcon("www/flag-of-Palestine.png", "www/flag-of-Palestine.png",iconWidth=20, iconHeight=15)
)

months = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
