# load packages
require(tidyverse)
require(lubridate)
require(readr)
require(dplyr)
require(tibble)
require(easyr)
require(magrittr)
require(glue)
require(data.table)

begin()

runfolder("scripts")

# object the app can use. 
options = list(
    Year = sort(unique(cm$Year)),
    Crimes = sort(unique(trimws(unlist(strsplit(unique(cm$Crimes), ";")))))
)

save(list = c('pa', 'il', 'cm', 'options'), file = '../app/data.RData')
