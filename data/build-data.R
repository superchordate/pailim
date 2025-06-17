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
require(qs2)

begin()

runfolder("scripts")

# object the app can use. 
options = list(
    Year = sort(unique(cm$Year)),
    `Type of Action` = sort(unique(trimws(unlist(strsplit(unique(cm$`Type of Action`), ";")))))
)

#save(list = c('pa', 'il', 'cm', 'options'), file = '../app/data.RData')
qs_savem(cm, il, pa, options, file = "../app/data.qs2")
