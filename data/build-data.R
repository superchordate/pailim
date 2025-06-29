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

# full mappings for line plots.
all_periods = list(
    `Annually` = data.frame(X = unique(cm$Year)),
    `Monthly` = data.frame(X = unique(paste0(cm$Year, '_', pad0(cm$MonthNum, 2)))),
    `Quarterly` = data.frame(X = unique(paste0(cm$Year, '_', cm$Quarter))),
    `Weekly` = data.frame(X = unique(paste0(cm$Year, '_', pad0(cm$Week, 2))))
)

#save(list = c('pa', 'il', 'cm', 'options'), file = '../app/data.RData')
qs_savem(cm, il, pa, options, all_periods, file = "../app/data.qs2")
