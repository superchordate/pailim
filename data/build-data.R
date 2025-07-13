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
    `Type of Action: cm` = sort(unique(trimws(unlist(strsplit(unique(cm$`Type of Action`), ";"))))),
    `Type of Action: pa` = sort(unique(trimws(unlist(strsplit(unique(pa$`Type of Action`), ";"))))),
    `Type of Action: il` = sort(unique(trimws(unlist(strsplit(unique(il$`Type of Action`), ";")))))
)

# full mappings for line plots.
all_periods = list(
    `Year` = data.frame(X = unique(cm$Year)),
    `Month` = data.frame(X = unique(paste0(cm$Year, '_', pad0(cm$MonthNum, 2)))),
    `Quarter` = data.frame(X = unique(paste0(cm$Year, '_', cm$Quarter))),
    `Week` = data.frame(X = unique(paste0(cm$Year, '_', pad0(cm$Week, 2))))
)

#save(list = c('pa', 'il', 'cm', 'options'), file = '../app/data.RData')
qs_savem(cm, il, pa, options, all_periods, time_covariates, geo_covariates, file = "../app/data.qs2")
