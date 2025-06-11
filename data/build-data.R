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

save(list = c('pa', 'il', 'cm'), file = '../app/data.RData')
 