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

# for the first time 
# set preproc to true to generate data to be used for shiny app, set TRUE the first time you run to generate .RData
# then set to TRUE
# this process takes a long time

preproc = FALSE

if(preproc){
  
  # wrangle multiple geocodes into multiple observations  
  # Palestine
  pa = as_tibble(read.csv('Palestinian.Violence.Covariates_new.csv',fileEncoding = 'Windows-1252'))
  pa$CaseNum = 1:nrow(pa)
  pa = pa %>% mutate(Longitude=X.1,Latitude=Y.1)
  pa = pa %>% filter(Longitude!=0,Latitude!=0) 
  pa = pa %>% mutate_if(is.factor,as.character)
  pa = pa %>% mutate(Add=0)
  tmp = list()
  for(i in 1:nrow(pa)){
    # i=1
    d = pa[i,]
    stmp = list()
    for(j in 2:8){
      # j=2
      if(!is.na(d[[paste0('X.',j)]])) {
        stmp[[j]]=tibble(CaseNum=d[['CaseNum']],X.1=d[[paste0('X.',j)]],Y.1=d[[paste0('Y.',j)]],Add=1)
        stmp[[j]] = bind_cols(stmp[[j]],d %>% select(-CaseNum,-X.1,-Y.1,-Add))
      }
      tmp[[i]] = bind_rows(stmp)
    }
  }
  tmp = bind_rows(tmp) %>% mutate(Longitude=X.1,Latitude=Y.1) %>% filter(Longitude!=0,Latitude!=0) %>% distinct()
  pa = bind_rows(pa,tmp)
  
  # Israel
  il = as_tibble(read.csv('Israeli.Violence.Covariates_new.csv',fileEncoding = 'Windows-1252'))
  il$CaseNum = 1:nrow(il)
  
  il = il %>% mutate(Longitude=X.1,Latitude=Y.1)
  il = il %>% filter(Longitude!=0,Latitude!=0) 
  il = il %>% mutate_if(is.factor,as.character)
  il = il %>% mutate(Add=0)
  tmp = list()
  for(i in 1:nrow(il)){
    # i=1
    d = il[i,]
    stmp = list()
    for(j in 2:6){
      if(!is.na(d[[paste0('X.',j)]])) {
        stmp[[j]]=tibble(CaseNum=d[['CaseNum']],X.1=d[[paste0('X.',j)]],Y.1=d[[paste0('Y.',j)]],Add=1)
        stmp[[j]] = bind_cols(stmp[[j]],d %>% select(-CaseNum,-X.1,-Y.1,-Add))
      }
      tmp[[i]] = bind_rows(stmp)
    }
  }
  tmp = bind_rows(tmp) %>% mutate(Longitude=X.1,Latitude=Y.1) %>% filter(Longitude!=0,Latitude!=0) %>% distinct()
  il = bind_rows(il,tmp)
  
  # manipulate data base on Carly's request
  # palestine
  pa = pa %>% mutate_if(is.factor,as.character)
  pa$Month = factor(pa$Month)
  levels(pa$Month) = month.abb
  pa = pa %>% mutate(Perpetrator.Origin = ifelse(!is.na(Perpetrator.Origin.2)|!is.na(Perpetrator.Origin.3),'Multiple Perpetrators',Perpetrator.Origin.1)) #%>% filter(!is.na(Perpetrator.Type))
  pa = pa %>% mutate(Victim.Type = ifelse(!is.na(Victim.2)|(Victim.1=='Multiple Victim'),'Multiple Victims',Victim.1)) #%>% filter(!is.na(Victim.Type))
  pa$datatype = 'Palestine'
  table(pa$Perpetrator.Origin)
  pa = pa %>% mutate(Perpetrator.Origin = ifelse(Perpetrator.Origin=='Foreign',"Abroad",Perpetrator.Origin))
  pa = pa %>% mutate(Victim.Type = ifelse(Victim.Type=='Civilian',"Israeli Civilian",Victim.Type))
  pa = pa %>% mutate(Victim.Type = ifelse(Victim.Type=='Military',"Israeli Military",Victim.Type))
  pa = pa %>% mutate(Victim.Type = ifelse(Victim.Type=='Government',"Israeli Government",Victim.Type))
  pa = pa %>% mutate(Riot.SubCategory = ifelse(!is.na(Riot.SubCategory.2)|!is.na(Riot.SubCategory.3)|
                                                 !is.na(Riot.SubCategory.4)|!is.na(Riot.SubCategory.5)|!is.na(Riot.SubCategory.6),
                                               'Multiple Types',Riot.SubCategory.1)) #%>% filter(Riot.SubCategory!="")
  pa = pa %>% mutate(Riot.SubCategory = ifelse(Riot.SubCategory=='Border/Blockage Breaching','Border/Blockade Breaching',Riot.SubCategory))
  pa = pa %>% mutate(Riot.SubCategory = ifelse(Riot.SubCategory=='Stones Incendiary','Stones and Incendiary',Riot.SubCategory))
  pa = pa %>% mutate(Riot.SubCategory = ifelse(Riot.SubCategory=='Stone','Stones',Riot.SubCategory))
  pa = pa %>% mutate(Riot.SubCategory = ifelse(Riot.SubCategory=='',NA,Riot.SubCategory))
  
  # israel
  il = il %>% mutate_if(is.factor,as.character)
  il$Month = factor(il$Month)
  levels(il$Month) = month.abb
  il = il %>% mutate(Perpetrator.Type = ifelse(!is.na(Perpetrator.2),'Multiple Perpetrators',Perpetrator.1)) #%>% filter(!is.na(Perpetrator.Type))
  il = il %>% mutate(Victim.Type = ifelse(!is.na(Victim.2)|!is.na(Victim.3)|!is.na(Victim.4),'Multiple Victims',Victim.1)) #%>% filter(!is.na(Victim.Type))
  il$datatype = 'Israel'
  table(il$Perpetrator.Type)
  il = il %>% mutate(Perpetrator.Type = ifelse(Perpetrator.Type=='Civilians',"Israeli Civilians",Perpetrator.Type))
  il = il %>% mutate(Victim.Type = ifelse(Victim.Type=='PCI',"Palestinian Citizen of Israel",Victim.Type))
  il = il %>% mutate(Victim.Type = ifelse(Victim.Type=='Palestinian Child',"Palestinian Minor",Victim.Type))
  
  
  # combined
  pa = pa %>% mutate(Date=as.Date(as.character(Date),format="%Y%m%d",origin = "1970-01-01"))
  pa$MonthNum = lubridate::month(pa$Date)
  pa$Week = lubridate::week(pa$Date)
  pa$Quarter = lubridate::quarter(pa$Date)
  il = il %>% mutate(Date=as.Date(as.character(Date),format="%Y%m%d",origin = "1970-01-01"))
  il$MonthNum = lubridate::month(il$Date)
  il$Week = lubridate::week(il$Date)
  il$Quarter = lubridate::quarter(il$Date)
  write.csv(x=pa,file='pa_AD.csv',row.names=F,fileEncoding = 'Windows-1252')
  write.csv(x=il,file='il_AD.csv',row.names=F,fileEncoding = 'Windows-1252')
  pa = as_tibble(read.csv('pa_AD.csv',fileEncoding = 'Windows-1252')) %>% mutate_if(is.factor,as.character)
  il = as_tibble(read.csv('il_AD.csv',fileEncoding = 'Windows-1252')) %>% mutate_if(is.factor,as.character)
  il$Verbatim.Report = gsub("[<>+]", "", il$Verbatim.Report)
  il$Verbatim.Report = gsub("U00..", "", il$Verbatim.Report)
  pa$Verbatim.Report = gsub("[<>+]", "", pa$Verbatim.Report)
  pa$Verbatim.Report = gsub("U00..", "", pa$Verbatim.Report)
  
  # comment this line out if you want all data for israel
  #il = il[sample(1:160907,4000),] %>% arrange(Date)
  
  
  
  # combined data
  cm = bind_rows(pa %>% select(Add,Year,Month,Date,Week,MonthNum,Quarter,Longitude,Latitude,Casualties,Killed,Injured,Verbatim.Report,Type.Violence,Israeli.CPI, Palestinian.CPI,Israeli.UE.Quarterly,Palestinian.UE.Quarterly,Israeli.Trade.Balance,Palestinian.Trade.Balance,Exchange.Rate,Demolished.Structures.Daily,TA125.PX_CLOSE,PASISI.PX_CLOSE,
                               TAVG,PRCP,Total.Entries.Exits.Gaza.Israel, Total.Imports.Gaza.Israel/Total.Exports.Gaza.Israel,Victim.Type,datatype,starts_with('Secondary.Type.Violence')),
                 il %>% select(Add,Year,Month,Date,Week,MonthNum,Quarter,Longitude,Latitude,Casualties,Killed,Injured,Verbatim.Report,Type.Violence, Israeli.CPI, Palestinian.CPI,Israeli.UE.Quarterly,Palestinian.UE.Quarterly,Israeli.Trade.Balance,Palestinian.Trade.Balance,Exchange.Rate,Demolished.Structures.Daily,TA125.PX_CLOSE,PASISI.PX_CLOSE,
                               TAVG,PRCP,Total.Entries.Exits.Gaza.Israel, Total.Imports.Gaza.Israel/Total.Exports.Gaza.Israel, Victim.Type,datatype,starts_with('Secondary.Type.Violence')))
  cm$datatype = factor(cm$datatype)
  
  # start from 2010
  pa = pa %>% filter(Year >= 2010)
  il = il %>% filter(Year >= 2010)
  cm = cm %>% filter(Year >= 2010)
  il <- il %>% filter(City != "")
  save(list=c('pa','il','cm'),file='data.RData')
}

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
