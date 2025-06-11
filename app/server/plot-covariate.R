# covariate plot
output$myplot1 = renderPlotly({
  
  req(input$palestine_or_israel)
  req(dataPlot())
  print(dim(dataPlot()))
  req(input$graphPeriods)
  req(input$palestine_or_israel %in% c('Palestinian Actions','Israeli Actions', 'Both'))
  req(input$p2!='None')
  
  d = dataPlot() %>% filter(Add==0)
  
  #d = pa
  
  labtab = tibble(
    cov_type=c('Consumer Price Index','Unemployment',
                'Trade Balance','Exchange Rate','Home Demolitions by Israel','Stock Market Index',
                'Temperature','Rainfall','Israel-Gaza Crossing (People)','Israel-Gaza Crossing (Goods)',
                'Hamas-Fatah Reconciliation Talks','Israeli Operation','US-Israel State Visits',
                'UN Vote','Israeli Coalition Size'
    ),
    label = c('CPI Percent','Percent Unemployed',
              'Exports - Imports in USD mn','Value of Israeli Shekel (NIS) Compared to USD','Number of Structures Demolished','Closing Value of Stock Index (NIS)',
              'Temperature','Rainfall','Number of Individual Entries + Exits','Number of Truckload Entries + Exits',
              'Ongoing = 1','Ongoing = 1','State Visit Ongoing = 1','UNSC or UNGA Takes Vote on Israel/Palestine = 1','Number of Knesset Members in Ruling Coalition'
    )
  )
  
  
  if(input$palestine_or_israel=='Palestinian Actions'){
    if(input$graphPeriods=='Annually'){
      if(input$p2=='Consumer Price Index'){
        d = d %>% group_by(Year) %>% summarise(Z=mean(Israeli.CPI,na.rm=T),Y=mean(Palestinian.CPI,na.rm=T)) %>% mutate(X=Year)
      } else if(input$p2=='Unemployment'){
        d = d %>% group_by(Year) %>% summarise(Z=mean(Israeli.UE.Quarterly,na.rm=T),Y=mean(Palestinian.UE.Quarterly,na.rm=T)) %>% mutate(X=Year)
      } else if(input$p2=='Trade Balance'){
        d = d %>% group_by(Year) %>% summarise(Z=mean(Israeli.Trade.Balance,na.rm=T),Y=mean(Palestinian.Trade.Balance,na.rm=T)) %>% mutate(X=Year)
      } else if(input$p2=='Exchange Rate'){
        d = d %>% group_by(Year) %>% summarise(Y=mean(Exchange.Rate,na.rm=T)) %>% mutate(X=Year)
      } else if(input$p2=='Home Demolitions by Israel'){
        d = d %>% group_by(Year) %>% summarise(Y=mean(Demolished.Structures.Daily,na.rm=T)) %>% mutate(X=Year)
      } else if(input$p2=='Stock Market Index'){
        d = d %>% group_by(Year) %>% summarise(Z=mean(TA125.PX_CLOSE,na.rm=T),Y=mean(PASISI.PX_CLOSE,na.rm=T)) %>% mutate(X=Year)
      } else if(input$p2=='Temperature'){
        d = d %>% group_by(Year) %>% summarise(Y=mean(TAVG,na.rm=T)) %>% mutate(X=Year)
      } else if(input$p2=='Rainfall'){
        d = d %>% group_by(Year) %>% summarise(Y=mean(PRCP,na.rm=T)) %>% mutate(X=Year)
      } else if(input$p2=='Israel-Gaza Crossing (People)'){
        d = d %>% group_by(Year) %>% summarise(Y=mean(Total.Entries.Exits.Gaza.Israel,na.rm=T)) %>% mutate(X=Year)
      } else if(input$p2=='Israel-Gaza Crossing (Goods)'){
        d = d %>% mutate(Goods = Total.Imports.Gaza.Israel/Total.Exports.Gaza.Israel)
        d$Goods[is.infinite(d$Goods)]=NA
        d = d %>% group_by(Year) %>% summarise(Y=mean(Goods,na.rm=T)) %>% mutate(X=Year)
      }
    }
    else if(input$graphPeriods=='Monthly'){
      if(input$p2=='Consumer Price Index'){
        d = d %>% group_by(Year,MonthNum) %>% summarise(Z=mean(Israeli.CPI,na.rm=T),Y=mean(Palestinian.CPI,na.rm=T)) %>% mutate(X=paste0(Year,'_',MonthNum))
        #d = d %>% group_by(Year,MonthNum) %>% summarise(X=mean(Israeli.UE.Quarterly,na.rm=T),Y=mean(Palestinian.UE.Quarterly,na.rm=T))
      } else if(input$p2=='Trade Balance'){
        d = d %>% group_by(Year,MonthNum) %>% summarise(Z=mean(Israeli.Trade.Balance,na.rm=T),Y=mean(Palestinian.Trade.Balance,na.rm=T)) %>% mutate(X=paste0(Year,'_',MonthNum))
      } else if(input$p2=='Exchange Rate'){
        d = d %>% group_by(Year,MonthNum) %>% summarise(Y=mean(Exchange.Rate,na.rm=T)) %>% mutate(X=paste0(Year,'_',MonthNum))
      } else if(input$p2=='Home Demolitions by Israel'){
        d = d %>% group_by(Year,MonthNum) %>% summarise(Y=mean(Demolished.Structures.Daily,na.rm=T)) %>% mutate(X=paste0(Year,'_',MonthNum))
      } else if(input$p2=='Stock Market Index'){
        d = d %>% group_by(Year,MonthNum) %>% summarise(Z=mean(TA125.PX_CLOSE,na.rm=T),Y=mean(PASISI.PX_CLOSE,na.rm=T)) %>% mutate(X=paste0(Year,'_',MonthNum))
      } else if(input$p2=='Temperature'){
        d = d %>% group_by(Year,MonthNum) %>% summarise(Y=mean(TAVG,na.rm=T)) %>% mutate(X=paste0(Year,'_',MonthNum))
      } else if(input$p2=='Rainfall'){
        d = d %>% group_by(Year,MonthNum) %>% summarise(Y=mean(PRCP,na.rm=T)) %>% mutate(X=paste0(Year,'_',MonthNum))
      } else if(input$p2=='Israel-Gaza Crossing (People)'){
        d = d %>% group_by(Year,MonthNum) %>% summarise(Y=mean(Total.Entries.Exits.Gaza.Israel,na.rm=T)) %>% mutate(X=paste0(Year,'_',MonthNum))
      } else if(input$p2=='Israel-Gaza Crossing (Goods)'){
        d = d %>% mutate(Goods = Total.Imports.Gaza.Israel/Total.Exports.Gaza.Israel)
        d$Goods[is.infinite(d$Goods)]=NA
        d = d %>% group_by(Year,MonthNum) %>% summarise(Y=mean(Goods,na.rm=T)) %>% mutate(X=Year)
        #d = d %>% group_by(Year,MonthNum) %>% summarise(Y=mean(Total.Imports.Gaza.Israel/Total.Exports.Gaza.Israel,na.rm=T)) %>% mutate(X=paste0(Year,'_',MonthNum))
      }
    }
    else if(input$graphPeriods=='Quarterly'){
      if(input$p2=='Consumer Price Index'){
        d = d %>% group_by(Year,Quarter) %>% summarise(Z=mean(Israeli.CPI,na.rm=T),Y=mean(Palestinian.CPI,na.rm=T)) %>% mutate(X=paste0(Year,'_',Quarter))
      } else if(input$p2=='Unemployment'){
        d = d %>% group_by(Year,Quarter) %>% summarise(Z=mean(Israeli.UE.Quarterly,na.rm=T),Y=mean(Palestinian.UE.Quarterly,na.rm=T)) %>% mutate(X=paste0(Year,'_',Quarter))
      } else if(input$p2=='Trade Balance'){
        d = d %>% group_by(Year,Quarter) %>% summarise(Z=mean(Israeli.Trade.Balance,na.rm=T),Y=mean(Palestinian.Trade.Balance,na.rm=T)) %>% mutate(X=paste0(Year,'_',Quarter))
      } else if(input$p2=='Exchange Rate'){
        d = d %>% group_by(Year,Quarter) %>% summarise(Y=mean(Exchange.Rate,na.rm=T)) %>% mutate(X=paste0(Year,'_',Quarter))
      } else if(input$p2=='Home Demolitions by Israel'){
        d = d %>% group_by(Year,Quarter) %>% summarise(Y=mean(Demolished.Structures.Daily,na.rm=T)) %>% mutate(X=paste0(Year,'_',Quarter))
      } else if(input$p2=='Stock Market Index'){
        d = d %>% group_by(Year,Quarter) %>% summarise(Z=mean(TA125.PX_CLOSE,na.rm=T),Y=mean(PASISI.PX_CLOSE,na.rm=T)) %>% mutate(X=paste0(Year,'_',Quarter))
      } else if(input$p2=='Temperature'){
        d = d %>% group_by(Year,Quarter) %>% summarise(Y=mean(TAVG,na.rm=T)) %>% mutate(X=paste0(Year,'_',Quarter))
      } else if(input$p2=='Rainfall'){
        d = d %>% group_by(Year,Quarter) %>% summarise(Y=mean(PRCP,na.rm=T)) %>% mutate(X=paste0(Year,'_',Quarter))
      } else if(input$p2=='Israel-Gaza Crossing (People)'){
        d = d %>% group_by(Year,Quarter) %>% summarise(Y=mean(Total.Entries.Exits.Gaza.Israel,na.rm=T)) %>% mutate(X=paste0(Year,'_',Quarter))
      } else if(input$p2=='Israel-Gaza Crossing (Goods)'){
        d = d %>% mutate(Goods = Total.Imports.Gaza.Israel/Total.Exports.Gaza.Israel)
        d$Goods[is.infinite(d$Goods)]=NA
        d = d %>% group_by(Year,Quarter) %>% summarise(Y=mean(Goods,na.rm=T)) %>% mutate(X=Year)
        #d = d %>% group_by(Year,Quarter) %>% summarise(Y=mean(Total.Imports.Gaza.Israel/Total.Exports.Gaza.Israel,na.rm=T)) %>% mutate(X=paste0(Year,'_',Quarter))
      }
    }
    else if(input$graphPeriods=='Weekly'){
      #d = d %>% group_by(Year,Week) %>% summarise(X=mean(Israeli.CPI,na.rm=T),Y=mean(Palestinian.CPI,na.rm=T))
      #d = d %>% group_by(Year,Week) %>% summarise(X=mean(Israeli.UE.Quarterly,na.rm=T),Y=mean(Palestinian.UE.Quarterly,na.rm=T))
      #d = d %>% group_by(Year,Week) %>% summarise(X=mean(Israeli.Trade.Balance,na.rm=T),Y=mean(Palestinian.Trade.Balance,na.rm=T))
      
      if(input$p2=='Exchange Rate'){
        d = d %>% group_by(Year,Week) %>% summarise(Y=mean(Exchange.Rate,na.rm=T)) %>% mutate(X=paste0(Year,'_',Week))
      } else if(input$p2=='Home Demolitions by Israel'){
        d = d %>% group_by(Year,Week) %>% summarise(Y=mean(Demolished.Structures.Daily,na.rm=T)) %>% mutate(X=paste0(Year,'_',Week))
      } else if(input$p2=='Stock Market Index'){
        d = d %>% group_by(Year,Week) %>% summarise(Z=mean(TA125.PX_CLOSE,na.rm=T),Y=mean(PASISI.PX_CLOSE,na.rm=T)) %>% mutate(X=paste0(Year,'_',Week))
      } else if(input$p2=='Temperature'){
        d = d %>% group_by(Year,Week) %>% summarise(Y=mean(TAVG,na.rm=T)) %>% mutate(X=paste0(Year,'_',Week))
      } else if(input$p2=='Rainfall'){
        d = d %>% group_by(Year,Week) %>% summarise(Y=mean(PRCP,na.rm=T)) %>% mutate(X=paste0(Year,'_',Week))
      } else if(input$p2=='Hamas-Fatah Reconciliation Talks') {
        d = d %>% group_by(Year,Week) %>% summarise(Y=ceiling(mean(hamas_fatah_talks_ongoing,na.rm=T))) %>% mutate(X=paste0(Year,'_',Week))
      } else if(input$p2=='Israeli Operation') {
        d = d %>% group_by(Year,Week) %>% summarise(Y=ceiling(mean(operation_ongoing,na.rm=T))) %>% mutate(X=paste0(Year,'_',Week))
      } else if(input$p2=='US-Israel State Visits') {
        d = d %>% group_by(Year,Week) %>% summarise(Y=ceiling(mean(israel_visit,na.rm=T))) %>% mutate(X=paste0(Year,'_',Week))
      } else if(input$p2=='UN Vote') {
        d = d %>% mutate(un_vote = ifelse(unsc_vote==1 | unga_vote==1,1,0))
        d = d %>% group_by(Year,Week) %>% summarise(Y=ceiling(mean(un_vote,na.rm=T))) %>% mutate(X=paste0(Year,'_',Week))
      } else if(input$p2=='Israeli Coalition Size') {
        d = d %>% group_by(Year,Week) %>% summarise(Y=round(mean(Coalition.Size,na.rm=T))) %>% mutate(X=paste0(Year,'_',Week))
      }
    }
  } else if (input$palestine_or_israel=='Israeli Actions'){
    
    if(input$graphPeriods=='Annually'){
      if(input$p2=='Consumer Price Index'){
        d = d %>% group_by(Year) %>% summarise(Z=mean(Israeli.CPI,na.rm=T),Y=mean(Palestinian.CPI,na.rm=T)) %>% mutate(X=Year)
      } else if(input$p2=='Unemployment'){
        d = d %>% group_by(Year) %>% summarise(Z=mean(Israeli.UE.Quarterly,na.rm=T),Y=mean(Palestinian.UE.Quarterly,na.rm=T)) %>% mutate(X=Year)
      } else if(input$p2=='Trade Balance'){
        d = d %>% group_by(Year) %>% summarise(Z=mean(Israeli.Trade.Balance,na.rm=T),Y=mean(Palestinian.Trade.Balance,na.rm=T)) %>% mutate(X=Year)
      } else if(input$p2=='Exchange Rate'){
        d = d %>% group_by(Year) %>% summarise(Y=mean(Exchange.Rate,na.rm=T)) %>% mutate(X=Year)
      } else if(input$p2=='Home Demolitions by Israel'){
        d = d %>% group_by(Year) %>% summarise(Y=mean(Demolished.Structures.Daily,na.rm=T)) %>% mutate(X=Year)
      } else if(input$p2=='Stock Market Index'){
        d = d %>% group_by(Year) %>% summarise(Z=mean(TA125.PX_CLOSE,na.rm=T),Y=mean(PASISI.PX_CLOSE,na.rm=T)) %>% mutate(X=Year)
      } else if(input$p2=='Temperature'){
        d = d %>% group_by(Year) %>% summarise(Y=mean(TAVG,na.rm=T)) %>% mutate(X=Year)
      } else if(input$p2=='Rainfall'){
        d = d %>% group_by(Year) %>% summarise(Y=mean(PRCP,na.rm=T)) %>% mutate(X=Year)
      } else if(input$p2=='Israel-Gaza Crossing (People)'){
        d = d %>% group_by(Year) %>% summarise(Y=mean(Total.Entries.Exits.Gaza.Israel,na.rm=T)) %>% mutate(X=Year)
      } else if(input$p2=='Israel-Gaza Crossing (Goods)'){
        d = d %>% mutate(Goods = Total.Imports.Gaza.Israel/Total.Exports.Gaza.Israel)
        d$Goods[is.infinite(d$Goods)]=NA
        d = d %>% group_by(Year) %>% summarise(Y=mean(Goods,na.rm=T)) %>% mutate(X=Year)
        #d = d %>% group_by(Year) %>% summarise(Y=mean(Total.Imports.Gaza.Israel/Total.Exports.Gaza.Israel,na.rm=T)) %>% mutate(X=Year)
      }
    }
    else if(input$graphPeriods=='Monthly'){
      if(input$p2=='Consumer Price Index'){
        d = d %>% group_by(Year,MonthNum) %>% summarise(Z=mean(Israeli.CPI,na.rm=T),Y=mean(Palestinian.CPI,na.rm=T)) %>% mutate(X=paste0(Year,'_',MonthNum))
        #d = d %>% group_by(Year,MonthNum) %>% summarise(X=mean(Israeli.UE.Quarterly,na.rm=T),Y=mean(Palestinian.UE.Quarterly,na.rm=T))
      } else if(input$p2=='Trade Balance'){
        d = d %>% group_by(Year,MonthNum) %>% summarise(Z=mean(Israeli.Trade.Balance,na.rm=T),Y=mean(Palestinian.Trade.Balance,na.rm=T)) %>% mutate(X=paste0(Year,'_',MonthNum))
      } else if(input$p2=='Exchange Rate'){
        d = d %>% group_by(Year,MonthNum) %>% summarise(Y=mean(Exchange.Rate,na.rm=T)) %>% mutate(X=paste0(Year,'_',MonthNum))
      } else if(input$p2=='Home Demolitions by Israel'){
        d = d %>% group_by(Year,MonthNum) %>% summarise(Y=mean(Demolished.Structures.Daily,na.rm=T)) %>% mutate(X=paste0(Year,'_',MonthNum))
      } else if(input$p2=='Stock Market Index'){
        d = d %>% group_by(Year,MonthNum) %>% summarise(Z=mean(TA125.PX_CLOSE,na.rm=T),Y=mean(PASISI.PX_CLOSE,na.rm=T)) %>% mutate(X=paste0(Year,'_',MonthNum))
      } else if(input$p2=='Temperature'){
        d = d %>% group_by(Year,MonthNum) %>% summarise(Y=mean(TAVG,na.rm=T)) %>% mutate(X=paste0(Year,'_',MonthNum))
      } else if(input$p2=='Rainfall'){
        d = d %>% group_by(Year,MonthNum) %>% summarise(Y=mean(PRCP,na.rm=T)) %>% mutate(X=paste0(Year,'_',MonthNum))
      } else if(input$p2=='Israel-Gaza Crossing (People)'){
        d = d %>% group_by(Year,MonthNum) %>% summarise(Y=mean(Total.Entries.Exits.Gaza.Israel,na.rm=T)) %>% mutate(X=paste0(Year,'_',MonthNum))
      } else if(input$p2=='Israel-Gaza Crossing (Goods)'){
        d = d %>% mutate(Goods = Total.Imports.Gaza.Israel/Total.Exports.Gaza.Israel)
        d$Goods[is.infinite(d$Goods)]=NA
        d = d %>% group_by(Year,MonthNum) %>% summarise(Y=mean(Goods,na.rm=T)) %>% mutate(X=Year)
        #d = d %>% group_by(Year,MonthNum) %>% summarise(Y=mean(Total.Imports.Gaza.Israel/Total.Exports.Gaza.Israel,na.rm=T)) %>% mutate(X=paste0(Year,'_',MonthNum))
      }
    }
    else if(input$graphPeriods=='Quarterly'){
      if(input$p2=='Consumer Price Index'){
        d = d %>% group_by(Year,Quarter) %>% summarise(Z=mean(Israeli.CPI,na.rm=T),Y=mean(Palestinian.CPI,na.rm=T)) %>% mutate(X=paste0(Year,'_',Quarter))
      } else if(input$p2=='Unemployment'){
        d = d %>% group_by(Year,Quarter) %>% summarise(Z=mean(Israeli.UE.Quarterly,na.rm=T),Y=mean(Palestinian.UE.Quarterly,na.rm=T)) %>% mutate(X=paste0(Year,'_',Quarter))
      } else if(input$p2=='Trade Balance'){
        d = d %>% group_by(Year,Quarter) %>% summarise(Z=mean(Israeli.Trade.Balance,na.rm=T),Y=mean(Palestinian.Trade.Balance,na.rm=T)) %>% mutate(X=paste0(Year,'_',Quarter))
      } else if(input$p2=='Exchange Rate'){
        d = d %>% group_by(Year,Quarter) %>% summarise(Y=mean(Exchange.Rate,na.rm=T)) %>% mutate(X=paste0(Year,'_',Quarter))
      } else if(input$p2=='Home Demolitions by Israel'){
        d = d %>% group_by(Year,Quarter) %>% summarise(Y=mean(Demolished.Structures.Daily,na.rm=T)) %>% mutate(X=paste0(Year,'_',Quarter))
      } else if(input$p2=='Stock Market Index'){
        d = d %>% group_by(Year,Quarter) %>% summarise(Z=mean(TA125.PX_CLOSE,na.rm=T),Y=mean(PASISI.PX_CLOSE,na.rm=T)) %>% mutate(X=paste0(Year,'_',Quarter))
      } else if(input$p2=='Temperature'){
        d = d %>% group_by(Year,Quarter) %>% summarise(Y=mean(TAVG,na.rm=T)) %>% mutate(X=paste0(Year,'_',Quarter))
      } else if(input$p2=='Rainfall'){
        d = d %>% group_by(Year,Quarter) %>% summarise(Y=mean(PRCP,na.rm=T)) %>% mutate(X=paste0(Year,'_',Quarter))
      } else if(input$p2=='Israel-Gaza Crossing (People)'){
        d = d %>% group_by(Year,Quarter) %>% summarise(Y=mean(Total.Entries.Exits.Gaza.Israel,na.rm=T)) %>% mutate(X=paste0(Year,'_',Quarter))
      } else if(input$p2=='Israel-Gaza Crossing (Goods)'){
        d = d %>% mutate(Goods = Total.Imports.Gaza.Israel/Total.Exports.Gaza.Israel)
        d$Goods[is.infinite(d$Goods)]=NA
        d = d %>% group_by(Year,Quarter) %>% summarise(Y=mean(Goods,na.rm=T)) %>% mutate(X=Year)
        #d = d %>% group_by(Year,Quarter) %>% summarise(Y=mean(Total.Imports.Gaza.Israel/Total.Exports.Gaza.Israel,na.rm=T)) %>% mutate(X=paste0(Year,'_',Quarter))
      }
    }
    else if(input$graphPeriods=='Weekly'){
      #d = d %>% group_by(Year,Week) %>% summarise(X=mean(Israeli.CPI,na.rm=T),Y=mean(Palestinian.CPI,na.rm=T))
      #d = d %>% group_by(Year,Week) %>% summarise(X=mean(Israeli.UE.Quarterly,na.rm=T),Y=mean(Palestinian.UE.Quarterly,na.rm=T))
      #d = d %>% group_by(Year,Week) %>% summarise(X=mean(Israeli.Trade.Balance,na.rm=T),Y=mean(Palestinian.Trade.Balance,na.rm=T))
      if(input$p2=='Exchange Rate'){
        d = d %>% group_by(Year,Week) %>% summarise(Y=mean(Exchange.Rate,na.rm=T)) %>% mutate(X=paste0(Year,'_',Week))
      } else if(input$p2=='Home Demolitions by Israel'){
        d = d %>% group_by(Year,Week) %>% summarise(Y=mean(Demolished.Structures.Daily,na.rm=T)) %>% mutate(X=paste0(Year,'_',Week))
      } else if(input$p2=='Stock Market Index'){
        d = d %>% group_by(Year,Week) %>% summarise(Z=mean(TA125.PX_CLOSE,na.rm=T),Y=mean(PASISI.PX_CLOSE,na.rm=T)) %>% mutate(X=paste0(Year,'_',Week))
      } else if(input$p2=='Temperature'){
        d = d %>% group_by(Year,Week) %>% summarise(Y=mean(TAVG,na.rm=T)) %>% mutate(X=paste0(Year,'_',Week))
      } else if(input$p2=='Rainfall'){
        d = d %>% group_by(Year,Week) %>% summarise(Y=mean(PRCP,na.rm=T)) %>% mutate(X=paste0(Year,'_',Week))
      } else if(input$p2=='Hamas-Fatah Reconciliation Talks') {
        d = d %>% group_by(Year,Week) %>% summarise(Y=ceiling(mean(hamas_fatah_talks_ongoing,na.rm=T))) %>% mutate(X=paste0(Year,'_',Week))
      } else if(input$p2=='Israeli Operation') {
        d = d %>% group_by(Year,Week) %>% summarise(Y=ceiling(mean(operation_ongoing,na.rm=T))) %>% mutate(X=paste0(Year,'_',Week))
      } else if(input$p2=='US-Israel State Visits') {
        d = d %>% group_by(Year,Week) %>% summarise(Y=ceiling(mean(israel_visit,na.rm=T))) %>% mutate(X=paste0(Year,'_',Week))
      } else if(input$p2=='UN Vote') {
        d = d %>% mutate(un_vote = ifelse(unsc_vote==1 | unga_vote==1,1,0))
        d = d %>% group_by(Year,Week) %>% summarise(Y=ceiling(mean(un_vote,na.rm=T))) %>% mutate(X=paste0(Year,'_',Week))
      } else if(input$p2=='Israeli Coalition Size') {
        d = d %>% group_by(Year,Week) %>% summarise(Y=round(mean(Coalition.Size,na.rm=T))) %>% mutate(X=paste0(Year,'_',Week))
      }
    }
  } else if (input$palestine_or_israel=='Both'){
    
    if(input$graphPeriods=='Annually'){
      if(input$p2=='Consumer Price Index'){
        d = d %>% group_by(Year) %>% summarise(Z=mean(Israeli.CPI,na.rm=T),Y=mean(Palestinian.CPI,na.rm=T)) %>% mutate(X=Year)
      } else if(input$p2=='Unemployment'){
        d = d %>% group_by(Year) %>% summarise(Z=mean(Israeli.UE.Quarterly,na.rm=T),Y=mean(Palestinian.UE.Quarterly,na.rm=T)) %>% mutate(X=Year)
      } else if(input$p2=='Trade Balance'){
        d = d %>% group_by(Year) %>% summarise(Z=mean(Israeli.Trade.Balance,na.rm=T),Y=mean(Palestinian.Trade.Balance,na.rm=T)) %>% mutate(X=Year)
      } else if(input$p2=='Exchange Rate'){
        d = d %>% group_by(Year) %>% summarise(Y=mean(Exchange.Rate,na.rm=T)) %>% mutate(X=Year)
      } else if(input$p2=='Home Demolitions by Israel'){
        d = d %>% group_by(Year) %>% summarise(Y=mean(Demolished.Structures.Daily,na.rm=T)) %>% mutate(X=Year)
      } else if(input$p2=='Stock Market Index'){
        d = d %>% group_by(Year) %>% summarise(Z=mean(TA125.PX_CLOSE,na.rm=T),Y=mean(PASISI.PX_CLOSE,na.rm=T)) %>% mutate(X=Year)
      } else if(input$p2=='Temperature'){
        d = d %>% group_by(Year) %>% summarise(Y=mean(TAVG,na.rm=T)) %>% mutate(X=Year)
      } else if(input$p2=='Rainfall'){
        d = d %>% group_by(Year) %>% summarise(Y=mean(PRCP,na.rm=T)) %>% mutate(X=Year)
      } else if(input$p2=='Israel-Gaza Crossing (People)'){
        d = d %>% group_by(Year) %>% summarise(Y=mean(Total.Entries.Exits.Gaza.Israel,na.rm=T)) %>% mutate(X=Year)
      } else if(input$p2=='Israel-Gaza Crossing (Goods)'){
        d = d %>% mutate(Goods = Total.Imports.Gaza.Israel/Total.Exports.Gaza.Israel)
        d$Goods[is.infinite(d$Goods)]=NA
        d = d %>% group_by(Year) %>% summarise(Y=mean(Goods,na.rm=T)) %>% mutate(X=Year)
        #d = d %>% group_by(Year) %>% summarise(Y=mean(Total.Imports.Gaza.Israel/Total.Exports.Gaza.Israel,na.rm=T)) %>% mutate(X=Year)
      }
    }
    else if(input$graphPeriods=='Monthly'){
      if(input$p2=='Consumer Price Index'){
        d = d %>% group_by(Year,MonthNum) %>% summarise(Z=mean(Israeli.CPI,na.rm=T),Y=mean(Palestinian.CPI,na.rm=T)) %>% mutate(X=paste0(Year,'_',MonthNum))
        #d = d %>% group_by(Year,MonthNum) %>% summarise(X=mean(Israeli.UE.Quarterly,na.rm=T),Y=mean(Palestinian.UE.Quarterly,na.rm=T))
      } else if(input$p2=='Trade Balance'){
        d = d %>% group_by(Year,MonthNum) %>% summarise(Z=mean(Israeli.Trade.Balance,na.rm=T),Y=mean(Palestinian.Trade.Balance,na.rm=T)) %>% mutate(X=paste0(Year,'_',MonthNum))
      } else if(input$p2=='Exchange Rate'){
        d = d %>% group_by(Year,MonthNum) %>% summarise(Y=mean(Exchange.Rate,na.rm=T)) %>% mutate(X=paste0(Year,'_',MonthNum))
      } else if(input$p2=='Home Demolitions by Israel'){
        d = d %>% group_by(Year,MonthNum) %>% summarise(Y=mean(Demolished.Structures.Daily,na.rm=T)) %>% mutate(X=paste0(Year,'_',MonthNum))
      } else if(input$p2=='Stock Market Index'){
        d = d %>% group_by(Year,MonthNum) %>% summarise(Z=mean(TA125.PX_CLOSE,na.rm=T),Y=mean(PASISI.PX_CLOSE,na.rm=T)) %>% mutate(X=paste0(Year,'_',MonthNum))
      } else if(input$p2=='Temperature'){
        d = d %>% group_by(Year,MonthNum) %>% summarise(Y=mean(TAVG,na.rm=T)) %>% mutate(X=paste0(Year,'_',MonthNum))
      } else if(input$p2=='Rainfall'){
        d = d %>% group_by(Year,MonthNum) %>% summarise(Y=mean(PRCP,na.rm=T)) %>% mutate(X=paste0(Year,'_',MonthNum))
      } else if(input$p2=='Israel-Gaza Crossing (People)'){
        d = d %>% group_by(Year,MonthNum) %>% summarise(Y=mean(Total.Entries.Exits.Gaza.Israel,na.rm=T)) %>% mutate(X=paste0(Year,'_',MonthNum))
      } else if(input$p2=='Israel-Gaza Crossing (Goods)'){
        d = d %>% mutate(Goods = Total.Imports.Gaza.Israel/Total.Exports.Gaza.Israel)
        d$Goods[is.infinite(d$Goods)]=NA
        d = d %>% group_by(Year,MonthNum) %>% summarise(Y=mean(Goods,na.rm=T)) %>% mutate(X=Year)
        #d = d %>% group_by(Year,MonthNum) %>% summarise(Y=mean(Total.Imports.Gaza.Israel/Total.Exports.Gaza.Israel,na.rm=T)) %>% mutate(X=paste0(Year,'_',MonthNum))
      }
    }
    else if(input$graphPeriods=='Quarterly'){
      if(input$p2=='Consumer Price Index'){
        d = d %>% group_by(Year,Quarter) %>% summarise(Z=mean(Israeli.CPI,na.rm=T),Y=mean(Palestinian.CPI,na.rm=T)) %>% mutate(X=paste0(Year,'_',Quarter))
      } else if(input$p2=='Unemployment'){
        d = d %>% group_by(Year,Quarter) %>% summarise(Z=mean(Israeli.UE.Quarterly,na.rm=T),Y=mean(Palestinian.UE.Quarterly,na.rm=T)) %>% mutate(X=paste0(Year,'_',Quarter))
      } else if(input$p2=='Trade Balance'){
        d = d %>% group_by(Year,Quarter) %>% summarise(Z=mean(Israeli.Trade.Balance,na.rm=T),Y=mean(Palestinian.Trade.Balance,na.rm=T)) %>% mutate(X=paste0(Year,'_',Quarter))
      } else if(input$p2=='Exchange Rate'){
        d = d %>% group_by(Year,Quarter) %>% summarise(Y=mean(Exchange.Rate,na.rm=T)) %>% mutate(X=paste0(Year,'_',Quarter))
      } else if(input$p2=='Home Demolitions by Israel'){
        d = d %>% group_by(Year,Quarter) %>% summarise(Y=mean(Demolished.Structures.Daily,na.rm=T)) %>% mutate(X=paste0(Year,'_',Quarter))
      } else if(input$p2=='Stock Market Index'){
        d = d %>% group_by(Year,Quarter) %>% summarise(Z=mean(TA125.PX_CLOSE,na.rm=T),Y=mean(PASISI.PX_CLOSE,na.rm=T)) %>% mutate(X=paste0(Year,'_',Quarter))
      } else if(input$p2=='Temperature'){
        d = d %>% group_by(Year,Quarter) %>% summarise(Y=mean(TAVG,na.rm=T)) %>% mutate(X=paste0(Year,'_',Quarter))
      } else if(input$p2=='Rainfall'){
        d = d %>% group_by(Year,Quarter) %>% summarise(Y=mean(PRCP,na.rm=T)) %>% mutate(X=paste0(Year,'_',Quarter))
      } else if(input$p2=='Israel-Gaza Crossing (People)'){
        d = d %>% group_by(Year,Quarter) %>% summarise(Y=mean(Total.Entries.Exits.Gaza.Israel,na.rm=T)) %>% mutate(X=paste0(Year,'_',Quarter))
      } else if(input$p2=='Israel-Gaza Crossing (Goods)'){
        d = d %>% mutate(Goods = Total.Imports.Gaza.Israel/Total.Exports.Gaza.Israel)
        d$Goods[is.infinite(d$Goods)]=NA
        d = d %>% group_by(Year,Quarter) %>% summarise(Y=mean(Goods,na.rm=T)) %>% mutate(X=Year)
        #d = d %>% group_by(Year,Quarter) %>% summarise(Y=mean(Total.Imports.Gaza.Israel/Total.Exports.Gaza.Israel,na.rm=T)) %>% mutate(X=paste0(Year,'_',Quarter))
      }
    }
    else if(input$graphPeriods=='Weekly'){
      #d = d %>% group_by(Year,Week) %>% summarise(X=mean(Israeli.CPI,na.rm=T),Y=mean(Palestinian.CPI,na.rm=T))
      #d = d %>% group_by(Year,Week) %>% summarise(X=mean(Israeli.UE.Quarterly,na.rm=T),Y=mean(Palestinian.UE.Quarterly,na.rm=T))
      #d = d %>% group_by(Year,Week) %>% summarise(X=mean(Israeli.Trade.Balance,na.rm=T),Y=mean(Palestinian.Trade.Balance,na.rm=T))
      if(input$p2=='Exchange Rate'){
        d = d %>% group_by(Year,Week) %>% summarise(Y=mean(Exchange.Rate,na.rm=T)) %>% mutate(X=paste0(Year,'_',Week))
      } else if(input$p2=='Home Demolitions by Israel'){
        d = d %>% group_by(Year,Week) %>% summarise(Y=mean(Demolished.Structures.Daily,na.rm=T)) %>% mutate(X=paste0(Year,'_',Week))
      } else if(input$p2=='Stock Market Index'){
        d = d %>% group_by(Year,Week) %>% summarise(Z=mean(TA125.PX_CLOSE,na.rm=T),Y=mean(PASISI.PX_CLOSE,na.rm=T)) %>% mutate(X=paste0(Year,'_',Week))
      } else if(input$p2=='Temperature'){
        d = d %>% group_by(Year,Week) %>% summarise(Y=mean(TAVG,na.rm=T)) %>% mutate(X=paste0(Year,'_',Week))
      } else if(input$p2=='Rainfall'){
        d = d %>% group_by(Year,Week) %>% summarise(Y=mean(PRCP,na.rm=T)) %>% mutate(X=paste0(Year,'_',Week))
      } else if(input$p2=='Hamas-Fatah Reconciliation Talks') {
        d = d %>% group_by(Year,Week) %>% summarise(Y=ceiling(mean(hamas_fatah_talks_ongoing,na.rm=T))) %>% mutate(X=paste0(Year,'_',Week))
      } else if(input$p2=='Israeli Operation') {
        d = d %>% group_by(Year,Week) %>% summarise(Y=ceiling(mean(operation_ongoing,na.rm=T))) %>% mutate(X=paste0(Year,'_',Week))
      } else if(input$p2=='US-Israel State Visits') {
        d = d %>% group_by(Year,Week) %>% summarise(Y=ceiling(mean(israel_visit,na.rm=T))) %>% mutate(X=paste0(Year,'_',Week))
      } else if(input$p2=='UN Vote') {
        d = d %>% mutate(un_vote = ifelse(unsc_vote==1 | unga_vote==1,1,0))
        d = d %>% group_by(Year,Week) %>% summarise(Y=ceiling(mean(un_vote,na.rm=T))) %>% mutate(X=paste0(Year,'_',Week))
      } else if(input$p2=='Israeli Coalition Size') {
        d = d %>% group_by(Year,Week) %>% summarise(Y=round(mean(Coalition.Size,na.rm=T))) %>% mutate(X=paste0(Year,'_',Week))
      }
    }
  }
  #d = pa %>% group_by(Year) %>% summarise(n=sum(Rocket.Number,na.rm=T)) %>% mutate(X=Year)
  
  #if(input$graphPeriods!='Annually'){d = d %>% ungroup() %>% mutate(X=factor(X,levels=d$X))}
  print(d)
  print(unique(d$X))
  if(input$graphPeriods!='Annually'){d = d %>% ungroup() %>% mutate(X=factor(X,levels=unique(d$X)))}
  if('Z' %in% names(d)){
    d %>% 
      ggplot() + 
      geom_line(aes(x=X,y=Y,group = 1,col='Palestinian'),size=1,alpha=0.5) + geom_point(aes(x=X,y=Y,group = 1,col='Palestinian'),size=1,alpha=1) +
      geom_line(aes(x=X,y=Z,group = 1,col='Israeli'),size=1,alpha=0.5) + geom_point(aes(x=X,y=Z,group = 1,col='Israeli'),size=1,alpha=1) +
      xlab('Time') +
      theme_classic() -> p
  } else {
    d %>% ggplot(aes(x=X,y=Y,group = 1)) + geom_line(size=1,alpha=0.5) + geom_point(size=1,col='black',alpha=1) +
      xlab('Time') +
      theme_classic() -> p
  }
  
  ylabel = labtab$label[labtab$cov_type==input$p2]
  
  p = p + ylab(ylabel)
  
  if(input$graphPeriods=='Annually'){
    p = p + scale_x_continuous(breaks = options$Year) 
  }
  else if(input$graphPeriods=='Monthly'){
    p = p + scale_x_discrete(breaks = paste0(options$Year,'_',1))
  }
  else if(input$graphPeriods=='Quarterly'){
    p = p + scale_x_discrete(breaks = paste0(options$Year,'_',1))
  }
  else if(input$graphPeriods=='Weekly'){
    p = p + scale_x_discrete(breaks = paste0(options$Year,'_',1))
  }
  #p = p + scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))
  p = p + scale_y_continuous(breaks = pretty_breaks())
  
  
  ggplotly(p=p) %>% config(displayModeBar = F)
  
})
