output$myplot = renderPlotly({
  req(myplot())
  p = myplot()
  ggplotly(p=p) %>% config(displayModeBar = F)  
})

mainplot_data = reactive({

  req(input$palestine_or_israel)
  req(input$graphPeriods)
  req(dataPlot())
  req(input$cV)

  cV = input$cV
  
  # filter to original records, exlude rows added to represent additional locations within a single record.
  d = dataPlot() %>% filter(Add == 0)

  if(cV=='Type.Violence'){
    d = d %>% select(Year,MonthNum,Quarter,Week,Date,contains('Type.Violence'),
                      contains("Casualties"),
                      contains("Killed"),
                      contains("Injured"),
                      contains("Detained.Arrested"),
                      contains("Rocket.Number"),
                      contains("Balloon.Number"),
                      contains("Riot.SubCategory"))
    d = d %>% pivot_longer(cols=contains('Type.Violence'),values_to='Type.Violence',values_drop_na = TRUE)
    d = d %>% filter(Type.Violence %in% input$primary.violence)
  }  
  print(glue("graphPeriods: {input$graphPeriods}"))
  if(input$chooseData=='Casualties'){
    
    if(input$casualtype=='All') {
      
      if(cV=='None'){
        if(input$graphPeriods=='Annually'){
          d = d %>% group_by(Year) %>% summarise(n=sum(Casualties,na.rm=T)) %>% mutate(X=Year)
        }
        else if(input$graphPeriods=='Monthly'){
          d = d %>% group_by(Year,MonthNum) %>% summarise(n=sum(Casualties,na.rm=T)) %>% arrange(Year,MonthNum) %>% mutate(X=paste0(Year,'_',MonthNum))
        }
        else if(input$graphPeriods=='Quarterly'){
          d = d %>% group_by(Year,Quarter) %>% summarise(n=sum(Casualties,na.rm=T)) %>% arrange(Year,Quarter) %>% mutate(X=paste0(Year,'_',Quarter))
        }
        else if(input$graphPeriods=='Weekly'){
          d= d %>% group_by(Year,Week) %>% summarise(n=sum(Casualties,na.rm=T)) %>% arrange(Year,Week) %>% mutate(X=paste0(Year,'_',Week))
        }
      } else if (cV=='Casualty Type') {
        
        if(input$graphPeriods=='Annually'){
          d1 = d %>% group_by(Year) %>% summarise(n=sum(Killed,na.rm=T)) %>% mutate(X=Year)
          d2 = d %>% group_by(Year) %>% summarise(n=sum(Injured,na.rm=T)) %>% mutate(X=Year)
        }
        else if(input$graphPeriods=='Monthly'){
          d1 = d %>% group_by(Year,MonthNum) %>% summarise(n=sum(Killed,na.rm=T)) %>% arrange(Year,MonthNum) %>% mutate(X=paste0(Year,'_',MonthNum))
          d2 = d %>% group_by(Year,MonthNum) %>% summarise(n=sum(Injured,na.rm=T)) %>% arrange(Year,MonthNum) %>% mutate(X=paste0(Year,'_',MonthNum))
        }
        else if(input$graphPeriods=='Quarterly'){
          d1 = d %>% group_by(Year,Quarter) %>% summarise(n=sum(Killed,na.rm=T)) %>% arrange(Year,Quarter) %>% mutate(X=paste0(Year,'_',Quarter))
          d2 = d %>% group_by(Year,Quarter) %>% summarise(n=sum(Injured,na.rm=T)) %>% arrange(Year,Quarter) %>% mutate(X=paste0(Year,'_',Quarter))
        }
        else if(input$graphPeriods=='Weekly'){
          d1 = d %>% group_by(Year,Week) %>% summarise(n=sum(Killed,na.rm=T)) %>% arrange(Year,Week) %>% mutate(X=paste0(Year,'_',Week))
          d2 = d %>% group_by(Year,Week) %>% summarise(n=sum(Injured,na.rm=T)) %>% arrange(Year,Week) %>% mutate(X=paste0(Year,'_',Week))
        }
        
        
      } else {
        if(input$graphPeriods=='Annually'){
          d = d %>% group_by(!!sym(cV),Year) %>% summarise(n=sum(Casualties,na.rm=T)) %>% mutate(X=Year)
        }
        else if(input$graphPeriods=='Monthly'){
          d = d %>% group_by(!!sym(cV),Year,MonthNum) %>% summarise(n=sum(Casualties,na.rm=T)) %>% arrange(Year,MonthNum) %>% mutate(X=paste0(Year,'_',MonthNum))
        }
        else if(input$graphPeriods=='Quarterly'){
          d = d %>% group_by(!!sym(cV),Year,Quarter) %>% summarise(n=sum(Casualties,na.rm=T)) %>% arrange(Year,Quarter) %>% mutate(X=paste0(Year,'_',Quarter))
        }
        else if(input$graphPeriods=='Weekly'){
          d= d %>% group_by(!!sym(cV),Year,Week) %>% summarise(n=sum(Casualties,na.rm=T)) %>% arrange(Year,Week) %>% mutate(X=paste0(Year,'_',Week))
        }
      }
    }
    else if(input$casualtype=='Killed') {
      
      if(cV=='None'){
        if(input$graphPeriods=='Annually'){
          d = d %>% group_by(Year) %>% summarise(n=sum(Killed,na.rm=T)) %>% mutate(X=Year)
        }
        else if(input$graphPeriods=='Monthly'){
          d = d %>% group_by(Year,MonthNum) %>% summarise(n=sum(Killed,na.rm=T)) %>% arrange(Year,MonthNum) %>% mutate(X=paste0(Year,'_',MonthNum))
        }
        else if(input$graphPeriods=='Quarterly'){
          d = d %>% group_by(Year,Quarter) %>% summarise(n=sum(Killed,na.rm=T)) %>% arrange(Year,Quarter) %>% mutate(X=paste0(Year,'_',Quarter))
        }
        else if(input$graphPeriods=='Weekly'){
          d= d %>% group_by(Year,Week) %>% summarise(n=sum(Killed,na.rm=T)) %>% arrange(Year,Week) %>% mutate(X=paste0(Year,'_',Week))
        }
      } else {
        if(input$graphPeriods=='Annually'){
          d = d %>% group_by(!!sym(cV),Year) %>% summarise(n=sum(Killed,na.rm=T)) %>% mutate(X=Year)
        }
        else if(input$graphPeriods=='Monthly'){
          d = d %>% group_by(!!sym(cV),Year,MonthNum) %>% summarise(n=sum(Killed,na.rm=T)) %>% arrange(Year,MonthNum) %>% mutate(X=paste0(Year,'_',MonthNum))
        }
        else if(input$graphPeriods=='Quarterly'){
          d = d %>% group_by(!!sym(cV),Year,Quarter) %>% summarise(n=sum(Killed,na.rm=T)) %>% arrange(Year,Quarter) %>% mutate(X=paste0(Year,'_',Quarter))
        }
        else if(input$graphPeriods=='Weekly'){
          d= d %>% group_by(!!sym(cV),Year,Week) %>% summarise(n=sum(Killed,na.rm=T)) %>% arrange(Year,Week) %>% mutate(X=paste0(Year,'_',Week))
        }
      }
    }
    else if(input$casualtype=='Injured') {
      
      if(cV=='None'){
        if(input$graphPeriods=='Annually'){
          d = d %>% group_by(Year) %>% summarise(n=sum(Injured,na.rm=T)) %>% mutate(X=Year)
        }
        else if(input$graphPeriods=='Monthly'){
          d = d %>% group_by(Year,MonthNum) %>% summarise(n=sum(Injured,na.rm=T)) %>% arrange(Year,MonthNum) %>% mutate(X=paste0(Year,'_',MonthNum))
        }
        else if(input$graphPeriods=='Quarterly'){
          d = d %>% group_by(Year,Quarter) %>% summarise(n=sum(Injured,na.rm=T)) %>% arrange(Year,Quarter) %>% mutate(X=paste0(Year,'_',Quarter))
        }
        else if(input$graphPeriods=='Weekly'){
          d= d %>% group_by(Year,Week) %>% summarise(n=sum(Injured,na.rm=T)) %>% arrange(Year,Week) %>% mutate(X=paste0(Year,'_',Week))
        }
      } else {
        if(input$graphPeriods=='Annually'){
          d = d %>% group_by(!!sym(cV),Year) %>% summarise(n=sum(Injured,na.rm=T)) %>% mutate(X=Year)
        }
        else if(input$graphPeriods=='Monthly'){
          d = d %>% group_by(!!sym(cV),Year,MonthNum) %>% summarise(n=sum(Injured,na.rm=T)) %>% arrange(Year,MonthNum) %>% mutate(X=paste0(Year,'_',MonthNum))
        }
        else if(input$graphPeriods=='Quarterly'){
          d = d %>% group_by(!!sym(cV),Year,Quarter) %>% summarise(n=sum(Injured,na.rm=T)) %>% arrange(Year,Quarter) %>% mutate(X=paste0(Year,'_',Quarter))
        }
        else if(input$graphPeriods=='Weekly'){
          d= d %>% group_by(!!sym(cV),Year,Week) %>% summarise(n=sum(Injured,na.rm=T)) %>% arrange(Year,Week) %>% mutate(X=paste0(Year,'_',Week))
        }
      }
    }
    
    
  } else if (input$chooseData=='Events'){
    
    if(cV=='None'){
      if(input$graphPeriods=='Annually'){
        d = d %>% group_by(Year) %>% summarise(n=n()) %>% mutate(X=Year)
      }
      else if(input$graphPeriods=='Monthly'){
        d = d %>% group_by(Year,MonthNum) %>% summarise(n=n()) %>% arrange(Year,MonthNum) %>% mutate(X=paste0(Year,'_',MonthNum))
      }
      else if(input$graphPeriods=='Quarterly'){
        d = d %>% group_by(Year,Quarter) %>% summarise(n=n()) %>% arrange(Year,Quarter) %>% mutate(X=paste0(Year,'_',Quarter))
      }
      else if(input$graphPeriods=='Weekly'){
        d= d %>% group_by(Year,Week) %>% summarise(n=n()) %>% arrange(Year,Week) %>% mutate(X=paste0(Year,'_',Week))
      }
    } else {
      
      if(input$graphPeriods=='Annually'){
        d = d %>% group_by(!!sym(cV),Year) %>% summarise(n=n()) %>% mutate(X=Year)
      }
      else if(input$graphPeriods=='Monthly'){
        d = d %>% group_by(!!sym(cV),Year,MonthNum) %>% summarise(n=n()) %>% arrange(Year,MonthNum) %>% mutate(X=paste0(Year,'_',MonthNum))
      }
      else if(input$graphPeriods=='Quarterly'){
        d = d %>% group_by(!!sym(cV),Year,Quarter) %>% summarise(n=n()) %>% arrange(Year,Quarter) %>% mutate(X=paste0(Year,'_',Quarter))
      }
      else if(input$graphPeriods=='Weekly'){
        d= d %>% group_by(!!sym(cV),Year,Week) %>% summarise(n=n()) %>% arrange(Year,Week) %>% mutate(X=paste0(Year,'_',Week))
      }
    }
    
  }
  
  else if (input$chooseData=='Detentions' & input$palestine_or_israel=='Israeli Actions') {
    
    if(cV=='None'){
      if(input$graphPeriods=='Annually'){
        d = d %>% group_by(Year) %>% summarise(n=sum(Detained.Arrested,na.rm=T)) %>% mutate(X=Year)
      }
      else if(input$graphPeriods=='Monthly'){
        d = d %>% group_by(Year,MonthNum) %>% summarise(n=sum(Detained.Arrested,na.rm=T)) %>% arrange(Year,MonthNum) %>% mutate(X=paste0(Year,'_',MonthNum))
      }
      else if(input$graphPeriods=='Quarterly'){
        d = d %>% group_by(Year,Quarter) %>% summarise(n=sum(Detained.Arrested,na.rm=T)) %>% arrange(Year,Quarter) %>% mutate(X=paste0(Year,'_',Quarter))
      }
      else if(input$graphPeriods=='Weekly'){
        d= d %>% group_by(Year,Week) %>% summarise(n=sum(Detained.Arrested,na.rm=T)) %>% arrange(Year,Week) %>% mutate(X=paste0(Year,'_',Week))
      }
    } else {
      if(input$graphPeriods=='Annually'){
        d = d %>% group_by(!!sym(cV),Year) %>% summarise(n=sum(Detained.Arrested,na.rm=T)) %>% mutate(X=Year)
      }
      else if(input$graphPeriods=='Monthly'){
        d = d %>% group_by(!!sym(cV),Year,MonthNum) %>% summarise(n=sum(Detained.Arrested,na.rm=T)) %>% arrange(Year,MonthNum) %>% mutate(X=paste0(Year,'_',MonthNum))
      }
      else if(input$graphPeriods=='Quarterly'){
        d = d %>% group_by(!!sym(cV),Year,Quarter) %>% summarise(n=sum(Detained.Arrested,na.rm=T)) %>% arrange(Year,Quarter) %>% mutate(X=paste0(Year,'_',Quarter))
      }
      else if(input$graphPeriods=='Weekly'){
        d= d %>% group_by(!!sym(cV),Year,Week) %>% summarise(n=sum(Detained.Arrested,na.rm=T)) %>% arrange(Year,Week) %>% mutate(X=paste0(Year,'_',Week))
      }
    }
    
  }
  
  else if (input$chooseData=='Rockets' & input$palestine_or_israel=='Palestinian Actions') {
    
    if(cV=='None'){
      if(input$graphPeriods=='Annually'){
        d = d %>% group_by(Year) %>% summarise(n=sum(Rocket.Number,na.rm=T)) %>% mutate(X=Year)
      }
      else if(input$graphPeriods=='Monthly'){
        d = d %>% group_by(Year,MonthNum) %>% summarise(n=sum(Rocket.Number,na.rm=T)) %>% arrange(Year,MonthNum) %>% mutate(X=paste0(Year,'_',MonthNum))
      }
      else if(input$graphPeriods=='Quarterly'){
        d = d %>% group_by(Year,Quarter) %>% summarise(n=sum(Rocket.Number,na.rm=T)) %>% arrange(Year,Quarter) %>% mutate(X=paste0(Year,'_',Quarter))
      }
      else if(input$graphPeriods=='Weekly'){
        d= d %>% group_by(Year,Week) %>% summarise(n=sum(Rocket.Number,na.rm=T)) %>% arrange(Year,Week) %>% mutate(X=paste0(Year,'_',Week))
      }
    } else {
      if(input$graphPeriods=='Annually'){
        d = d %>% group_by(!!sym(cV),Year) %>% summarise(n=sum(Rocket.Number,na.rm=T)) %>% mutate(X=Year)
      }
      else if(input$graphPeriods=='Monthly'){
        d = d %>% group_by(!!sym(cV),Year,MonthNum) %>% summarise(n=sum(Rocket.Number,na.rm=T)) %>% arrange(Year,MonthNum) %>% mutate(X=paste0(Year,'_',MonthNum))
      }
      else if(input$graphPeriods=='Quarterly'){
        d = d %>% group_by(!!sym(cV),Year,Quarter) %>% summarise(n=sum(Rocket.Number,na.rm=T)) %>% arrange(Year,Quarter) %>% mutate(X=paste0(Year,'_',Quarter))
      }
      else if(input$graphPeriods=='Weekly'){
        d= d %>% group_by(!!sym(cV),Year,Week) %>% summarise(n=sum(Rocket.Number,na.rm=T)) %>% arrange(Year,Week) %>% mutate(X=paste0(Year,'_',Week))
      }
    }
  }
  
  else if(input$chooseData=='Incendiary Balloons' & input$palestine_or_israel=='Palestinian Actions') {
    
    if(cV=='None'){
      if(input$graphPeriods=='Annually'){
        d = d %>% group_by(Year) %>% summarise(n=sum(Balloon.Number,na.rm=T)) %>% mutate(X=Year)
      }
      else if(input$graphPeriods=='Monthly'){
        d = d %>% group_by(Year,MonthNum) %>% summarise(n=sum(Balloon.Number,na.rm=T)) %>% arrange(Year,MonthNum) %>% mutate(X=paste0(Year,'_',MonthNum))
      }
      else if(input$graphPeriods=='Quarterly'){
        d = d %>% group_by(Year,Quarter) %>% summarise(n=sum(Balloon.Number,na.rm=T)) %>% arrange(Year,Quarter) %>% mutate(X=paste0(Year,'_',Quarter))
      }
      else if(input$graphPeriods=='Weekly'){
        d= d %>% group_by(Year,Week) %>% summarise(n=sum(Balloon.Number,na.rm=T)) %>% arrange(Year,Week) %>% mutate(X=paste0(Year,'_',Week))
      }
    } else {
      if(input$graphPeriods=='Annually'){
        d = d %>% group_by(!!sym(cV),Year) %>% summarise(n=sum(Balloon.Number,na.rm=T)) %>% mutate(X=Year)
      }
      else if(input$graphPeriods=='Monthly'){
        d = d %>% group_by(!!sym(cV),Year,MonthNum) %>% summarise(n=sum(Balloon.Number,na.rm=T)) %>% arrange(Year,MonthNum) %>% mutate(X=paste0(Year,'_',MonthNum))
      }
      else if(input$graphPeriods=='Quarterly'){
        d = d %>% group_by(!!sym(cV),Year,Quarter) %>% summarise(n=sum(Balloon.Number,na.rm=T)) %>% arrange(Year,Quarter) %>% mutate(X=paste0(Year,'_',Quarter))
      }
      else if(input$graphPeriods=='Weekly'){
        d= d %>% group_by(!!sym(cV),Year,Week) %>% summarise(n=sum(Balloon.Number,na.rm=T)) %>% arrange(Year,Week) %>% mutate(X=paste0(Year,'_',Week))
      }
    }
  } 
  else if(input$chooseData=='Riots' & input$palestine_or_israel=='Palestinian Actions'){
    
    d = d %>% filter(Type.Violence=='Riot'|Secondary.Type.Violence.2=='Riot') %>% filter(Riot.SubCategory %in% input$riot.sub)
    
    if(cV=='None'){
      if(input$graphPeriods=='Annually'){
        d = d %>% group_by(Year) %>% summarise(n=n()) %>% mutate(X=Year)
      }
      else if(input$graphPeriods=='Monthly'){
        d = d %>% group_by(Year,MonthNum) %>% summarise(n=n()) %>% arrange(Year,MonthNum) %>% mutate(X=paste0(Year,'_',MonthNum))
      }
      else if(input$graphPeriods=='Quarterly'){
        d = d %>% group_by(Year,Quarter) %>% summarise(n=n()) %>% arrange(Year,Quarter) %>% mutate(X=paste0(Year,'_',Quarter))
      }
      else if(input$graphPeriods=='Weekly'){
        d= d %>% group_by(Year,Week) %>% summarise(n=n()) %>% arrange(Year,Week) %>% mutate(X=paste0(Year,'_',Week))
      }
    } else {
      if(input$graphPeriods=='Annually'){
        d = d %>% group_by(!!sym(cV),Year) %>% summarise(n=n()) %>% mutate(X=Year)
      }
      else if(input$graphPeriods=='Monthly'){
        d = d %>% group_by(!!sym(cV),Year,MonthNum) %>% summarise(n=n()) %>% arrange(Year,MonthNum) %>% mutate(X=paste0(Year,'_',MonthNum))
      }
      else if(input$graphPeriods=='Quarterly'){
        d = d %>% group_by(!!sym(cV),Year,Quarter) %>% summarise(n=n()) %>% arrange(Year,Quarter) %>% mutate(X=paste0(Year,'_',Quarter))
      }
      else if(input$graphPeriods=='Weekly'){
        d= d %>% group_by(!!sym(cV),Year,Week) %>% summarise(n=n()) %>% arrange(Year,Week) %>% mutate(X=paste0(Year,'_',Week))
      }
    }
  }

  # return data.
  if(exists('d2')){
    return(list(cV = cV, d1 = d1, d2 = d2))
  } else {
    return(list(cV = cV, d = d))
  }

})

# main line plot
myplot = reactive({

  req(mainplot_data())

  d1 = mainplot_data()$d1
  d2 = mainplot_data()$d2
  d = mainplot_data()$d  
  cV = mainplot_data()$cV
  
  # if input$graphPeriods != Annually, the time frame will be added to the data as a column
  #  and X will expand to show the period number. 
  # for example, if graphPeriods == "Monthly" and cV == "None" then d is:
  # A tibble: 54 × 4
  # Groups:   Year [5]
  # Year MonthNum     n X      
  # <int>    <dbl> <int> <chr>  
  # 1  2019        1   277 2019_1 
  # 2  2019        2   266 2019_2 
  # 3  2019        3   469 2019_3 
  # 4  2019        4   179 2019_4 
  # 5  2019        5   521 2019_5 
  # 6  2019        6   289 2019_6 
  # 7  2019        7   306 2019_7 
  # 8  2019        8   418 2019_8 
  # 9  2019        9   360 2019_9 
  # 10  2019       10   396 2019_10
  
  if(cV=='Casualty Type'){ # Casualty Type is only an option if chooseData (Choose Datae) == "Casualties".
  
    # if cv == "Casualty Type", then d1 and d2 are defined (d is NULL) and like:
    # A tibble: 5 × 3
    # Year     n     X
    # <int> <int> <int>
    # 1  2019   173  2019
    # 2  2020    37  2020
    # 3  2021   162  2021
    # 4  2022   221  2022
    # 5  2023   196  2023  
    
    if(input$graphPeriods!='Annually'){
      d1 = d1 %>% ungroup() %>% mutate(X=factor(X,levels=unique(d1$X)))
      d2 = d2 %>% ungroup() %>% mutate(X=factor(X,levels=unique(d2$X)))
    }

    p = ggplot() + 
      geom_line(data=d1,aes(x=X,y=n,group = 1,color='Killed'),size=1,alpha=0.5) + 
      geom_point(data=d1,aes(x=X,y=n,group = 1,color='Killed'),size=1,,alpha=1) +
      geom_line(data=d2,aes(x=X,y=n,group = 1,color='Injured'),size=1,alpha=0.5) + 
      geom_point(data=d2,aes(x=X,y=n,group = 1,color='Injured'),size=1,,alpha=1)

  } else {
  
    # d example if cV == "Type.Violence":
    # A tibble: 119 × 4
    # Groups:   Type.Violence [25]
    # Type.Violence  Year     n     X
    # <chr>         <int> <int> <int>
    #   1 Air Patrol     2020     0  2020
    # 2 Air Patrol     2021     0  2021
    # 3 Air Patrol     2022     0  2022
    # 4 Air Patrol     2023     0  2023
    # 5 Air Raid       2019   274  2019
    # 6 Air Raid       2020    17  2020
    # 7 Air Raid       2021   167  2021
    # 8 Air Raid       2022   269  2022
    # 9 Air Raid       2023    85  2023
    # 10 Assault        2019   261  2019
    # # ℹ 109 more rows
    # # ℹ Use `print(n = ...)` to see more rows
    
    # d example if cV == "palestine_or_israel":
    # A tibble: 10 × 4
    # Groups:   palestine_or_israel [2]
    # palestine_or_israel   Year     n     X
    # <fct>     <int> <int> <int>
    #   1 Israel     2019  3825  2019
    # 2 Israel     2020  1196  2020
    # 3 Israel     2021  4775  2021
    # 4 Israel     2022  3875  2022
    # 5 Israel     2023  1903  2023
    # 6 Palestine  2019   198  2019
    # 7 Palestine  2020   116  2020
    # 8 Palestine  2021   385  2021
    # 9 Palestine  2022   308  2022
    # 10 Palestine  2023   200  2023
    
    if(input$graphPeriods!='Annually'){d = d %>% ungroup() %>% mutate(X=factor(X,levels=unique(d$X)))}
    
    p = d %>% ggplot()
    
    if(cV=='None') {
  
      # d example if cV == "None":
      # Year     n     X
      # <int> <int> <int>
      # 1  2019 15010  2019
      # 2  2020 14073  2020
      # 3  2021 16421  2021
      # 4  2022 20823  2022
      # 5  2023 10943  2023

      p = p + geom_line(aes(x=X,y=n,group = 1),size=1,alpha=0.5) + geom_point(aes(x=X,y=n,group = 1),size=1,fill='black',alpha=1) 
    } else {
      p = p + 
        geom_line(aes(x=X,y=n,group = 1,color=!!sym(cV)),size=1,alpha=0.5) + 
        geom_point(aes(x=X,y=n,group = 1,color=!!sym(cV)),size=1,,alpha=1)
    }
    
  }
  
  p = p +
    ylab('Frequency') +
    xlab('Time') +
    theme_classic()
  
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
  p = p + scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))
})


