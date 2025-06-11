# main line plot
myplot = reactive({
  req(input$datatype)
  req(input$graphType)
  req(dataPlot())
  req(input$cV)
  #req(input$shade)
  
  d = dataPlot() %>% filter(Add==0)
  
  cV = input$cV
  
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
  
  
  if(input$maptype=='Casualties'){
    
    if(input$casualtype=='All') {
      
      if(cV=='None'){
        if(input$graphType=='Annually'){
          d = d %>% group_by(Year) %>% summarise(n=sum(Casualties,na.rm=T)) %>% mutate(X=Year)
        }
        else if(input$graphType=='Monthly'){
          d = d %>% group_by(Year,MonthNum) %>% summarise(n=sum(Casualties,na.rm=T)) %>% arrange(Year,MonthNum) %>% mutate(X=paste0(Year,'_',MonthNum))
        }
        else if(input$graphType=='Quarterly'){
          d = d %>% group_by(Year,Quarter) %>% summarise(n=sum(Casualties,na.rm=T)) %>% arrange(Year,Quarter) %>% mutate(X=paste0(Year,'_',Quarter))
        }
        else if(input$graphType=='Weekly'){
          d= d %>% group_by(Year,Week) %>% summarise(n=sum(Casualties,na.rm=T)) %>% arrange(Year,Week) %>% mutate(X=paste0(Year,'_',Week))
          #d= d %>% mutate(Xn = 1:n())
        }
      } else if (cV=='Casualty Type') {
        
        if(input$graphType=='Annually'){
          d1 = d %>% group_by(Year) %>% summarise(n=sum(Killed,na.rm=T)) %>% mutate(X=Year)
          d2 = d %>% group_by(Year) %>% summarise(n=sum(Injured,na.rm=T)) %>% mutate(X=Year)
        }
        else if(input$graphType=='Monthly'){
          d1 = d %>% group_by(Year,MonthNum) %>% summarise(n=sum(Killed,na.rm=T)) %>% arrange(Year,MonthNum) %>% mutate(X=paste0(Year,'_',MonthNum))
          d2 = d %>% group_by(Year,MonthNum) %>% summarise(n=sum(Injured,na.rm=T)) %>% arrange(Year,MonthNum) %>% mutate(X=paste0(Year,'_',MonthNum))
        }
        else if(input$graphType=='Quarterly'){
          d1 = d %>% group_by(Year,Quarter) %>% summarise(n=sum(Killed,na.rm=T)) %>% arrange(Year,Quarter) %>% mutate(X=paste0(Year,'_',Quarter))
          d2 = d %>% group_by(Year,Quarter) %>% summarise(n=sum(Injured,na.rm=T)) %>% arrange(Year,Quarter) %>% mutate(X=paste0(Year,'_',Quarter))
        }
        else if(input$graphType=='Weekly'){
          d1 = d %>% group_by(Year,Week) %>% summarise(n=sum(Killed,na.rm=T)) %>% arrange(Year,Week) %>% mutate(X=paste0(Year,'_',Week))
          d2 = d %>% group_by(Year,Week) %>% summarise(n=sum(Injured,na.rm=T)) %>% arrange(Year,Week) %>% mutate(X=paste0(Year,'_',Week))
        }
        
        
      } else {
        if(input$graphType=='Annually'){
          d = d %>% group_by(!!sym(cV),Year) %>% summarise(n=sum(Casualties,na.rm=T)) %>% mutate(X=Year)
        }
        else if(input$graphType=='Monthly'){
          d = d %>% group_by(!!sym(cV),Year,MonthNum) %>% summarise(n=sum(Casualties,na.rm=T)) %>% arrange(Year,MonthNum) %>% mutate(X=paste0(Year,'_',MonthNum))
        }
        else if(input$graphType=='Quarterly'){
          d = d %>% group_by(!!sym(cV),Year,Quarter) %>% summarise(n=sum(Casualties,na.rm=T)) %>% arrange(Year,Quarter) %>% mutate(X=paste0(Year,'_',Quarter))
        }
        else if(input$graphType=='Weekly'){
          d= d %>% group_by(!!sym(cV),Year,Week) %>% summarise(n=sum(Casualties,na.rm=T)) %>% arrange(Year,Week) %>% mutate(X=paste0(Year,'_',Week))
          #d= d %>% mutate(Xn = 1:n())
        }
      }
    }
    else if(input$casualtype=='Killed') {
      #d = d %>% select(Year,MonthNum,Quarter,Week,Killed) %>% distinct()
      #print(d)
      
      if(cV=='None'){
        if(input$graphType=='Annually'){
          d = d %>% group_by(Year) %>% summarise(n=sum(Killed,na.rm=T)) %>% mutate(X=Year)
        }
        else if(input$graphType=='Monthly'){
          d = d %>% group_by(Year,MonthNum) %>% summarise(n=sum(Killed,na.rm=T)) %>% arrange(Year,MonthNum) %>% mutate(X=paste0(Year,'_',MonthNum))
        }
        else if(input$graphType=='Quarterly'){
          d = d %>% group_by(Year,Quarter) %>% summarise(n=sum(Killed,na.rm=T)) %>% arrange(Year,Quarter) %>% mutate(X=paste0(Year,'_',Quarter))
        }
        else if(input$graphType=='Weekly'){
          d= d %>% group_by(Year,Week) %>% summarise(n=sum(Killed,na.rm=T)) %>% arrange(Year,Week) %>% mutate(X=paste0(Year,'_',Week))
        }
      } else {
        if(input$graphType=='Annually'){
          d = d %>% group_by(!!sym(cV),Year) %>% summarise(n=sum(Killed,na.rm=T)) %>% mutate(X=Year)
        }
        else if(input$graphType=='Monthly'){
          d = d %>% group_by(!!sym(cV),Year,MonthNum) %>% summarise(n=sum(Killed,na.rm=T)) %>% arrange(Year,MonthNum) %>% mutate(X=paste0(Year,'_',MonthNum))
        }
        else if(input$graphType=='Quarterly'){
          d = d %>% group_by(!!sym(cV),Year,Quarter) %>% summarise(n=sum(Killed,na.rm=T)) %>% arrange(Year,Quarter) %>% mutate(X=paste0(Year,'_',Quarter))
        }
        else if(input$graphType=='Weekly'){
          d= d %>% group_by(!!sym(cV),Year,Week) %>% summarise(n=sum(Killed,na.rm=T)) %>% arrange(Year,Week) %>% mutate(X=paste0(Year,'_',Week))
        }
      }
    }
    else if(input$casualtype=='Injured') {
      #d = d %>% select(Year,MonthNum,Quarter,Week,Injured) %>% distinct()
      #print(d)
      
      if(cV=='None'){
        if(input$graphType=='Annually'){
          d = d %>% group_by(Year) %>% summarise(n=sum(Injured,na.rm=T)) %>% mutate(X=Year)
        }
        else if(input$graphType=='Monthly'){
          d = d %>% group_by(Year,MonthNum) %>% summarise(n=sum(Injured,na.rm=T)) %>% arrange(Year,MonthNum) %>% mutate(X=paste0(Year,'_',MonthNum))
        }
        else if(input$graphType=='Quarterly'){
          d = d %>% group_by(Year,Quarter) %>% summarise(n=sum(Injured,na.rm=T)) %>% arrange(Year,Quarter) %>% mutate(X=paste0(Year,'_',Quarter))
        }
        else if(input$graphType=='Weekly'){
          d= d %>% group_by(Year,Week) %>% summarise(n=sum(Injured,na.rm=T)) %>% arrange(Year,Week) %>% mutate(X=paste0(Year,'_',Week))
        }
      } else {
        if(input$graphType=='Annually'){
          d = d %>% group_by(!!sym(cV),Year) %>% summarise(n=sum(Injured,na.rm=T)) %>% mutate(X=Year)
        }
        else if(input$graphType=='Monthly'){
          d = d %>% group_by(!!sym(cV),Year,MonthNum) %>% summarise(n=sum(Injured,na.rm=T)) %>% arrange(Year,MonthNum) %>% mutate(X=paste0(Year,'_',MonthNum))
        }
        else if(input$graphType=='Quarterly'){
          d = d %>% group_by(!!sym(cV),Year,Quarter) %>% summarise(n=sum(Injured,na.rm=T)) %>% arrange(Year,Quarter) %>% mutate(X=paste0(Year,'_',Quarter))
        }
        else if(input$graphType=='Weekly'){
          d= d %>% group_by(!!sym(cV),Year,Week) %>% summarise(n=sum(Injured,na.rm=T)) %>% arrange(Year,Week) %>% mutate(X=paste0(Year,'_',Week))
        }
      }
    }
    
    
  } else if (input$maptype=='Events'){
    #d = d %>% select(Year,MonthNum,Quarter,Week) %>% distinct()
    #print(d)
    # d = cm; colorV = 'datatype
    
    if(cV=='None'){
      if(input$graphType=='Annually'){
        d = d %>% group_by(Year) %>% summarise(n=n()) %>% mutate(X=Year)
      }
      else if(input$graphType=='Monthly'){
        d = d %>% group_by(Year,MonthNum) %>% summarise(n=n()) %>% arrange(Year,MonthNum) %>% mutate(X=paste0(Year,'_',MonthNum))
      }
      else if(input$graphType=='Quarterly'){
        d = d %>% group_by(Year,Quarter) %>% summarise(n=n()) %>% arrange(Year,Quarter) %>% mutate(X=paste0(Year,'_',Quarter))
      }
      else if(input$graphType=='Weekly'){
        d= d %>% group_by(Year,Week) %>% summarise(n=n()) %>% arrange(Year,Week) %>% mutate(X=paste0(Year,'_',Week))
      }
    } else {
      
      if(input$graphType=='Annually'){
        #d %>% select(Date,Type.Violence,starts_with('Secondary.Type.Violence')) %>% distinct() %>% print(n=Inf)
        d = d %>% group_by(!!sym(cV),Year) %>% summarise(n=n()) %>% mutate(X=Year)
        #d %>% print(n=Inf)
      }
      else if(input$graphType=='Monthly'){
        d = d %>% group_by(!!sym(cV),Year,MonthNum) %>% summarise(n=n()) %>% arrange(Year,MonthNum) %>% mutate(X=paste0(Year,'_',MonthNum))
      }
      else if(input$graphType=='Quarterly'){
        d = d %>% group_by(!!sym(cV),Year,Quarter) %>% summarise(n=n()) %>% arrange(Year,Quarter) %>% mutate(X=paste0(Year,'_',Quarter))
      }
      else if(input$graphType=='Weekly'){
        d= d %>% group_by(!!sym(cV),Year,Week) %>% summarise(n=n()) %>% arrange(Year,Week) %>% mutate(X=paste0(Year,'_',Week))
      }
      print(d)
      # d = pa;cV='Type.Violence'
    }
    
  }
  
  else if (input$maptype=='Detentions' & input$datatype=='Israeli Actions') {
    #d = d %>% select(Year,MonthNum,Quarter,Week,Detained.Arrested) %>% distinct()
    #print(d)
    
    if(cV=='None'){
      if(input$graphType=='Annually'){
        d = d %>% group_by(Year) %>% summarise(n=sum(Detained.Arrested,na.rm=T)) %>% mutate(X=Year)
      }
      else if(input$graphType=='Monthly'){
        d = d %>% group_by(Year,MonthNum) %>% summarise(n=sum(Detained.Arrested,na.rm=T)) %>% arrange(Year,MonthNum) %>% mutate(X=paste0(Year,'_',MonthNum))
      }
      else if(input$graphType=='Quarterly'){
        d = d %>% group_by(Year,Quarter) %>% summarise(n=sum(Detained.Arrested,na.rm=T)) %>% arrange(Year,Quarter) %>% mutate(X=paste0(Year,'_',Quarter))
      }
      else if(input$graphType=='Weekly'){
        d= d %>% group_by(Year,Week) %>% summarise(n=sum(Detained.Arrested,na.rm=T)) %>% arrange(Year,Week) %>% mutate(X=paste0(Year,'_',Week))
      }
    } else {
      if(input$graphType=='Annually'){
        d = d %>% group_by(!!sym(cV),Year) %>% summarise(n=sum(Detained.Arrested,na.rm=T)) %>% mutate(X=Year)
      }
      else if(input$graphType=='Monthly'){
        d = d %>% group_by(!!sym(cV),Year,MonthNum) %>% summarise(n=sum(Detained.Arrested,na.rm=T)) %>% arrange(Year,MonthNum) %>% mutate(X=paste0(Year,'_',MonthNum))
      }
      else if(input$graphType=='Quarterly'){
        d = d %>% group_by(!!sym(cV),Year,Quarter) %>% summarise(n=sum(Detained.Arrested,na.rm=T)) %>% arrange(Year,Quarter) %>% mutate(X=paste0(Year,'_',Quarter))
      }
      else if(input$graphType=='Weekly'){
        d= d %>% group_by(!!sym(cV),Year,Week) %>% summarise(n=sum(Detained.Arrested,na.rm=T)) %>% arrange(Year,Week) %>% mutate(X=paste0(Year,'_',Week))
      }
    }
    
  }
  
  else if (input$maptype=='Rockets' & input$datatype=='Palestinian Actions') {
    #d = d %>% select(Year,MonthNum,Quarter,Week,Rocket.Number) %>% distinct()
    #print(d)
    
    if(cV=='None'){
      if(input$graphType=='Annually'){
        d = d %>% group_by(Year) %>% summarise(n=sum(Rocket.Number,na.rm=T)) %>% mutate(X=Year)
      }
      else if(input$graphType=='Monthly'){
        d = d %>% group_by(Year,MonthNum) %>% summarise(n=sum(Rocket.Number,na.rm=T)) %>% arrange(Year,MonthNum) %>% mutate(X=paste0(Year,'_',MonthNum))
      }
      else if(input$graphType=='Quarterly'){
        d = d %>% group_by(Year,Quarter) %>% summarise(n=sum(Rocket.Number,na.rm=T)) %>% arrange(Year,Quarter) %>% mutate(X=paste0(Year,'_',Quarter))
      }
      else if(input$graphType=='Weekly'){
        d= d %>% group_by(Year,Week) %>% summarise(n=sum(Rocket.Number,na.rm=T)) %>% arrange(Year,Week) %>% mutate(X=paste0(Year,'_',Week))
      }
    } else {
      if(input$graphType=='Annually'){
        d = d %>% group_by(!!sym(cV),Year) %>% summarise(n=sum(Rocket.Number,na.rm=T)) %>% mutate(X=Year)
      }
      else if(input$graphType=='Monthly'){
        d = d %>% group_by(!!sym(cV),Year,MonthNum) %>% summarise(n=sum(Rocket.Number,na.rm=T)) %>% arrange(Year,MonthNum) %>% mutate(X=paste0(Year,'_',MonthNum))
      }
      else if(input$graphType=='Quarterly'){
        d = d %>% group_by(!!sym(cV),Year,Quarter) %>% summarise(n=sum(Rocket.Number,na.rm=T)) %>% arrange(Year,Quarter) %>% mutate(X=paste0(Year,'_',Quarter))
      }
      else if(input$graphType=='Weekly'){
        d= d %>% group_by(!!sym(cV),Year,Week) %>% summarise(n=sum(Rocket.Number,na.rm=T)) %>% arrange(Year,Week) %>% mutate(X=paste0(Year,'_',Week))
      }
    }
  }
  
  else if(input$maptype=='Incendiary Balloons' & input$datatype=='Palestinian Actions') {
    #d = d %>% select(Year,MonthNum,Quarter,Week,Balloon.Number) %>% distinct()
    #print(d)
    if(cV=='None'){
      if(input$graphType=='Annually'){
        d = d %>% group_by(Year) %>% summarise(n=sum(Balloon.Number,na.rm=T)) %>% mutate(X=Year)
      }
      else if(input$graphType=='Monthly'){
        d = d %>% group_by(Year,MonthNum) %>% summarise(n=sum(Balloon.Number,na.rm=T)) %>% arrange(Year,MonthNum) %>% mutate(X=paste0(Year,'_',MonthNum))
      }
      else if(input$graphType=='Quarterly'){
        d = d %>% group_by(Year,Quarter) %>% summarise(n=sum(Balloon.Number,na.rm=T)) %>% arrange(Year,Quarter) %>% mutate(X=paste0(Year,'_',Quarter))
      }
      else if(input$graphType=='Weekly'){
        d= d %>% group_by(Year,Week) %>% summarise(n=sum(Balloon.Number,na.rm=T)) %>% arrange(Year,Week) %>% mutate(X=paste0(Year,'_',Week))
      }
    } else {
      if(input$graphType=='Annually'){
        d = d %>% group_by(!!sym(cV),Year) %>% summarise(n=sum(Balloon.Number,na.rm=T)) %>% mutate(X=Year)
      }
      else if(input$graphType=='Monthly'){
        d = d %>% group_by(!!sym(cV),Year,MonthNum) %>% summarise(n=sum(Balloon.Number,na.rm=T)) %>% arrange(Year,MonthNum) %>% mutate(X=paste0(Year,'_',MonthNum))
      }
      else if(input$graphType=='Quarterly'){
        d = d %>% group_by(!!sym(cV),Year,Quarter) %>% summarise(n=sum(Balloon.Number,na.rm=T)) %>% arrange(Year,Quarter) %>% mutate(X=paste0(Year,'_',Quarter))
      }
      else if(input$graphType=='Weekly'){
        d= d %>% group_by(!!sym(cV),Year,Week) %>% summarise(n=sum(Balloon.Number,na.rm=T)) %>% arrange(Year,Week) %>% mutate(X=paste0(Year,'_',Week))
      }
    }
  } 
  else if(input$maptype=='Riots' & input$datatype=='Palestinian Actions'){
    # riot.sub = 'Arson'
    # d = d %>% filter(Type.Violence=='Riot'|Secondary.Type.Violence.2=='Riot') %>% filter(Riot.SubCategory %in% riot.sub)
    d = d %>% filter(Type.Violence=='Riot'|Secondary.Type.Violence.2=='Riot') %>% filter(Riot.SubCategory %in% input$riot.sub)
    #d = d %>% select(Year,MonthNum,Quarter,Week) %>% distinct()
    
    if(cV=='None'){
      if(input$graphType=='Annually'){
        d = d %>% group_by(Year) %>% summarise(n=n()) %>% mutate(X=Year)
      }
      else if(input$graphType=='Monthly'){
        d = d %>% group_by(Year,MonthNum) %>% summarise(n=n()) %>% arrange(Year,MonthNum) %>% mutate(X=paste0(Year,'_',MonthNum))
      }
      else if(input$graphType=='Quarterly'){
        d = d %>% group_by(Year,Quarter) %>% summarise(n=n()) %>% arrange(Year,Quarter) %>% mutate(X=paste0(Year,'_',Quarter))
      }
      else if(input$graphType=='Weekly'){
        d= d %>% group_by(Year,Week) %>% summarise(n=n()) %>% arrange(Year,Week) %>% mutate(X=paste0(Year,'_',Week))
      }
    } else {
      if(input$graphType=='Annually'){
        d = d %>% group_by(!!sym(cV),Year) %>% summarise(n=n()) %>% mutate(X=Year)
      }
      else if(input$graphType=='Monthly'){
        d = d %>% group_by(!!sym(cV),Year,MonthNum) %>% summarise(n=n()) %>% arrange(Year,MonthNum) %>% mutate(X=paste0(Year,'_',MonthNum))
      }
      else if(input$graphType=='Quarterly'){
        d = d %>% group_by(!!sym(cV),Year,Quarter) %>% summarise(n=n()) %>% arrange(Year,Quarter) %>% mutate(X=paste0(Year,'_',Quarter))
      }
      else if(input$graphType=='Weekly'){
        d= d %>% group_by(!!sym(cV),Year,Week) %>% summarise(n=n()) %>% arrange(Year,Week) %>% mutate(X=paste0(Year,'_',Week))
        d %>% print(n=Inf)
      }
    }
  }
  
  #d = pa %>% group_by(Year) %>% summarise(n=sum(Rocket.Number,na.rm=T)) %>% mutate(X=Year)
  
  if(cV=='Casualty Type'){
    
    if(input$graphType!='Annually'){
      d1 = d1 %>% ungroup() %>% mutate(X=factor(X,levels=unique(d1$X)))
      d2 = d2 %>% ungroup() %>% mutate(X=factor(X,levels=unique(d2$X)))
    }
    p = ggplot() + 
      geom_line(data=d1,aes(x=X,y=n,group = 1,color='Killed'),size=1,alpha=0.5) + 
      geom_point(data=d1,aes(x=X,y=n,group = 1,color='Killed'),size=1,,alpha=1) +
      geom_line(data=d2,aes(x=X,y=n,group = 1,color='Injured'),size=1,alpha=0.5) + 
      geom_point(data=d2,aes(x=X,y=n,group = 1,color='Injured'),size=1,,alpha=1)
  } else {
    
    if(input$graphType!='Annually'){d = d %>% ungroup() %>% mutate(X=factor(X,levels=unique(d$X)))}
    
    p = d %>% ggplot()
    if(cV=='None') {
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
  
  
  
  
  if(input$graphType=='Annually'){
    p = p + scale_x_continuous(breaks = options$Year) 
  }
  else if(input$graphType=='Monthly'){
    p = p + scale_x_discrete(breaks = paste0(options$Year,'_',1))
  }
  else if(input$graphType=='Quarterly'){
    p = p + scale_x_discrete(breaks = paste0(options$Year,'_',1))
  }
  else if(input$graphType=='Weekly'){
    print('plot weekly')
    #p = p + geom_vline(xintercept=ev,color='#CC79A7',alpha=0.5,size=1)
    p = p + scale_x_discrete(breaks = paste0(options$Year,'_',1))
  }
  p = p + scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))
})


output$myplot = renderPlotly({
  req(myplot())
  p = myplot()
  ggplotly(p=p) %>% config(displayModeBar = F)
  
})
