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

  # Extract input values. 
  # I set these up here to make it clear which ones are used below, and also to avoid repetitive input$ calls.
  cV = input$cV
  chooseData = input$chooseData
  casualtyType = input$casualtyType
  palestine_or_israel = input$palestine_or_israel
  graphPeriods = input$graphPeriods
  riot.sub = input$riot.sub
  primary.violence = input$primary.violence
  
  # Filter to original records, exlude rows added to represent additional locations within a single record.
  d = dataPlot() %>% filter(Add == 0)

  # Add Events count (just 1 per row).
  d$Events = 1

  # Set up the periods.
  d$X = if(graphPeriods == 'Annually') {
    d$Year
  } else if(graphPeriods == 'Monthly') {
    paste0(d$Year, '_', pad0(d$MonthNum, 2))
  } else if(graphPeriods == 'Quarterly') {
    paste0(d$Year, '_', d$Quarter)
  } else if(graphPeriods == 'Weekly') {
    paste0(d$Year, '_', pad0(d$Week, 2))
  } else {
    stop(glue("mainplot_data: Unhanded case for graphPeriods: {graphPeriods}"))
  }  

  # Identify the column(s) we'll be summing.
  sum_cols = if(chooseData == 'Casualties'){
    if(casualtyType == 'All'){
      if (cV == 'Casualty Type') {
        c("Killed", "Injured")
      } else {
        c("Casualties")
      }
    } else if(casualtyType == 'Killed') {
      c("Killed")
    } else if(casualtyType == 'Injured') {
      c("Injured")
    }
  } else if(chooseData == 'Events'){
    c("Events")  

  # Israeli-only Actions
  } else if(palestine_or_israel=='Israeli Actions'){
    if(chooseData == 'Detentions') {
      c("Detained.Arrested")
    }

  # Palestine-only Actions
  } else if(palestine_or_israel == "Palestinian Actions") {
    if(chooseData == 'Rockets') {
      c("Rocket.Number")
    } else if(chooseData == 'Incendiary Balloons') {
      c("Balloon.Number")
    } else if(chooseData == 'Riots') {
      c("Events") # Riots are counted via filtering instead of summing.
    }
  }

  if(is.null(sum_cols)) stop(glue("
    mainplot_data: Unhanded case for chooseData: {chooseData}; palestine_or_israel: {palestine_or_israel}
  "))

  # Identify columns we'll be grouping by.
  group_cols = if(cV %in% c('None', 'Casualty Type')) {
    c('X')
  } else {
    c('X', cV)
  }

  # Apply data transformations.
  if(cV=='Type.Violence'){
    
    d %<>% select(
      X,
      contains('Type.Violence'),
      contains("Casualties"),
      contains("Killed"),
      contains("Injured"),
      contains("Events"),
      contains("Detained.Arrested"),
      contains("Rocket.Number"),
      contains("Balloon.Number"),
      contains("Riot.SubCategory")
    )

    d %<>% pivot_longer(cols = contains('Type.Violence'), values_to = 'Type.Violence', values_drop_na = TRUE)

    d %<>% filter(Type.Violence %in% primary.violence)

  }

  if(chooseData=='Riots' & palestine_or_israel=='Palestinian Actions'){    
    d %<>% filter(
      Type.Violence=='Riot' | Secondary.Type.Violence.2=='Riot',
      Riot.SubCategory %in% riot.sub
    )

  }

  # Apply grouping and summing. 
  d %<>% 
    group_by(across(all_of(group_cols))) %>% 
    summarise(across(all_of(sum_cols), sum, na.rm = TRUE), .groups = 'drop')

  return(d)

})

myplot = reactive({

  req(mainplot_data())

  d = mainplot_data()
  cV = input$cV
  
  # Set the X axis to be a factor with levels in the order they appear in the data.
  # Not necessary for Annual since Year orders properly. 
  if(input$graphPeriods != 'Annually'){
    d %<>% ungroup() %>% mutate(X = factor(X, levels = unique(d$X)))
  }
  
  if(cV=='Casualty Type'){ # Casualty Type is only an option if chooseData == "Casualties".

    p = ggplot() + 
      geom_line(data=d,aes(x=X,y=n,group = 1,color='Killed'),size=1,alpha=0.5) + 
      geom_point(data=d,aes(x=X,y=n,group = 1,color='Killed'),size=1,,alpha=1) +
      geom_line(data=d,aes(x=X,y=n,group = 1,color='Injured'),size=1,alpha=0.5) + 
      geom_point(data=d,aes(x=X,y=n,group = 1,color='Injured'),size=1,,alpha=1)

  } else {
    
    p = d %>% ggplot()
    
    if(cV=='None') {

      p = p + 
        geom_line(
          aes(x = X, y = Events, group = 1), 
          size = 1, alpha = 0.5
        ) + 
        geom_point(
          aes(x = X,y = Events, group = 1),
          size = 1 ,
          fill = 'black',
          alpha = 1
        ) 

    } else {

      p = p + 
        geom_line(aes(x=X,y=Events,group = 1,color=!!sym(cV)),size=1,alpha=0.5) + 
        geom_point(aes(x=X,y=Events,group = 1,color=!!sym(cV)),size=1,alpha=1)
    }
    
  }
  
  p = p +
    ylab('Frequency') +
    xlab('Time') +
    theme_classic()
  
  if(input$graphPeriods == 'Annually'){
    p = p + scale_x_continuous(breaks = options$Year) 
  } else {
    p = p + scale_x_discrete(breaks = paste0(options$Year,'_0', 1))
  }
  
  p = p + scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))

  return(p)

})


