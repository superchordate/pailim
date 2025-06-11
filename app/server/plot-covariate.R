# covariate plot
covariate_data = reactive({
  
  req(input$palestine_or_israel)
  req(dataPlot())
  req(input$graphPeriods)
  req(input$palestine_or_israel %in% c('Palestinian Actions','Israeli Actions', 'Both'))
  req(input$selectedCovariates)
  req(input$selectedCovariates != 'None')

  # Extract input values. 
  # I set these up here to make it clear which ones are used below, and also to avoid repetitive input$ calls.
  palestine_or_israel = input$palestine_or_israel
  selectedCovariates = input$selectedCovariates
  graphPeriods = input$graphPeriods
  
  # Filter to original records, exclude rows added to represent additional locations within a single record.
  d = dataPlot() %>% filter(Add == 0)

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

  # Calculate Goods.
  d %<>% mutate(Goods = Total.Imports.Gaza.Israel / Total.Exports.Gaza.Israel)
  d$Goods[is.infinite(d$Goods)] <- NA

  # Calculate all the covariates.
  d %<>% 
    summarise(across(all_of(c(
      'Israeli.CPI', 'Palestinian.CPI', 'Israeli.UE.Quarterly', 'Palestinian.UE.Quarterly',
      'Israeli.Trade.Balance', 'Palestinian.Trade.Balance', 'Exchange.Rate',
      'Demolished.Structures.Daily', 'TA125.PX_CLOSE', 'PASISI.PX_CLOSE',
      'TAVG', 'PRCP', 'Total.Entries.Exits.Gaza.Israel',
      'Goods'
    )), mean, na.rm = TRUE), .by = X)

  # Select Z and Y.
  covariate_selection = list(
    `Consumer Price Index` = list(Z = 'Israeli.CPI', Y = 'Palestinian.CPI'),
    `Unemployment` = list(Z = 'Israeli.UE.Quarterly', Y = 'Palestinian.UE.Quarterly'),
    `Trade Balance` = list(Z = 'Israeli.Trade.Balance', Y = 'Palestinian.Trade.Balance'),
    `Exchange Rate` = list(Y = 'Exchange.Rate'),
    `Home Demolitions by Israel` = list(Y = 'Demolished.Structures.Daily'),
    `Stock Market Index` = list(Z = 'TA125.PX_CLOSE', Y = 'PASISI.PX_CLOSE'),
    `Temperature` = list(Y = 'TAVG'),
    `Rainfall` = list(Y = 'PRCP'),
    `Israel-Gaza Crossing (People)` = list(Y = 'Total.Entries.Exits.Gaza.Israel'),
    `Israel-Gaza Crossing (Goods)` = list(Y = 'Goods')
  )[[selectedCovariates]]

  # Create X Y Z dataset.
  d$Y = d[[covariate_selection$Y]]
  if(!is.null(covariate_selection$Z)){
    d$Z = d[[covariate_selection$Z]]
    d %<>% select(X, Y, Z)
  } else {
    d %<>% select(X, Y)
  } 

  return(d)

})

output$covariate_plot = renderPlotly({

  req(covariate_data())
  d = covariate_data()  
  
  # Set the X axis to be a factor with levels in the order they appear in the data.
  # Not necessary for Annual since Year orders properly. 
  if(input$graphPeriods != 'Annually'){
    d %<>% ungroup() %>% mutate(X = factor(X, levels = unique(d$X)))
  }

  if('Z' %in% names(d)){
    d %>% 
      ggplot() + 
      geom_line(aes(x=X,y=Y,group = 1,col='Palestinian'),size=1,alpha=0.5) + 
      geom_point(aes(x=X,y=Y,group = 1,col='Palestinian'),size=1,alpha=1) +
      geom_line(aes(x=X,y=Z,group = 1,col='Israeli'),size=1,alpha=0.5) + 
      geom_point(aes(x=X,y=Z,group = 1,col='Israeli'),size=1,alpha=1) +
      xlab('Time') +
      theme_classic() -> p
  } else {
    d %>% ggplot(aes(x=X,y=Y,group = 1)) + 
      geom_line(size=1,alpha=0.5) + 
      geom_point(size=1,col='black',alpha=1) +
      xlab('Time') +
      theme_classic() -> p
  }    
  
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

  ylabel = labtab$label[labtab$cov_type==input$selectedCovariates]
  
  p = p + ylab(ylabel)
  
  if(input$graphPeriods == 'Annually'){
    p = p + scale_x_continuous(breaks = options$Year) 
  } else {
    p = p + scale_x_discrete(breaks = paste0(options$Year,'_0', 1))
  }
  
  p = p + scale_y_continuous(breaks = pretty_breaks())  
  
  ggplotly(p = p) %>% 
    config(displayModeBar = FALSE)
  
})
