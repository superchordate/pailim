mainplot_data = reactive({

  req(input$actor)
  req(input$graphPeriods)
  req(dataPlot())
  req(input$colorBy)

  # Extract input values. 
  # I set these up here to make it clear which ones are used below, and also to avoid repetitive input$ calls.
  colorBy = input$colorBy
  chooseData = input$chooseData
  casualtyType = input$casualtyType
  actor = input$actor
  graphPeriods = input$graphPeriods
  riot.sub = input$riot.sub
  primary.violence = input$primary.violence
  
  # Filter to original records, exclude rows added to represent additional locations within a single record.
  d = dataPlot() %>% filter(Add == 0)

  # Add Events count (just 1 per row).
  d$Events = 1
  if("Type of Action" %in% colnames(d)) d$Riots = str_detect(d$`Type of Action`, "\\bRiot\\b") * 1

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
      if (colorBy == 'Casualty Type') {
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
  } else if(actor=='Israeli Actions'){
    if(chooseData == 'Detentions') {
      c("Detained.Arrested")
    }

  # Palestine-only Actions
  } else if(actor == "Palestinian Actions") {
    if(chooseData == 'Rockets') {
      c("Rocket.Number")
    } else if(chooseData == 'Incendiary Balloons') {
      c("Balloon.Number")
    } else if(chooseData == 'Riots') {
      c("Riots")
    }
  }

  # Special handling for `Type of Action`.
  if(colorBy=='Type of Action'){

    # Loop over each input$selectedActionTypes selection and create a metric column that can be used later for plotting.
    for(crime in input$selectedActionTypes){

      # Add a column we can sum to count actions. 
      d[[crime]] = str_detect(d$`Type of Action`, paste0("\\b", crime, "\\b")) * d[[sum_cols[1]]]

      # Capture this as a column we need to sum in the upcoming group and sum operation.
      sum_cols = c(sum_cols, crime)

    }

    # Since we have set up all our numeric columns, we only need to group by X.
    group_cols = c("X")    
    sum_cols = sum_cols[-1]  # Remove the first element, which is the original sum_cols (e.g., Casualties, Events, etc.)

  } else {

    if(is.null(sum_cols)) stop(glue("
      mainplot_data: Unhanded case for chooseData: {chooseData}; actor: {actor}
    "))

    # Identify columns we'll be grouping by.
    group_cols = if(colorBy %in% c('None', 'Casualty Type')) {
      c('X')
    } else {
      c('X', colorBy)
    }

  }

  if(chooseData=='Riots' & actor=='Palestinian Actions'){    
    d %<>% filter(
      str_detect(d$`Type of Action`, "\\bRiot\\b"),
      Riot.SubCategory %in% riot.sub
    )
  }

  # Apply grouping and summing. 
  d %<>% 
    group_by(across(all_of(group_cols))) %>% 
    summarise(across(all_of(sum_cols), sum, na.rm = TRUE), .groups = 'drop')

  # Pivot wider on group columns that are not X, to prep these for plotting.
  if(length(setdiff(group_cols, 'X')) > 0){
    d %<>% pivot_wider(
      names_from = setdiff(group_cols, 'X'),
      values_from = all_of(sum_cols),
      values_fill = 0
    )
  }

  return(d)

})

# covariate plot
covariate_data = reactive({
  
  if(is.null(input$selectedCovariates) || input$selectedCovariates == 'None') {
    return(NULL)
  }

  # Extract input values. 
  # I set these up here to make it clear which ones are used below, and also to avoid repetitive input$ calls.
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

  # Return just the columns the user has selected.
  get_cols = if(!is.null(covariate_selection$Z)){
    c('X', covariate_selection$Y, covariate_selection$Z)
  } else {
    c('X', covariate_selection$Y)
  } 

  return(d[, get_cols])

})

output$lineplot = renderUI({
  
  req(mainplot_data())
  
  d = mainplot_data()
  colorBy = input$colorBy

  # add missing periods. 
  d = all_periods[[input$graphPeriods]] %<>% 
    filter(X >= min(d$X) & X <= max(d$X)) %>%
    left_join(d, by = "X")

  # join covariate data.
  if(!is.null(covariate_data())){
    covariate_cols = setdiff(colnames(covariate_data()), "X")
    d = left_join(d, covariate_data(), by = "X")
  } else {
    covariate_cols = c()
  }

  x_title = c(
    `Annually` = 'Year',
    `Monthly` = 'Year_Month',
    `Quarterly` = 'Year_Quarter',
    `Weekly` = 'Year_Week'
  )[[input$graphPeriods]]

  # convert X to numeric. 
  xvals = unique(d$X)
  d$X = as.numeric(factor(d$X, levels = xvals, ordered = TRUE)) - 1

  # Base options for Highcharts
  chart_options = list(
    chart = list(
      type = 'line',
      events = list(
        load = hc_markjs(paste0("function() { var filterElement = document.getElementById('filter-note-line'); if (filterElement) { filterElement.innerHTML = '", filter_description, "'; }}"))
      )
    ),
    title = list(text = ''),
    yAxis = list(
      list(
        title = list(text = input$chooseData)
      ),
      list(
        title = list(text = if(!is.null(covariate_data())) input$selectedCovariates else ""),
        opposite = TRUE
      )
    ),
    xAxis = list(
      title = list(text = x_title),
      type = "categorical",
      categories = xvals,
      endOnTick = FALSE,
      startOnTick = FALSE
    ),
    plotOptions = list(
      line = list(
        dataLabels = list(enabled = FALSE)
      )
    ),
    series = list()
  )
  
  # Add the applicable series. This will include everything that is not X. 
  series = list()
  for(col in setdiff(colnames(d), "X")){
    chart_options$series[[length(chart_options$series) + 1]] = list(
      name = col,
      data = d[[col]],
      dashStyle = if(col %in% covariate_cols) "Dot" else "Solid",
      yAxis = if(col %in% covariate_cols) 1 else 0
    )
  }

  # Enable tooltips with custom format showing X and Y values with labels
  chart_options = hc_enabletooltips(chart_options)
  chart_options$tooltip = list(
    pointFormat = '<b>{point.y}</b> {series.name}',
    useHTML = TRUE
  )

  chart_options$legend = list(
    enabled = length(chart_options$series) > 1,
    align = if(length(chart_options$series) > 3) "left" else "center",
    verticalAlign = if(length(chart_options$series) > 3) "middle" else "bottom",
    layout = if(length(chart_options$series) > 3) "vertical" else "horizontal",
    itemMarginTop = if(length(chart_options$series) > 3) 2 else 10,
    itemMarginBottom = if(length(chart_options$series) > 3) 2 else 10
  )
  
  # Create the chart HTML with a div container
  return(
    hc_html('lineplot', chart_options)
  )
})


