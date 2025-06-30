mainplot_data = reactive({

  req(input$actor)
  req(input$xAxis)
  req(dataPlot())
  req(input$colorBy)

  # Extract input values. 
  # I set these up here to make it clear which ones are used below, and also to avoid repetitive input$ calls.
  colorBy = input$colorBy
  chooseData = input$chooseData
  casualtyType = input$casualtyType
  actor = input$actor
  xAxis = input$xAxis
  riot.sub = input$riot.sub
  primary.violence = input$primary.violence
  
  # Filter to original records, exclude rows added to represent additional locations within a single record.
  d = dataPlot() %>% filter(Add == 0)

  # Add Events count (just 1 per row).
  d$Events = 1
  if("Type of Action" %in% colnames(d)) d$Riots = str_detect(d$`Type of Action`, "\\bRiot\\b") * 1

  # Set up the X variable based on axis selection.
  d$X = if(xAxis == 'Year') {
    d$Year
  } else if(xAxis == 'Month') {
    paste0(d$Year, '_', pad0(d$MonthNum, 2))
  } else if(xAxis == 'Quarter') {
    paste0(d$Year, '_', d$Quarter)
  } else if(xAxis == 'Week') {
    paste0(d$Year, '_', pad0(d$Week, 2))
  } else if(xAxis == 'District') {
    d$District
  } else if(xAxis == 'Region') {
    d$Region
  } else if(xAxis == 'Area') {
    d$Area
  } else if(xAxis == 'City') {
    d$City
  } else if(xAxis == 'Type of Action') {
    d$`Type of Action`
  } else if(xAxis == 'Perpetrator Origin' && actor == 'Palestinian Actions') {
    d$`Perpetrator Origin`
  } else if(xAxis == 'Perpetrator Type' && actor == 'Israeli Actions') {
    d$`Perpetrator Type`
  } else {
    stop(glue("mainplot_data: Unhandled case for xAxis: {xAxis}"))
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
  selectedCovariates = input$selectedCovariates
  xAxis = input$xAxis
  
  # Filter to original records, exclude rows added to represent additional locations within a single record.
  d = dataPlot() %>% filter(Add == 0)

  # Determine if this is a time-based or geographical chart
  is_time_based = xAxis %in% c('Year', 'Month', 'Quarter', 'Week')

  if(is_time_based) {
    # Set up the time periods for time-based charts
    d$X = if(xAxis == 'Year') {
      d$Year
    } else if(xAxis == 'Month') {
      paste0(d$Year, '_', pad0(d$MonthNum, 2))
    } else if(xAxis == 'Quarter') {
      paste0(d$Year, '_', d$Quarter)
    } else if(xAxis == 'Week') {
      paste0(d$Year, '_', pad0(d$Week, 2))
    } else {
      stop(glue("covariate_data: Unhandled case for xAxis: {xAxis}"))
    }
  } else {
    # For geographical charts, we need to group by both geography and date to get daily averages
    # Use the existing Date column
    
    # Set up the geographical grouping
    d$X = if(xAxis == 'District') {
      d$District
    } else if(xAxis == 'Region') {
      d$Region
    } else if(xAxis == 'Area') {
      d$Area
    } else if(xAxis == 'City') {
      d$City
    } else if(xAxis == 'Type of Action') {
      d$`Type of Action`
    } else if(xAxis == 'Perpetrator Origin') {
      d$`Perpetrator Origin`
    } else if(xAxis == 'Perpetrator Type') {
      d$`Perpetrator Type`
    } else {
      stop(glue("covariate_data: Unhandled geographical case for xAxis: {xAxis}"))
    }
  }

  # Calculate Goods.
  d %<>% mutate(Goods = Total.Imports.Gaza.Israel / Total.Exports.Gaza.Israel)
  d$Goods[is.infinite(d$Goods)] <- NA

  # Convert to data.table for faster grouped operations
  setDT(d)
  
  # Define columns to aggregate
  agg_cols = c(
    'Israeli.CPI', 'Palestinian.CPI', 'Israeli.UE.Quarterly', 'Palestinian.UE.Quarterly',
    'Israeli.Trade.Balance', 'Palestinian.Trade.Balance', 'Exchange.Rate',
    'Demolished.Structures.Daily', 'TA125.PX_CLOSE', 'PASISI.PX_CLOSE',
    'TAVG', 'PRCP', 'Total.Entries.Exits.Gaza.Israel',
    'Goods'
  )
  
  if(is_time_based) {
    # For time-based charts, group by time period
    d = d[, lapply(.SD, mean, na.rm = TRUE), by = X, .SDcols = agg_cols]
  } else {
    # For geographical charts, first group by Date and X to get daily averages per location,
    # then group by X to get overall averages per location
    d = d[, lapply(.SD, mean, na.rm = TRUE), by = .(Date, X), .SDcols = agg_cols]
    d = d[, lapply(.SD, mean, na.rm = TRUE), by = X, .SDcols = agg_cols]
  }
  
  # Convert back to data.frame for compatibility with rest of code
  d = as.data.frame(d)

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

output$chartplot = renderUI({
  
  req(mainplot_data())
  
  d = mainplot_data()
  colorBy = input$colorBy
  xAxis = input$xAxis
  
  # Determine if this is a time-based or geographical chart
  is_time_based = xAxis %in% c('Year', 'Month', 'Quarter', 'Week')
  chart_type = if(is_time_based) 'line' else 'column'

  # For time-based charts, add missing periods and join covariate data
  if(is_time_based) {
    
    # add missing periods. 
    d = all_periods[[xAxis]] %<>% 
      filter(X >= min(d$X) & X <= max(d$X)) %>%
      left_join(d, by = "X")
    
    # join covariate data.
    if(!is.null(covariate_data())){
      covariate_cols = setdiff(colnames(covariate_data()), "X")
      d = left_join(d, covariate_data(), by = "X")
    } else {
      covariate_cols = c()
    }
  } else {
    # For geographical charts, remove rows with missing X values and join covariate data
    d = d %>% filter(!is.na(X) & X != "")
    
    # join covariate data for geographical charts
    if(!is.null(covariate_data())){
      covariate_cols = setdiff(colnames(covariate_data()), "X")
      d = left_join(d, covariate_data(), by = "X")
    } else {
      covariate_cols = c()
    }
  }

  x_title = c(
    `Year` = 'Year',
    `Month` = 'Year_Month',
    `Quarter` = 'Year_Quarter',
    `Week` = 'Year_Week',
    `District` = 'District',
    `Region` = 'Region',
    `Area` = 'Area',
    `City` = 'City',
    `Type of Action` = 'Type of Action',
    `Perpetrator Origin` = 'Perpetrator Origin',
    `Perpetrator Type` = 'Perpetrator Type'
  )[[input$xAxis]]

  # For time-based charts, convert X to numeric, for geographical keep as categorical
  xvals = unique(d$X)
  if(is_time_based) {
    d$X = as.numeric(factor(d$X, levels = xvals, ordered = TRUE)) - 1
  }

  # Base options for Highcharts
  chart_options = list(
    chart = list(
      type = chart_type,
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
      type = if(is_time_based) "categorical" else "category",
      categories = xvals,
      endOnTick = FALSE,
      startOnTick = FALSE
    ),
    plotOptions = list()
  )
  
  # Set plotOptions based on chart type
  if(chart_type == 'line') {
    chart_options$plotOptions$line = list(
      dataLabels = list(enabled = FALSE)
    )
  } else {
    # Check if we should stack columns (when Color By is selected and we have multiple series)
    non_covariate_series = setdiff(colnames(d), c("X", if(!is.null(covariate_data())) setdiff(colnames(covariate_data()), "X") else c()))
    should_stack = colorBy != 'None' && length(non_covariate_series) > 1
    
    chart_options$plotOptions$column = list(
      dataLabels = list(enabled = FALSE),
      stacking = if(should_stack) "normal" else NULL
    )
  }
  
  # Initialize series list
  chart_options$series = list()
  
  # Add the applicable series. This will include everything that is not X. 
  for(col in setdiff(colnames(d), "X")){
    series_data = if(is_time_based) {
      d[[col]]
    } else {
      # For geographical charts, create data points with proper formatting
      data_points = list()
      for(i in 1:nrow(d)) {
        data_points[[i]] = list(name = d$X[i], y = d[[col]][i])
      }
      data_points
    }
    
    # Determine if this series should be stacked
    is_covariate = col %in% covariate_cols
    series_config = list(
      name = col,
      data = series_data,
      dashStyle = if(is_covariate) "Dot" else "Solid",
      yAxis = if(is_covariate) 1 else 0
    )
    
    # Add stack property for non-covariate series in column charts when Color By is selected
    if(chart_type == 'column' && !is_covariate && colorBy != 'None') {
      series_config$stack = 'main'
    }
    
    chart_options$series[[length(chart_options$series) + 1]] = series_config
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
    hc_html('chartplot', chart_options)
  )
})


