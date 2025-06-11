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
  
  # Filter to original records, exclude rows added to represent additional locations within a single record.
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

output$myplot = renderUI({
  
  req(mainplot_data())
  
  d = mainplot_data()
  cV = input$cV

  x_title = c(
    `Annually` = 'Year',
    `Monthly` = 'Year_Month',
    `Quarterly` = 'Year_Quarter',
    `Weekly` = 'Year_Week'
  )[[input$graphPeriods]]
  
  # Base options for Highcharts
  chart_options = list(
    chart = list(type = 'line'),
    title = list(text = ''),
    yAxis = list(
      title = list(text =  input$chooseData)
    ),
    xAxis = list(
      title = list(text = x_title),
      type = "categorical",
      categories = d$X
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
      data = d[[col]]
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
  return(div(
    id = "myplot",
    style = "height: 400px; width: 100%;",
    hc_html('myplot', chart_options)
  ))
})


