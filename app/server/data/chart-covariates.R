# This function handles calculation of covariate metrics in the Chart tab. 
# It returns covariate data at the same X-value level as the main plot data, so it can be joined easily.
covariate_data = reactive({
  
  if(is.null(input$selectedCovariates) || input$selectedCovariates == 'None') {
    return(NULL)
  }

  # Extract input values. 
  selectedCovariates = input$selectedCovariates
  xAxis = input$xAxis

  # Determine if this is a time-based or geographical chart
  is_time_based = xAxis %in% c('Year', 'Month', 'Quarter', 'Week')

  # Data differs between time-based and geographical-based covariates.
  if(is_time_based) {

    d = time_covariates_filtered()

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
  
    # Define columns to aggregate
    agg_cols = c(
        'Israeli.CPI', 'Palestinian.CPI', 'Israeli.UE.Quarterly', 'Palestinian.UE.Quarterly',
        'Israeli.Trade.Balance', 'Palestinian.Trade.Balance', 'Exchange.Rate',
        'Demolished.Structures.Daily', 'TA125.PX_CLOSE', 'PASISI.PX_CLOSE',
        'TAVG', 'PRCP', 'Total.Entries.Exits.Gaza.Israel',
        'Settler.Population', 'N.Outposts', 'Palestinian.Population',
        'Avg.Daily.Wage', 'Crime', 'Labor.Participation',
        # Goods will be added later since it is a ratio and must be weighted properly.
        # To calculate Goods:
        'Total.Imports.Gaza.Israel', 'Total.Exports.Gaza.Israel'
    )

  } else {

    d = geo_covariates_filtered()
    
    # Geographical data is only available by district. 
    d$X = d$District

    agg_cols = c(
        "Settler.Population", "N.Outposts", "Palestinian.Population",
        "Avg.Daily.Wage", "Crime", "Labor.Participation"
    )

  }

  # Convert to data.table for faster grouped operations
  setDT(d)
  
  # Only use columns that actually exist in the data
  agg_cols = intersect(agg_cols, colnames(d))
  
  # Aggregate to the X level.
  d = d[, lapply(.SD, mean, na.rm = TRUE), by = X, .SDcols = agg_cols]

  # Convert NaN to NA since Highcharts can't handle NaN values.
  d %<>% mutate(across(where(is.numeric), ~ ifelse(is.nan(.x), NA, .x)))

  # Calculate Goods.
  if(is_time_based) d %<>% mutate(
    Goods = fifelse(
        Total.Exports.Gaza.Israel == 0, NA, 
        Total.Imports.Gaza.Israel / Total.Exports.Gaza.Israel
    )
  ) %>% select(-c(Total.Imports.Gaza.Israel, Total.Exports.Gaza.Israel))

  # Convert back to data.frame for compatibility with rest of code
  d = as.data.frame(d)

  # Select Z and Y.
  covariate_selection = list(
    # time-varying covariates.
    `Consumer Price Index` = list(Z = 'Israeli.CPI', Y = 'Palestinian.CPI'),
    `Unemployment` = list(Z = 'Israeli.UE.Quarterly', Y = 'Palestinian.UE.Quarterly'),
    `Trade Balance` = list(Z = 'Israeli.Trade.Balance', Y = 'Palestinian.Trade.Balance'),
    `Exchange Rate` = list(Y = 'Exchange.Rate'),
    `Home Demolitions by Israel` = list(Y = 'Demolished.Structures.Daily'),
    `Stock Market Index` = list(Z = 'TA125.PX_CLOSE', Y = 'PASISI.PX_CLOSE'),
    `Temperature` = list(Y = 'TAVG'),
    `Rainfall` = list(Y = 'PRCP'),
    `Israel-Gaza Crossing (People)` = list(Y = 'Total.Entries.Exits.Gaza.Israel'),
    `Israel-Gaza Crossing (Goods)` = list(Y = 'Goods'),
    `Settler Population` = list(Y = 'Settler.Population'),
    `Number of Outposts` = list(Y = 'N.Outposts'),
    `Palestinian Population` = list(Y = 'Palestinian.Population'),
    `Average Daily Wage` = list(Y = 'Avg.Daily.Wage'),
    `Crime` = list(Y = 'Crime'),
    `Labor Participation` = list(Y = 'Labor.Participation'),
    # geographic covariates.
    `Settler.Population` = list(Y = 'Settler.Population'),
    `N.Outposts` = list(Y = 'N.Outposts'),
    `Palestinian.Population` = list(Y = 'Palestinian.Population'),
    `Avg.Daily.Wage` = list(Y = 'Avg.Daily.Wage'),
    `Crime` = list(Y = 'Crime'),
    `Labor.Participation` = list(Y = 'Labor.Participation')
  )[[selectedCovariates]]

  # Return just the columns the user has selected.
  get_cols = if(!is.null(covariate_selection$Z)){
    c('X', covariate_selection$Y, covariate_selection$Z)
  } else {
    c('X', covariate_selection$Y)
  }

  return(d[, get_cols])

})
