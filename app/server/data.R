filter_description = "Loading.."

# update what data to be used for the map/line graph
d = reactive({
  req(input$actor)
  if(input$actor == 'Both') {
      cm
  } else if(input$actor == 'Palestinian Actions') {
      pa
  } else if(input$actor == 'Israeli Actions') {
      il
  } else {
      stop(glue("Invalid actor selection: {input$actor}"))
  }
})

# data used in map/line graphs
# this will be filterd by year, month, etc ..
dataPlot = reactive({
  
  req(d())
  req(input$chooseData)
  req(input$casualtyType)
  
  # Threshold for showing individual items vs count
  display_threshold = 5
  
  # Build filter description and update UI notes
  filter_parts = c()
  
  # Actor filter
  filter_parts = c(filter_parts, paste("Actor:", input$actor))
  
  d = d() %>% 
    filter(Year %in% input$year) %>% 
    filter(Month %in% input$month)
  
  # Year filter description
  filter_parts = c(filter_parts, format_year_filter(input$year, display_threshold))
  
  # Month filter description
  filter_parts = c(filter_parts, format_month_filter(input$month, display_threshold))

  # apply `Type of Action` filter.
  # this data field has ;-separated values so filtering is a bit tricky.
  if(
      (length(input$selectedActionTypes) > 0) &&
      (!identical(sort(input$selectedActionTypes), available_actions))
  ) {
      # loop over each value in selectedActionTypes and capture the matching indices, then take the union and apply as a filter. 
      indices = lapply(input$selectedActionTypes, function(crime) {
          which(str_detect(d$`Type of Action`, paste0("\\b", crime, "\\b")))
      })
      
      if (length(indices) > 0) {
          # Combine all indices into a single vector
          combined_indices <- unique(unlist(indices))
          # Filter the data frame using the combined indices
          d <- d[combined_indices, ]
      } else {
          # If no indices found, return an empty data frame
          d <- d[0, ]
      }
      
      # Action types filter description
      filter_parts = c(filter_parts, format_generic_filter(input$selectedActionTypes, "Action Types", display_threshold))
  }

  # Filters that apply to both. 
  if(length(input$City) > 0) {
    d %<>% filter(City %in% input$City) 
    # Cities filter description
    filter_parts = c(filter_parts, format_generic_filter(input$City, "Cities", display_threshold))
  }

  if(length(input$District) > 0) {
    d %<>% filter(District %in% input$District) 
    # Districts filter description
    filter_parts = c(filter_parts, format_generic_filter(input$District, "Districts", display_threshold))
  }

  # Inputs specific to Palestinian data.
  if(input$actor=='Palestinian Actions'){

    if(length(input$perpetrator.origin) > 0) {
      d %<>% filter(`Perpetrator Origin` %in% input$perpetrator.origin)
      # Perpetrator origin filter description
      filter_parts = c(filter_parts, format_generic_filter(input$perpetrator.origin, "Origin", display_threshold))
    }
    if(length(input$region) > 0) {
      d %<>% filter(Region %in% input$region)
      # Region filter description
      filter_parts = c(filter_parts, format_generic_filter(input$region, "Region", display_threshold))
    }

  # Inputs specific to Israeli data.    
  } else if(input$actor=='Israeli Actions'){
    
    if(length(input$perpetrator.type) > 0) {
      d %<>% filter(`Perpetrator Type` %in% input$perpetrator.type)
      # Perpetrator type filter description
      filter_parts = c(filter_parts, format_generic_filter(input$perpetrator.type, "Perpetrator Type", display_threshold))
    }
    
  }

  # Special input$chooseData
  # Data type filter description
  data_desc = input$chooseData
  if(input$chooseData == 'Casualties' && !is.null(input$casualtyType)) {
    data_desc = paste(input$chooseData, "-", input$casualtyType)
  }
  filter_parts = c(filter_parts, paste("Data:", data_desc))
  
  # For map tab, if unsupported data type is selected, return empty data
  if(exists('input') && !is.null(input$tab) && input$tab == 'Maps' && 
     input$chooseData %in% c('Rockets', 'Incendiary Balloons')) {
    # Return empty data frame but maintain structure
    d = d[0, ]
    filter_description <<- paste(filter_parts, collapse = " | ")
    return(d)
  }
  
  if(input$chooseData=='Casualties' & input$casualtyType=='All'){
    d = d %>% filter(Casualties > 0)      
  } else if(input$chooseData=='Casualties' & input$casualtyType=='Killed'){
    d = d %>% filter(Killed > 0)
  } else if(input$chooseData=='Casualties' & input$casualtyType=='Injured'){
    d = d %>% filter(Injured > 0)
  } else if(input$chooseData == 'Detentions') {
    d = d %>% filter(Detained.Arrested > 0)
  } else if(input$chooseData == 'Rockets') {
    d = d %>% filter(Rocket.Number > 0)
  } else if(input$chooseData == 'Incendiary Balloons') {
    d = d %>% filter(Balloon.Number > 0)
  } else if(input$chooseData == 'Riots') {
    d = d %>% filter(
      str_detect(`Type of Action`, "\\bRiot\\b"),
      Riot.SubCategory %in% input$riot.sub
    )
    # Riots filter description
    if(!is.null(input$riot.sub) && length(input$riot.sub) > 0) {
      filter_parts = c(filter_parts, format_generic_filter(input$riot.sub, "Riot Types", display_threshold))
    }
  }

  # Filter to only columns needed by downstream plots/table to reduce memory usage
  plot_cols = c(
    # Core columns for map plotting
    "Longitude", "Latitude", "Palestine/Israel",
    
    # Temporal columns for line plotting and table
    "Year", "Month", "MonthNum", "Quarter", "Week", "Add", "Date",
    
    # Data columns displayed in maps and line plots
    "Casualties", "Killed", "Injured", "Detained.Arrested", 
    "Rocket.Number", "Balloon.Number",
    
    # Action data for filtering and display  
    "Type of Action", "Verbatim.Report",
    
    # Grouping/coloring columns used in line plots
    "Perpetrator Type", "Region", "City", "Victim.Type", "Perpetrator Origin", "District", 
    "Riot.SubCategory",
    
    # Covariate columns needed for line plot covariates
    "Total.Imports.Gaza.Israel", "Total.Exports.Gaza.Israel",
    "Israeli.CPI", "Palestinian.CPI", "Israeli.UE.Quarterly", "Palestinian.UE.Quarterly",
    "Israeli.Trade.Balance", "Palestinian.Trade.Balance", "Exchange.Rate",
    "Demolished.Structures.Daily", "TA125.PX_CLOSE", "PASISI.PX_CLOSE",
    "TAVG", "PRCP", "Total.Entries.Exits.Gaza.Israel",
    "Settler.Population", "N.Outposts", "Palestinian.Population",
    "Avg.Daily.Wage", "Crime", "Labor.Participation"
  )
  
  # Only keep columns that exist in the data and are needed
  available_cols = intersect(plot_cols, colnames(d))
  d = d %>% select(all_of(available_cols))

  # Create final description and update UI
  filter_description <<- paste(filter_parts, collapse = " | ")

  return(d)

})


