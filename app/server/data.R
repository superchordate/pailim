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
  req(length(input$year)>0)
  req(length(input$month)>0)
  req(input$chooseData)
  
  # Build filter description and update UI notes
  filter_parts = c()
  
  # Actor filter
  filter_parts = c(filter_parts, paste("Actor:", input$actor))
  
  d = d() %>% 
    filter(Year %in% input$year) %>% 
    filter(Month %in% input$month)
  
  # Year filter description
  if(length(input$year) == length(options$Year)) {
    filter_parts = c(filter_parts, "Years: All")
  } else if(length(input$year) <= 5) {
    filter_parts = c(filter_parts, paste("Years:", paste(input$year, collapse = ", ")))
  } else {
    filter_parts = c(filter_parts, paste("Years:", length(input$year), "selected"))
  }
  
  # Month filter description
  if(length(input$month) == 12) {
    filter_parts = c(filter_parts, "Months: All")
  } else if(length(input$month) <= 4) {
    filter_parts = c(filter_parts, paste("Months:", paste(input$month, collapse = ", ")))
  } else {
    filter_parts = c(filter_parts, paste("Months:", length(input$month), "selected"))
  }

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
      if(length(input$selectedActionTypes) <= 5) {
        filter_parts = c(filter_parts, paste("Action Types:", paste(input$selectedActionTypes, collapse = ", ")))
      } else {
        filter_parts = c(filter_parts, paste("Action Types:", length(input$selectedActionTypes), "selected"))
      }
  }

  # Inputs specific to Palestinian data.
  if(input$actor=='Palestinian Actions'){

    if(length(input$perpetrator.origin) > 0) {
      d %<>% filter(Perpetrator.Origin %in% input$perpetrator.origin)
      # Perpetrator origin filter description
      if(length(input$perpetrator.origin) <= 5) {
        filter_parts = c(filter_parts, paste("Origin:", paste(input$perpetrator.origin, collapse = ", ")))
      } else {
        filter_parts = c(filter_parts, paste("Origin:", length(input$perpetrator.origin), "selected"))
      }
    }
    if(length(input$region) > 0) {
      d %<>% filter(Region %in% input$region)
      # Region filter description
      if(length(input$region) <= 5) {
        filter_parts = c(filter_parts, paste("Region:", paste(input$region, collapse = ", ")))
      } else {
        filter_parts = c(filter_parts, paste("Region:", length(input$region), "selected"))
      }
    }

  # Inputs specific to Israeli data.    
  } else if(input$actor=='Israeli Actions'){
    
    if(length(input$perpetrator.type) > 0) {
      d %<>% filter(Perpetrator.Type %in% input$perpetrator.type)
      # Perpetrator type filter description
      if(length(input$perpetrator.type) <= 5) {
        filter_parts = c(filter_parts, paste("Perpetrator Type:", paste(input$perpetrator.type, collapse = ", ")))
      } else {
        filter_parts = c(filter_parts, paste("Perpetrator Type:", length(input$perpetrator.type), "selected"))
      }
    }
    if(length(input$gover) > 0) {
      d %<>% filter(City %in% input$gover) 
      # Cities filter description
      if(length(input$gover) <= 5) {
        filter_parts = c(filter_parts, paste("Cities:", paste(input$gover, collapse = ", ")))
      } else {
        filter_parts = c(filter_parts, paste("Cities:", length(input$gover), "selected"))
      }
    }
    
  }

  # Special input$chooseData
  # Data type filter description
  data_desc = input$chooseData
  if(input$chooseData == 'Casualties' && !is.null(input$casualtyType)) {
    data_desc = paste(input$chooseData, "-", input$casualtyType)
  }
  filter_parts = c(filter_parts, paste("Data:", data_desc))
  
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
      if(length(input$riot.sub) <= 5) {
        filter_parts = c(filter_parts, paste("Riot Types:", paste(input$riot.sub, collapse = ", ")))
      } else {
        filter_parts = c(filter_parts, paste("Riot Types:", length(input$riot.sub), "selected"))
      }
    }
  }

  # Filter to only columns needed by downstream plots/table to reduce memory usage
  plot_cols = c(
    # Core columns for map plotting
    "Longitude", "Latitude", "Palestine/Israel",
    
    # Temporal columns for line plotting and table
    "Year", "Month", "MonthNum", "Quarter", "Week", "Add",
    
    # Data columns displayed in maps and line plots
    "Casualties", "Killed", "Injured", "Detained.Arrested", 
    "Rocket.Number", "Balloon.Number",
    
    # Action data for filtering and display  
    "Type of Action", "Verbatim.Report",
    
    # Grouping/coloring columns used in line plots
    "Perpetrator.Type", "Region", "City", "Victim.Type",
    "Riot.SubCategory"
  )
  
  # Only keep columns that exist in the data and are needed
  available_cols = intersect(plot_cols, colnames(d))
  d = d %>% select(all_of(available_cols))

  # Create final description and update UI
  filter_description <<- paste(filter_parts, collapse = " | ")

  return(d)

})


