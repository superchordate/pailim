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
  
  d = d() %>% 
    filter(Year %in% input$year) %>% 
    filter(Month %in% input$month)

  # apply `Type of Action` filter.
  # this data field has ;-separated values so filtering is a bit tricky.
  if(
      (length(input$selectedActionTypes) > 0) &&
      (!identical(input$selectedActionTypes, options$`Type of Action`))
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
  }

  # Inputs specific to Palestinian data.
  if(input$actor=='Palestinian Actions'){

    if(length(input$perpetrator.type) > 0) d %<>% filter(Perpetrator.Type %in% input$perpetrator.type)
    if(length(input$region) > 0) d %<>% filter(Region %in% input$region)

  # Inputs specific to Israeli data.    
  } else if(input$actor=='Israeli Actions'){
    
    if(length(input$perpetrator.type) > 0) d %<>% filter(Perpetrator.Type %in% input$perpetrator.type)
    if(length(input$gover) > 0) d %<>% filter(City %in% input$gover)
    
  }

  # Special input$chooseData
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
    d %>% filter(
      str_detect(`Type of Action`, "\\bRiot\\b"),
      Riot.SubCategory %in% input$riot.sub
    )
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

  return(d)

})

