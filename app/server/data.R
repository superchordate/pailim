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
  
  if(input$chooseData %in% c('Events','Casualties')){    
    
    # Inputs specific to Palestinian data.
    if(input$actor=='Palestinian Actions'){

      if(length(input$perpetrator.type) > 0) d %<>% filter(Perpetrator.Type %in% input$perpetrator.type)
      if(length(input$region) > 0) d %<>% filter(Region %in% input$region)

    # Inputs specific to Israeli data.    
    } else if(input$actor=='Israeli Actions'){
      
      if(length(input$perpetrator.type) > 0) d %<>% filter(Perpetrator.Type %in% input$perpetrator.type)
      if(length(input$gover) > 0) d %<>% filter(City %in% input$gover)
      
    }
  }

  return(d)

})

