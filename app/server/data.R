# update what data to be used for the map/line graph
d = reactive({
  #req(input$palestine_or_israel)
  switch(input$palestine_or_israel,
          "Both" = cm,
          "Palestinian Actions" = pa,
          "Israeli Actions" = il
  )
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

  # apply Crimes filter.
  # this data field has ;-separated values so filtering is a bit tricky.
  if(
      (length(input$selectedCrimes) > 0) &&
      (!identical(input$selectedCrimes, options$Crimes))
  ) {
      # loop over each value in selectedCrimes and capture the matching indices, then take the union and apply as a filter. 
      indices = lapply(input$selectedCrimes, function(crime) {
          which(str_detect(d$Crimes, paste0("\\b", crime, "\\b")))
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
      
      #print(unique(d$Crimes))
  }
  
  if(input$chooseData %in% c('Events','Casualties')){    
    
    # Inputs specific to Palestinian data.
    if(input$palestine_or_israel=='Palestinian Actions'){

      if(length(input$perpetrator.type) > 0) d %<>% filter(Perpetrator.Type %in% input$perpetrator.type)
      if(length(input$region) > 0) d %<>% filter(Region %in% input$region)

    # Inputs specific to Israeli data.    
    } else if(input$palestine_or_israel=='Israeli Actions'){
      
      if(length(input$perpetrator.type) > 0) d %<>% filter(Perpetrator.Type %in% input$perpetrator.type)
      if(length(input$gover) > 0) d %<>% filter(City %in% input$gover)
      
    }
  }

  return(d)

})

