# update what data to be used for the map/line graph
d = reactive({
  #req(input$datatype)
  switch(input$datatype,
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
  
  d = d() %>% 
    filter(Year %in% input$year) %>% 
    filter(Month %in% input$month)
  
  if(input$maptype %in% c('Events','Casualties')){
    
    if('Secondary.Type.Violence.3'%in%names(d)){
      d = d %>% filter(Type.Violence %in% input$primary.violence | 
                          Secondary.Type.Violence.1 %in% input$primary.violence |
                          Secondary.Type.Violence.2 %in% input$primary.violence |
                          Secondary.Type.Violence.3 %in% input$primary.violence |
                          Secondary.Type.Violence.4 %in% input$primary.violence |
                          Secondary.Type.Violence.5 %in% input$primary.violence |
                          Secondary.Type.Violence.6 %in% input$primary.violence |
                          Secondary.Type.Violence.7 %in% input$primary.violence |
                          Secondary.Type.Violence.8 %in% input$primary.violence)
    } else {
      d = d %>% filter(Type.Violence %in% input$primary.violence | 
                          Secondary.Type.Violence.1 %in% input$primary.violence |
                          Secondary.Type.Violence.2 %in% input$primary.violence)
    }
    
    
    # Inputs specific to Palestinian data.
    if(input$datatype=='Palestinian Actions'){
      
      req(length(input$perpetrator.origin)>0)
      
      d = d %>% 
        filter(Perpetrator.Origin %in% input$perpetrator.origin) %>%
        filter(Region %in% input$region)

    # Inputs specific to Israeli data.    
    } else if(input$datatype=='Israeli Actions'){
      
      if(length(input$perpetrator.type) > 0) d %<>% filter(Perpetrator.Type %in% input$perpetrator.type)
      if(length(input$gover) > 0) d %<>% filter(City %in% input$gover)

      # apply Combined_Crimes filter.
      # this data field has ;-separated values so filtering is a bit tricky.
      if(
          (length(input$Combined_Crimes) > 0) &&
          (!identical(input$Combined_Crimes, options$Combined_Crimes))
      ) {
          # loop over each value in Combined_Crimes and capture the matching indices, then take the union and apply as a filter. 
          indices = lapply(input$Combined_Crimes, function(crime) {
              which(str_detect(d$Combined_Crimes, paste0("\\b", crime, "\\b")))
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
          
          #print(unique(d$Combined_Crimes))
      }
      
    }
  }
  d
  
})

