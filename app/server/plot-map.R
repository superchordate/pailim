# This file contains the code for the map.

map_available_actions = '' # save a server-level object for immediate checking of status. 
map_init = FALSE

output$mymap = renderLeaflet({
  
  req(d())
  req(dataPlot())
  
  # Don't render map for data types that are not supported
  if(input$chooseData %in% c('Rockets', 'Incendiary Balloons')) {
    return(NULL)
  }
  
  d = dataPlot()
  
  # check if we are in the middle of an options change, to prevent loading the map twice.  
  if(map_init && !identical(map_available_actions, available_actions)){
    map_available_actions <<- available_actions
    return(NULL)
  }
  if(!map_init){
    map_available_actions <<- available_actions
    map_init <<- TRUE
  }
  
  if(input$chooseData=='Events'){

    leaflet(data = d) %>% addProviderTiles(providers$Stadia.StamenTonerLite,options = providerTileOptions(noWrap = TRUE)) %>% 
      addMarkers(lng=~Longitude,lat=~Latitude,
                  icon = ~countries[`Palestine/Israel`],
                  #popup = ~as.character(Verbatim.Report), label = ~as.character(Verbatim.Report),
                  clusterOptions = markerClusterOptions()) -> p
                  
  }
  
  else if(input$chooseData=='Casualties' & input$casualtyType=='All'){
    
    leaflet(data = d) %>% addProviderTiles(providers$Stadia.StamenTonerLite,options = providerTileOptions(noWrap = TRUE)) %>% 
      addMarkers(
        lng=~Longitude,lat=~Latitude,
        icon = ~countries[`Palestine/Israel`],
        #popup = ~as.character(Casualties), label = ~as.character(Casualties),
        clusterOptions = markerClusterOptions()
      ) -> p
    
  }
  
  else if(input$chooseData=='Casualties' & input$casualtyType=='Killed'){
    
    leaflet(data = d) %>% addProviderTiles(providers$Stadia.StamenTonerLite,options = providerTileOptions(noWrap = TRUE)) %>% 
      addMarkers(
        lng=~Longitude,lat=~Latitude,
        icon = ~countries[`Palestine/Israel`],
        popup = ~as.character(Killed), label = ~as.character(Killed),
        clusterOptions = markerClusterOptions()
      ) -> p
  }
  
  else if(input$chooseData=='Casualties' & input$casualtyType=='Injured'){
    
    leaflet(data = d) %>% addProviderTiles(providers$Stadia.StamenTonerLite,options = providerTileOptions(noWrap = TRUE)) %>% 
      addMarkers(
        lng=~Longitude,lat=~Latitude,
        icon = ~countries[`Palestine/Israel`],
        popup = ~as.character(Injured), label = ~as.character(Injured),
        clusterOptions = markerClusterOptions()
      ) -> p
  }
  
  else if(input$chooseData=='Detentions'){
    
    leaflet(data = d) %>% addProviderTiles(providers$Stadia.StamenTonerLite,options = providerTileOptions(noWrap = TRUE)) %>% addMarkers(
      lng=~Longitude,lat=~Latitude,
      icon = ~countries[`Palestine/Israel`],
      popup = ~as.character(Detained.Arrested), label = ~as.character(Detained.Arrested),
      clusterOptions = markerClusterOptions()
    ) -> p
  }
  
  # else if(input$chooseData=='Rockets'){
    
  #   leaflet(data = d) %>% addProviderTiles(providers$Stadia.StamenTonerLite,options = providerTileOptions(noWrap = TRUE)) %>%  addMarkers(
  #     lng=~Longitude,lat=~Latitude,
  #     icon = ~countries[`Palestine/Israel`],
  #     popup = ~as.character(Rocket.Number), label = ~as.character(Rocket.Number),
  #     clusterOptions = markerClusterOptions()
  #   ) -> p
  # }
  
  # else if(input$chooseData=='Incendiary Balloons'){
    
  #   leaflet(data = d) %>% addProviderTiles(providers$Stadia.StamenTonerLite,options = providerTileOptions(noWrap = TRUE)) %>%  addMarkers(
  #     lng=~Longitude,lat=~Latitude,
  #     icon = ~countries[`Palestine/Israel`],
  #     popup = ~as.character(Balloon.Number), label = ~as.character(Balloon.Number),
  #     clusterOptions = markerClusterOptions()
  #   ) -> p
  # }
  
  else if(input$chooseData=='Riots'){

    leaflet(data = d) %>% addProviderTiles(providers$Stadia.StamenTonerLite,options = providerTileOptions(noWrap = TRUE)) %>% addMarkers(
      lng=~Longitude,lat=~Latitude,
      icon = ~countries[`Palestine/Israel`],
      #popup = ~as.character(Balloon.Number), label = ~as.character(Balloon.Number),
      clusterOptions = markerClusterOptions()
    ) -> p
    
  }

  p %>% 
    setView(lng = 34.7818, lat = 32.0853, zoom = 7) %>%
    htmlwidgets::onRender(paste0("
      function(el, x) {
        // Update filter note after map renders
        var filterElement = document.getElementById('filter-note-map');
        if (filterElement) {
          filterElement.innerHTML = '", gsub("'", "\\'", filter_description), "';
        }
      }
    "))
})



