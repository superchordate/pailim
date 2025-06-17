# this is the map
output$mymap = renderLeaflet({
  
  req(d())
  req(dataPlot())
  
  d = dataPlot()
  
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
  
  else if(input$chooseData=='Rockets'){
    
    leaflet(data = d) %>% addProviderTiles(providers$Stadia.StamenTonerLite,options = providerTileOptions(noWrap = TRUE)) %>%  addMarkers(
      lng=~Longitude,lat=~Latitude,
      icon = ~countries[`Palestine/Israel`],
      popup = ~as.character(Rocket.Number), label = ~as.character(Rocket.Number),
      clusterOptions = markerClusterOptions()
    ) -> p
  }
  
  else if(input$chooseData=='Incendiary Balloons'){
    
    leaflet(data = d) %>% addProviderTiles(providers$Stadia.StamenTonerLite,options = providerTileOptions(noWrap = TRUE)) %>%  addMarkers(
      lng=~Longitude,lat=~Latitude,
      icon = ~countries[`Palestine/Israel`],
      popup = ~as.character(Balloon.Number), label = ~as.character(Balloon.Number),
      clusterOptions = markerClusterOptions()
    ) -> p
  }
  
  else if(input$chooseData=='Riots'){

    leaflet(data = d) %>% addProviderTiles(providers$Stadia.StamenTonerLite,options = providerTileOptions(noWrap = TRUE)) %>% addMarkers(
      lng=~Longitude,lat=~Latitude,
      icon = ~countries[`Palestine/Israel`],
      #popup = ~as.character(Balloon.Number), label = ~as.character(Balloon.Number),
      clusterOptions = markerClusterOptions()
    ) -> p
    
  }

  p %>% setView(lng = 34.7818, lat = 32.0853, zoom = 7) # set the center of the map
})



