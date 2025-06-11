# this is the map
output$mymap = renderLeaflet({
  
  req(d())
  req(dataPlot())
  req(input$palestine_or_israel)
  
  d = dataPlot()
  
  if(input$chooseData=='Events'){
    leaflet(data = d) %>% addProviderTiles(providers$Stadia.StamenTonerLite,options = providerTileOptions(noWrap = TRUE)) %>% 
      addMarkers(lng=~Longitude,lat=~Latitude,
                  icon = ~countries[datatype],
                  #popup = ~as.character(Verbatim.Report), label = ~as.character(Verbatim.Report),
                  clusterOptions = markerClusterOptions()) -> p
  }
  
  else if(input$chooseData=='Casualties' & input$casualtyType=='All'){
    d = d %>% filter(Casualties>0)
    leaflet(data = d) %>% addProviderTiles(providers$Stadia.StamenTonerLite,options = providerTileOptions(noWrap = TRUE)) %>% 
      addMarkers(
        lng=~Longitude,lat=~Latitude,
        icon = ~countries[datatype],
        #popup = ~as.character(Casualties), label = ~as.character(Casualties),
        clusterOptions = markerClusterOptions()
      ) -> p
    
  }
  
  else if(input$chooseData=='Casualties' & input$casualtyType=='Killed'){
    d = d %>% filter(Killed>0)
    leaflet(data = d) %>% addProviderTiles(providers$Stadia.StamenTonerLite,options = providerTileOptions(noWrap = TRUE)) %>% 
      addMarkers(
        lng=~Longitude,lat=~Latitude,
        icon = ~countries[datatype],
        popup = ~as.character(Killed), label = ~as.character(Killed),
        clusterOptions = markerClusterOptions()
      ) -> p
  }
  
  else if(input$chooseData=='Casualties' & input$casualtyType=='Injured'){
    d = d %>% filter(Injured>0)
    leaflet(data = d) %>% addProviderTiles(providers$Stadia.StamenTonerLite,options = providerTileOptions(noWrap = TRUE)) %>% 
      addMarkers(
        lng=~Longitude,lat=~Latitude,
        icon = ~countries[datatype],
        popup = ~as.character(Injured), label = ~as.character(Injured),
        clusterOptions = markerClusterOptions()
      ) -> p
  }
  
  else if(input$chooseData=='Detentions'){
    d = d %>% filter(Detained.Arrested>0)
    leaflet(data = d) %>% addProviderTiles(providers$Stadia.StamenTonerLite,options = providerTileOptions(noWrap = TRUE)) %>% addMarkers(
      lng=~Longitude,lat=~Latitude,
      icon = ~countries[datatype],
      popup = ~as.character(Detained.Arrested), label = ~as.character(Detained.Arrested),
      clusterOptions = markerClusterOptions()
    ) -> p
  }
  
  else if(input$chooseData=='Rockets'){
    d = d %>% filter(Rocket.Number>0)
    leaflet(data = d) %>% addProviderTiles(providers$Stadia.StamenTonerLite,options = providerTileOptions(noWrap = TRUE)) %>%  addMarkers(
      lng=~Longitude,lat=~Latitude,
      icon = ~countries[datatype],
      popup = ~as.character(Rocket.Number), label = ~as.character(Rocket.Number),
      clusterOptions = markerClusterOptions()
    ) -> p
  }
  
  else if(input$chooseData=='Incendiary Balloons'){
    d = d %>% filter(Balloon.Number>0)
    leaflet(data = d) %>% addProviderTiles(providers$Stadia.StamenTonerLite,options = providerTileOptions(noWrap = TRUE)) %>%  addMarkers(
      lng=~Longitude,lat=~Latitude,
      icon = ~countries[datatype],
      popup = ~as.character(Balloon.Number), label = ~as.character(Balloon.Number),
      clusterOptions = markerClusterOptions()
    ) -> p
  }
  
  else if(input$chooseData=='Riots'){
    d = d %>% filter(Type.Violence=='Riot'|Secondary.Type.Violence.2=='Riot') %>% 
      filter(Riot.SubCategory %in% input$riot.sub)
    leaflet(data = d) %>% addProviderTiles(providers$Stadia.StamenTonerLite,options = providerTileOptions(noWrap = TRUE)) %>% addMarkers(
      lng=~Longitude,lat=~Latitude,
      icon = ~countries[datatype],
      #popup = ~as.character(Balloon.Number), label = ~as.character(Balloon.Number),
      clusterOptions = markerClusterOptions()
    ) -> p
  }
  
  p %>% setView(lng = 34.7818, lat = 32.0853, zoom = 7) # set the center of the map
})



