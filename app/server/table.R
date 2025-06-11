output$vertabUI = renderUI({
  #req(input$chooseData=='Events')
  tagList(
    div(style = 'height:200px; overflow-y: scroll;overflow-x: scroll;', DTOutput("vertab") %>% withSpinner(color='#000000'))
  )
})
#})

# get the boundaries of map to display the actions in text (on bottom)
vertab = reactive({
  req(!is.null(input$mymap_bounds))
  #req(input$chooseData=='Events')
  
  bounds = input$mymap_bounds
  d = dataPlot()
  d = d %>% select(Year,Month,Type=datatype,Description=Verbatim.Report,Longitude,Latitude) %>% 
    mutate(Type = ifelse(Type=='Palestine','Palestinian Actions','Israeli Actions')) %>% 
    filter(Latitude <= bounds$north,Latitude >= bounds$south,Longitude <= bounds$east,Longitude >= bounds$west) %>%
    select(-Longitude,-Latitude)
  sample_size = ifelse(nrow(d)>100,100,nrow(d))
  d = d[sample(1:nrow(d),sample_size),]
  d
})

output$vertab = DT::renderDT(
  vertab() %>% arrange(Year,Month),
  options = list(dom = 't',pageLength = 100),
  rownames= FALSE
)