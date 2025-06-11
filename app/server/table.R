# get the boundaries of map to display the actions in text (on bottom)
vertab = reactive({

    req(!is.null(input$mymap_bounds))

    bounds = input$mymap_bounds
    d = dataPlot() %>% 
        select(
            Year, Month, 
            Type = `Palestine/Israel`, 
            Crimes,
            Injured, Killed, 
            Description = Verbatim.Report,
            Longitude, Latitude
        ) %>% 
        mutate(
            Year = as.integer(Year),
            Month = factor(Month, levels = month.abb),
            Crimes = factor(Crimes),
            Type = factor(fifelse(
                Type == 'Palestine', 
                'Palestinian Actions',
                'Israeli Actions'
            ))
        ) %>% 
        filter(
            Latitude <= bounds$north,
            Latitude >= bounds$south,  
            Longitude <= bounds$east,
            Longitude >= bounds$west
        ) %>%
        select(-Longitude,-Latitude) %>%
        arrange(Year, Month, Type)
    
    return(d)

})

output$vertab = DT::renderDT(
    vertab(),
    filter = 'top',
    options = list(dom = 'frtip', pageLength = 5),
    rownames = FALSE
)


output$vertabUI = renderUI({
  req(vertab())
  div(style = "background-color: white; padding: 10px; ", DTOutput('vertab'))
})