# get the boundaries of map to display the actions in text (on bottom)
vertab = reactive({

    req(!is.null(input$mymap_bounds))

    bounds = input$mymap_bounds
    d = dataPlot() %>% 
        select(
            Year, Month, 
            Type = `Palestine/Israel`, 
            `Type of Action`,
            Injured, Killed, 
            Description = Verbatim.Report,
            Longitude, Latitude
        ) %>% 
        mutate(
            Year = as.integer(Year),
            Month = factor(Month, levels = month.abb),
            `Type of Action` = factor(`Type of Action`),
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
    options = list(
      dom = 'rtip', 
      pageLength = 5, 
      processing = FALSE,
      initComplete = DT::JS(paste0("
        function(settings, json) {
          var filterElement = document.getElementById('filter-note-table');
          if (filterElement) {
            filterElement.innerHTML = '", gsub("'", "\\'", filter_description), "';
          }
        }
      "))
    ),
    rownames = FALSE
)


output$vertabUI = renderUI({
  req(vertab())
  div(
    style = "background-color: white; padding: 10px;",
    div(
      class = "filter-note",
      HTML('<strong>Filters:</strong> <span id="filter-note-table">Loading...</span>')
    ),
    DTOutput('vertab')
  )
})