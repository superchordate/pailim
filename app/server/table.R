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
      pageLength = 10, 
      processing = FALSE,
      scrollX = TRUE,
      scrollY = "400px",
      scrollCollapse = TRUE,
      autoWidth = TRUE,
      responsive = TRUE,
      columnDefs = list(
        list(width = "60px", targets = c(0, 1)), # Year, Month - narrower
        list(width = "100px", targets = c(2, 4, 5)), # Type, Injured, Killed
        list(width = "120px", targets = 3), # Type of Action
        list(width = "250px", targets = 6, className = "description-col") # Description - wider but controlled
      ),
      initComplete = DT::JS(paste0("
        function(settings, json) {
          var filterElement = document.getElementById('filter-note-table');
          if (filterElement) {
            filterElement.innerHTML = '", gsub("'", "\\'", filter_description), "';
          }
          
          // Add word wrapping to description column
          $(this.api().table().container()).find('.description-col').css({
            'word-wrap': 'break-word',
            'white-space': 'normal',
            'max-width': '250px'
          });
        }
      "))
    ),
    rownames = FALSE
)


output$vertabUI = renderUI({
  req(vertab())
  div(
    style = "background-color: white; padding: 10px; max-height: 100dvh; overflow-y: auto;",
    div(
      class = "filter-note",
      HTML('<strong>Filters:</strong> <span id="filter-note-table">Loading...</span>')
    ),
    div(
      class = "table-container",
      DTOutput('vertab')
    )
  )
})