# This files contains the server-side logic for rendering the data table at the bottom of the map tab.

# Data function for the table. 
# Gets data based on the current map bounds and formats it for display in the table.
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

# Render the table in the UI.
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

