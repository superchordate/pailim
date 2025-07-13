time_covariates_filtered = reactive({
    time_covariates %>%
        filter(
            Year %in% input$year, 
            Month %in% input$month
        )
})

geo_covariates_filtered = reactive({
    geo_covariates %>%
        filter(
            Year %in% input$year, 
            Month %in% input$month,
            District %in% input$District
        )
})