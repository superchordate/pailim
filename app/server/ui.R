observeEvent(input$resetAll, {
  reset("form")
})

# update graph choice based on selected actions
observeEvent(input$actor,{

# available plot types for selected actions
  chooseData_both = c('Events', 'Casualties')
  
  if(input$actor=='Both'){
    updated_choices = chooseData_both
  } else if (input$actor=='Palestinian Actions'){
    updated_choices = c(chooseData_both, 'Rockets', 'Incendiary Balloons','Riots')
  } else if (input$actor=='Israeli Actions'){
    updated_choices = c(chooseData_both, 'Detentions')
  }
  
  updateSelectInput(session, 'chooseData', choices = sort(updated_choices), selected = 'Events')

})

# this is to update color_by option for main line graphs
observeEvent(
  {
    input$tab
    input$chooseData
    input$casualtyType
    input$actor
    input$xAxis
  },{

    req(input$actor)
    req(input$chooseData)
    req(input$casualtyType)

    color_choices_all =  c('None', 'Type of Action', "District")
    
    if(input$actor=='Both'){
      color_choices = c(color_choices_all, "Palestine/Israel")
    } else if (input$actor=='Palestinian Actions'){

      color_choices = c(color_choices_all, 'Perpetrator Origin', 'Region')

    } else if (input$actor=='Israeli Actions'){

      color_choices = c(color_choices_all, 'Perpetrator Type')
    }

    if(input$chooseData == 'Casualties' && input$casualtyType == 'All'){
      color_choices = c(color_choices, 'Casualty Type')
    } else if(input$chooseData == 'Riots'){
      color_choices = c(color_choices, 'Riot Subcategories') %>% setdiff('Type of Action')
    } else if(input$chooseData == 'Rockets' || input$chooseData == 'Incendiary Balloons'){
      color_choices = color_choices %>% setdiff('Type of Action')
    }

    # Don't allow color by = xAxis.
    color_choices %<>% setdiff(input$xAxis)
    
    updateSelectInput(session, 'colorBy', choices = color_choices, selected = 'None')

  })


# this is to update the covariate plot options
observeEvent(
  {
    input$actor
    input$xAxis
  },{
    
    covariate_choices = c()
    
    # time-based covariates.
    if(input$xAxis %in% c('Year', 'Month', 'Quarter', 'Week')){
      
      covariate_choices %<>% c(
        'Exchange Rate', 'Home Demolitions by Israel', 'Rainfall',
        'Stock Market Index', 'Temperature', 'Settler Population', 'Number of Outposts',
        'Palestinian Population', 'Average Daily Wage', 'Crime', 'Labor Participation'
      )

      if (input$xAxis == 'Week'){
        
        covariate_choices %<>% c(
          'Hamas-Fatah Reconciliation Talks', 'Israeli Coalition Size',
          'Israeli Operation', 'UN Vote', 'US-Israel State Visits'
        )

      } else {

        covariate_choices %<>% c(
          'Consumer Price Index', 'Israel-Gaza Crossing (Goods)',
          'Israel-Gaza Crossing (People)', 'Trade Balance', 'Unemployment'
        )

      }

    }

    # geographic covariates.
    if(input$xAxis == "District"){

      # We can take these options directly from the data. 
      covariate_choices = c(
          "Settler.Population", "N.Outposts", "Palestinian.Population",
          "Avg.Daily.Wage", "Crime", "Labor.Participation"
      )

    }
    
    updateSelectInput(
      session,
      'selectedCovariates',
      choices = c('None', covariate_choices),
      selected = 'None'
    )
  })

# Update X-Axis choices based on actor selection
observeEvent({
  input$actor
  input$chooseData
}, {

  time_choices = c('Year', 'Month', 'Quarter', 'Week')

  if(input$actor == 'Palestinian Actions') {
    updated_choices = c(time_choices, 'District', 'Region', 'Type of Action', 'Perpetrator Origin')
  } else if(input$actor == 'Israeli Actions') {
    updated_choices = c(time_choices, 'District', 'Type of Action', 'Perpetrator Type')
  } else {
    updated_choices = c(time_choices, 'District', 'Type of Action')
  }
  
  # Remove Type of Action for specific data types
  if(input$chooseData == 'Riots' || input$chooseData == 'Rockets' || input$chooseData == 'Incendiary Balloons') {
    updated_choices = updated_choices %>% setdiff('Type of Action')
  }
  
  updateSelectInput(
    session, 
    'xAxis', 
    choices = updated_choices, 
    selected = if(input$xAxis %in% updated_choices) input$xAxis else 'Year'
  )

})

# general dropdowns on sidebar
output$dynamic_inputs = renderUI({
  tagList(
                
    # riot
    conditionalPanel(
      condition="input.chooseData=='Riots'",
      pickerInput('riot.sub','Riot Subcategories',
                  choices=sort(na.omit(unique(d()$`Riot Subcategories`))),
                  selected=na.omit(unique(d()$`Riot Subcategories`)),multiple=TRUE,
                  options = list(
                    `actions-box` = TRUE,
                    `deselect-all-text` = "None",
                    `select-all-text` = "All"
                  ))
    ),
    conditionalPanel(
      condition="input.chooseData=='Casualties'",
      pickerInput('casualtyType','Casualty Subcategories',choices=sort(c('All','Killed','Injured')),selected='All',multiple=FALSE#,
      )
    ),
    
    # Hide Type of Action input for specific data types
    conditionalPanel(
      condition="input.chooseData!='Riots' && input.chooseData!='Rockets' && input.chooseData!='Incendiary Balloons'",
      pickerInput(
        'selectedActionTypes', 'Type of Action',
        choices = options$`Type of Action: cm`,
        selected = options$`Type of Action: cm`, 
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `deselect-all-text` = "None",
          `select-all-text` = "All"
        ))
    ),

        # There are too many cities to include in a picker. 
        # pickerInput('City','City',choices=sort(na.omit(unique(d()$City))),selected=na.omit(unique(d()$City)),multiple=TRUE,
        #             options = list(
        #               `actions-box` = TRUE,
        #               `deselect-all-text` = "None",
        #               `select-all-text` = "All"
        #             )),

        pickerInput('District','District',choices=sort(na.omit(unique(d()$District))),selected=na.omit(unique(d()$District)),multiple=TRUE,
                    options = list(
                      `actions-box` = TRUE,
                      `deselect-all-text` = "None",
                      `select-all-text` = "All"
                    )),

       # Inputs specific to Israeli data.
      conditionalPanel(
        condition="input.actor=='Israeli Actions'",
        pickerInput('perpetrator.type','Perpetrator Type',choices=sort(na.omit(unique(d()$`Perpetrator Type`))),selected=na.omit(unique(d()$`Perpetrator Type`)),multiple=TRUE,
                    options = list(
                      `actions-box` = TRUE,
                      `deselect-all-text` = "None",
                      `select-all-text` = "All"
                    ))
        
      ),

      # Inputs specific to Palestinian data.
      conditionalPanel(
        condition="input.actor=='Palestinian Actions'",
        pickerInput('perpetrator.origin','Perpetrator Origin',choices=sort(na.omit(unique(d()$`Perpetrator Origin`))),selected=na.omit(unique(d()$`Perpetrator Origin`)),multiple=TRUE,
                    options = list(
                      `actions-box` = TRUE,
                      `deselect-all-text` = "None",
                      `select-all-text` = "All"
                    )),
        pickerInput('region','Region',choices=sort(na.omit(unique(d()$Region))),selected=na.omit(unique(d()$Region)),multiple=TRUE,
                    options = list(
                      `actions-box` = TRUE,
                      `deselect-all-text` = "None",
                      `select-all-text` = "All"
                    ))
      ),
      
    )
})


