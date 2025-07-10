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
  },{

    req(input$actor)
    req(input$chooseData)
    req(input$casualtyType)
    
    if(input$actor=='Both'){
      color_choices = c('None', 'Palestine/Israel', 'Type of Action', "District")
    } else if (input$actor=='Palestinian Actions'){
      color_choices = c('None', 'Type of Action', 'Perpetrator Origin', 'Region', "District")
    } else if (input$actor=='Israeli Actions'){
      color_choices = c('None', 'Type of Action', 'Perpetrator Type', "District")
    }
    
    if(input$chooseData=='Casualties'& input$casualtyType=='All'){
      color_choices=c(color_choices,'Casualty Type')
    } else {
      color_choices
    }
    
    updateSelectInput(session,'colorBy',
                      choices=color_choices,
                      selected='None')
  })


# this is to update the covariate plot options
observeEvent(
  {
    input$actor
    input$xAxis
  },{
    
    updated_choices = NULL
    
    if(input$actor=='Both' | input$actor=='Palestinian Actions' | input$actor=='Israeli Actions'){
      # Base covariates available for most time periods
      base_covariates = c('None','Exchange Rate','Home Demolitions by Israel','Rainfall',
                         'Stock Market Index','Temperature','Settler Population','Number of Outposts',
                         'Palestinian Population','Average Daily Wage','Crime','Labor Participation')
      
      if (input$xAxis=='Quarter'){
            updated_choices = c(base_covariates, 'Consumer Price Index','Israel-Gaza Crossing (Goods)',
                               'Israel-Gaza Crossing (People)','Trade Balance','Unemployment')
      } else if (input$xAxis=='Month'){
            updated_choices = c(base_covariates, 'Consumer Price Index','Israel-Gaza Crossing (Goods)',
                               'Israel-Gaza Crossing (People)','Trade Balance','Unemployment')
      } else if (input$xAxis=='Week'){
            updated_choices = c(base_covariates, 'Hamas-Fatah Reconciliation Talks','Israeli Coalition Size',
                               'Israeli Operation','UN Vote','US-Israel State Visits')
      } else {
            updated_choices = c(base_covariates, 'Consumer Price Index','Israel-Gaza Crossing (Goods)',
                               'Israel-Gaza Crossing (People)','Trade Balance','Unemployment')
      }
    } else if (input$actor=='Both') {
      updated_choices = 'None'
    }
    
    updateSelectInput(session,'selectedCovariates',
                      choices=updated_choices,
                      selected='None')
  })
# Update X-Axis choices based on actor selection
observeEvent(input$actor, {

  time_choices = c('Year', 'Month', 'Quarter', 'Week')

  if(input$actor == 'Palestinian Actions') {
    updated_choices = c(time_choices, 'District', 'Region', 'City', 'Type of Action', 'Perpetrator Origin')
  } else if(input$actor == 'Israeli Actions') {
    updated_choices = c(time_choices, 'District', 'City', 'Type of Action', 'Perpetrator Type')
  } else {
    updated_choices = c(time_choices, 'District', 'City', 'Type of Action')
  }
  
  updateSelectInput(session, 'xAxis',
                    choices = updated_choices,
                    selected = if(input$xAxis %in% updated_choices) input$xAxis else 'Year')
})

# general dropdowns on sidebar
output$dynamic_inputs = renderUI({
  tagList(
                
    # riot
    conditionalPanel(
      condition="input.chooseData=='Riots'",
      pickerInput('riot.sub','Riot Subcategories',
                  choices=sort(na.omit(unique(d()$Riot.SubCategory))),
                  selected=na.omit(unique(d()$Riot.SubCategory)),multiple=TRUE,
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
    
    
        pickerInput(
          'selectedActionTypes', 'Type of Action',
          choices = options$`Type of Action: cm`,
          selected = options$`Type of Action: cm`, 
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `deselect-all-text` = "None",
            `select-all-text` = "All"
          )),

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


