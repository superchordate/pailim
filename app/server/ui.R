observeEvent(input$resetAll, {
  reset("form")
})

# update graph choice based on selected actions
observeEvent(input$actor,{
  
  if(input$actor=='Both'){
    updated_choices = chooseData_both
  } else if (input$actor=='Palestinian Actions'){
    updated_choices = chooseData_pa
  } else if (input$actor=='Israeli Actions'){
    updated_choices = chooseData_il
  }
  
  updateSelectInput(session,'chooseData',
                    choices=updated_choices,
                    selected='Events')
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
      color_choices = c('None', 'Palestine/Israel', 'Type of Action', "District", "City")
    } else if (input$actor=='Palestinian Actions'){
      color_choices = c('None', 'Type of Action', 'Perpetrator Origin', 'Region', "District", 'City')
    } else if (input$actor=='Israeli Actions'){
      color_choices = c('None', 'Type of Action', 'Perpetrator Type', "District", 'City')
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
    #req(input$tab=="lines")
    
    "
  Consumer Price Index, Unemployment, Trade Balance, Home Demolitions by Israel, Rainfall, 
  Temperature, Stock Market Index, Israel-Gaza Crossing (People), Israel-Gaza Crossing (Goods)
  "
    
    updated_choices=NULL
    
    if(input$actor=='Both' | input$actor=='Palestinian Actions' | input$actor=='Israeli Actions'){
      if (input$xAxis=='Quarter'){
            updated_choices = c('None','Consumer Price Index','Exchange Rate','Home Demolitions by Israel',
                          'Israel-Gaza Crossing (Goods)','Israel-Gaza Crossing (People)','Rainfall',
                          'Stock Market Index','Temperature','Trade Balance','Unemployment')
      } else if (input$xAxis=='Month'){
            updated_choices = c('None','Consumer Price Index','Exchange Rate','Home Demolitions by Israel',
                          'Israel-Gaza Crossing (Goods)','Israel-Gaza Crossing (People)','Rainfall',
                          'Stock Market Index','Temperature','Trade Balance')
      } else if (input$xAxis=='Week'){
            updated_choices = c('None','Exchange Rate','Hamas-Fatah Reconciliation Talks','Home Demolitions by Israel',
                          'Israeli Coalition Size','Israeli Operation','Rainfall','Stock Market Index',
                          'Temperature','UN Vote','US-Israel State Visits')
      } else {
            updated_choices = c('None','Consumer Price Index','Exchange Rate','Home Demolitions by Israel',
                          'Israel-Gaza Crossing (Goods)','Israel-Gaza Crossing (People)','Rainfall',
                          'Stock Market Index','Temperature','Trade Balance','Unemployment')
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

  time_choices = c('Year','Month','Quarter','Week')

  if(input$actor == 'Palestinian Actions') {
    updated_choices = c(time_choices, 'District', 'Region', 'Type of Action', 'Perpetrator Origin')
  } else if(input$actor == 'Israeli Actions') {
    updated_choices = c(time_choices, 'Area', 'City', 'Type of Action', 'Perpetrator Type')
  } else {
    updated_choices = c(time_choices, 'Type of Action')
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

        pickerInput('City','City',choices=sort(na.omit(unique(d()$City))),selected=na.omit(unique(d()$City)),multiple=TRUE,
                    options = list(
                      `actions-box` = TRUE,
                      `deselect-all-text` = "None",
                      `select-all-text` = "All"
                    )),

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


