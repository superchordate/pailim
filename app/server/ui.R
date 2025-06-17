
observeEvent(input$resetAll, {
  reset("form")
})

# update graph choice based on selected actions
observeEvent(input$palestine_or_israel,{
  
  if(input$palestine_or_israel=='Both'){
    updated_choices = chooseData_both
  } else if (input$palestine_or_israel=='Palestinian Actions'){
    updated_choices = chooseData_pa
  } else if (input$palestine_or_israel=='Israeli Actions'){
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
  },{
    
    if(input$palestine_or_israel=='Both'){
      color_choices = c('None','Palestine/Israel', 'Crimes')
    } else if (input$palestine_or_israel=='Palestinian Actions'){
      color_choices = c('None','Crimes', 'Perpetrator.Origin','Region')
    } else if (input$palestine_or_israel=='Israeli Actions'){
      color_choices = c('None','Crimes', 'Perpetrator.Type','City')
    }
    
    if(input$chooseData=='Casualties'& input$casualtyType=='All'){
      color_choices=c(color_choices,'Casualty Type')
    } else {
      color_choices
    }
    
    updateSelectInput(session,'cV',
                      choices=color_choices,
                      selected='None')
  })


# this is to update the covariate plot options
observeEvent(
  {
    input$palestine_or_israel
    input$graphPeriods
  },{
    #req(input$tab=="lines")
    
    "
  Consumer Price Index, Unemployment, Trade Balance, Home Demolitions by Israel, Rainfall, 
  Temperature, Stock Market Index, Israel-Gaza Crossing (People), Israel-Gaza Crossing (Goods)
  "
    
    updated_choices=NULL
    
    if(input$palestine_or_israel=='Both' | input$palestine_or_israel=='Palestinian Actions' | input$palestine_or_israel=='Israeli Actions'){
      if(input$graphPeriods=='Annually'){
        updated_choices = c('None','Consumer Price Index','Exchange Rate','Home Demolitions by Israel',
                          'Israel-Gaza Crossing (Goods)','Israel-Gaza Crossing (People)','Rainfall',
                          'Stock Market Index','Temperature','Trade Balance','Unemployment')
            } else if (input$graphPeriods=='Quarterly'){
            updated_choices = c('None','Consumer Price Index','Exchange Rate','Home Demolitions by Israel',
                          'Israel-Gaza Crossing (Goods)','Israel-Gaza Crossing (People)','Rainfall',
                          'Stock Market Index','Temperature','Trade Balance','Unemployment')
            } else if (input$graphPeriods=='Monthly'){
            updated_choices = c('None','Consumer Price Index','Exchange Rate','Home Demolitions by Israel',
                          'Israel-Gaza Crossing (Goods)','Israel-Gaza Crossing (People)','Rainfall',
                          'Stock Market Index','Temperature','Trade Balance')
            } else if (input$graphPeriods=='Weekly'){
            updated_choices = c('None','Exchange Rate','Hamas-Fatah Reconciliation Talks','Home Demolitions by Israel',
                          'Israeli Coalition Size','Israeli Operation','Rainfall','Stock Market Index',
                          'Temperature','UN Vote','US-Israel State Visits')
      }
    } else if (input$palestine_or_israel=='Both') {
      updated_choices = 'None'
    }
    
    updateSelectInput(session,'selectedCovariates',
                      choices=updated_choices,
                      selected='None')
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
    
    
        pickerInput('selectedCrimes','Crimes',choices=options$Crimes,selected=options$Crimes,multiple=TRUE,
                    options = list(
                      `actions-box` = TRUE,
                      `deselect-all-text` = "None",
                      `select-all-text` = "All"
                    )),

       # Inputs specific to Israeli data.
      conditionalPanel(
        condition="input.palestine_or_israel=='Israeli Actions'",
        pickerInput('perpetrator.type','Select Perpetrator Type',choices=sort(na.omit(unique(d()$Perpetrator.Type))),selected=na.omit(unique(d()$Perpetrator.Type)),multiple=TRUE,
                    options = list(
                      `actions-box` = TRUE,
                      `deselect-all-text` = "None",
                      `select-all-text` = "All"
                    )),
        pickerInput('gover','Governorate',choices=sort(na.omit(unique(d()$City))),selected=na.omit(unique(d()$City)),multiple=TRUE,
                    options = list(
                      `actions-box` = TRUE,
                      `deselect-all-text` = "None",
                      `select-all-text` = "All"
                    ))
        
      ),

      # Inputs specific to Palestinian data.
      conditionalPanel(
        condition="input.palestine_or_israel=='Palestinian Actions'",
        pickerInput('perpetrator.origin','Select Perpetrator Origin',choices=sort(na.omit(unique(d()$Perpetrator.Origin))),selected=na.omit(unique(d()$Perpetrator.Origin)),multiple=TRUE,
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


