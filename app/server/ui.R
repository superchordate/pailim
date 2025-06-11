
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
    
    print(input$chooseData)
    print(input$casualtyType)
    
    if(input$palestine_or_israel=='Both'){
      color_choices = c('None','palestine_or_israel','Type.Violence')
    } else if (input$palestine_or_israel=='Palestinian Actions'){
      color_choices = c('None','Type.Violence','Perpetrator.Origin','Region')
    } else if (input$palestine_or_israel=='Israeli Actions'){
      color_choices = c('None','Type.Violence','Perpetrator.Type','City')
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
        updated_choices = sort(c('None','Consumer Price Index','Unemployment','Trade Balance','Exchange Rate',
                                  'Home Demolitions by Israel','Rainfall','Stock Market Index',"Temperature",
                                  'Israel-Gaza Crossing (People)','Israel-Gaza Crossing (Goods)'))
      } else if (input$graphPeriods=='Quarterly'){
        updated_choices = sort(c('None','Consumer Price Index','Unemployment','Trade Balance','Exchange Rate',
                                  'Home Demolitions by Israel','Rainfall','Stock Market Index',"Temperature",
                                  'Israel-Gaza Crossing (People)','Israel-Gaza Crossing (Goods)'))
      } else if (input$graphPeriods=='Monthly'){
        updated_choices = sort(c('None','Consumer Price Index','Trade Balance','Exchange Rate','Home Demolitions by Israel',
                                  'Rainfall','Stock Market Index',"Temperature",'Israel-Gaza Crossing (People)','Israel-Gaza Crossing (Goods)'))
      } else if (input$graphPeriods=='Weekly'){
        updated_choices = sort(c('None','Exchange Rate','Home Demolitions by Israel','Rainfall','Stock Market Index',"Temperature",
                                  'Hamas-Fatah Reconciliation Talks','Israeli Operation','US-Israel State Visits',
                                  'UN Vote','Israeli Coalition Size'))
      }
    } else if (input$palestine_or_israel=='Both') {
      updated_choices = 'None'
    }
    
    updateSelectInput(session,'p2',
                      choices=updated_choices,
                      selected='None')
  })

# p2 is the dropdown for covariate graph, only available when action is not 'Both'
output$p2 = renderUI({
  req(input$palestine_or_israel=='Both' | input$palestine_or_israel=='Palestinian Actions' | input$palestine_or_israel=='Israeli Actions')
  selectInput('p2','Add Covariate Graph',choices=NULL)
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
    
    
    conditionalPanel(
      condition="input.chooseData=='Events'|input.chooseData=='Casualties'",
      pickerInput('primary.violence','Select Primary Violence Type',choices=sort(na.omit(unique(d()$Type.Violence))),selected=na.omit(unique(d()$Type.Violence)),multiple=TRUE,
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
                    )),
        pickerInput('Combined_Crimes','Combined Crimes',choices=options$Combined_Crimes,selected=options$Combined_Crimes,multiple=TRUE,
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
  )
})


