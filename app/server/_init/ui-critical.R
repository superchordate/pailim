# This file contains dynamic UI code that must run first to prevent errors in the published app. 

# type of action choices.

available_actions = '' # save server-level object for immediate checking of status. 

# update selectedActionTypes choices based on selected actor

observeEvent(input$actor, {
  req(input$actor)
  
  # Get unique action types from the selected actor's dataset
  if(input$actor == 'Both') {
    available_actions <<- options$`Type of Action: cm`
  } else if(input$actor == 'Palestinian Actions') {
    available_actions <<- options$`Type of Action: pa`
  } else if(input$actor == 'Israeli Actions') {
    available_actions <<- options$`Type of Action: il`
  }
  
  # Update the pickerInput choices
  updatePickerInput(
    session = session,
    inputId = 'selectedActionTypes',
    choices = available_actions,
    selected = available_actions
  )
})
