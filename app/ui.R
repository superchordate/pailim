ui = dashboardPage(
  dashboardHeader(title = "Palestine vs. Israel"),
  dashboardSidebar(
    useShinyjs(),
    sidebarMenu(
      id='tab',
      menuItem(
        "Maps", 
        tabName = "Maps", 
        icon = icon("globe")
      ),
      menuItem(
        "Lines",
        tabName = "Lines",
        icon = icon("bar-chart")
      ),
      div(
        id='form',
        selectInput('palestine_or_israel','Palestine and/or Israel',choices=c('Both', 'Palestinian Actions','Israeli Actions')),
        # chooseData options will be set based on palestine_or_israel selection.
        selectInput('chooseData','Choose Data',choices=NULL,selected=NULL),
        # year and month.        
        pickerInput('year','Select Year',choices=options$Year,selected=tail(options$Year, 5),multiple=TRUE,
                    options = list(
                      `actions-box` = TRUE,
                      `deselect-all-text` = "None",
                      `select-all-text` = "All"
                    )),
        pickerInput('month','Select Month',choices=months,selected=months,multiple=TRUE,
                    options = list(
                      `actions-box` = TRUE,
                      `deselect-all-text` = "None",
                      `select-all-text` = "All"
                    )),
                    
        uiOutput('dynamic_inputs'),
        actionButton("resetAll", "Reset")
      )
    )
  ),
  dashboardBody(
    downloadButton('download','Get Data'),
    downloadButton('download_1','Get Codebook'),
    p("Note: List below map contains up to 100 observations from selected geographic area."),
    tabItems(
      tabItem(
        tabName = "Maps",
        leafletOutput("mymap",height=600),
        p("Note: GPS coordinates are generally accurate to the town or village level."),
        uiOutput('vertabUI')
      ),
      tabItem(
        tabName = "Lines",
        div(
          selectInput('graphPeriods','Graph Periods',choices=sort(c('Annually','Monthly','Quarterly','Weekly'))),          
          conditionalPanel(
            condition="input.chooseData=='Casualties'|input.chooseData=='Events'",
            selectInput('cV','Color by',choices=NULL)
          ),
          plotlyOutput("myplot",height=400)
        ),
        br(),
        uiOutput("selectedCovariates"),
        plotlyOutput("myplot1",height=400)
      )
    )
  )
)
