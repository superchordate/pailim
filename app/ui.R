ui = dashboardPage(
  dashboardHeader(title = "PA'ILIM Dataset"),
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
        selectInput('actor','Actor',choices=c('Both', 'Palestinian Actions','Israeli Actions')),
        # chooseData options will be set based on actor selection.
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
    )  ),
  dashboardBody(

    # Javascript imports. 
    tags$head(
      tags$script(src = "highcharts.js"),
      tags$script(src = "highcharts-defaults.js"),
      HTML('<style id="map-loading-styles">@keyframes spin { 0% { transform: rotate(0deg); } 100% { transform: rotate(360deg); } }</style>'),
    ),

    # Downloads.
    tags$a(
      href = "https://storage.googleapis.com/pailim-public/Codebook.pdf", 
      target = "_blank", 
      class = "btn btn-default", 
      icon("file-pdf"), 
      "Get Codebook"
    ),
    tags$a(
      href = "https://docs.google.com/forms/d/e/1FAIpQLSdNtT3-43qJkrT28MI_3GIowBwgL6H0HWEuBLVKB10QgGAF1g/viewform?usp=dialog", 
      target = "_blank", 
      class = "btn btn-default", 
      icon("download"), 
      "Get Data"
    ),

    # Main content
    br(), br(),
    tabItems(
      tabItem(
        tabName = "Maps",
        HTML('
            <div style="position: absolute; top: 300px; left: calc(50% - 50px); text-align: center; padding: 20px; background: white; border-radius: 8px; box-shadow: 0 4px 12px rgba(0,0,0,0.15);">
              <div class="spinner" style="
                border: 3px solid #e9ecef;
                border-top: 3px solid #007bff;
                border-radius: 50%;
                width: 32px;
                height: 32px;
                animation: spin 1s linear infinite;
                margin: 0 auto 12px auto;
              "></div>
              <div style="font-size: 14px; color: #495057; font-weight: 500;">Loading map...</div>
          </div>'),
        p("Note: GPS coordinates are generally accurate to the town or village level."),
        leafletOutput("mymap",height=600),
        br(),
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
          selectInput('selectedCovariates', 'Add Covariates', choices = NULL),
          uiOutput("lineplot", height = 400)
        )
      )
    )
  )
)
