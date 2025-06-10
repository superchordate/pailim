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
        uiOutput('visualtype'),
        selectInput('datatype','Select Data',choices=c('Both', 'Palestinian Actions','Israeli Actions')),
        uiOutput('yearmonth'),
        uiOutput('ctrlUI'),
        uiOutput('p3'),
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
        plotlyOutput("myplot",height=400),
        br(),
        uiOutput("p2"),
        plotlyOutput("myplot1",height=400)
      )
    )
  )
)
