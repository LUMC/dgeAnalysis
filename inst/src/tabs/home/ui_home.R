
tab_home <- tabItem(
  tabName = "home",
  align = "center",
  br(),
  
  h1("Welcome"),
  uiOutput("welcome"),
  
  HTML('<hr style="border-color: #0088cc;">'),
  
  h1("Get started"),
  uiOutput("get_started"),
  
  HTML('<hr style="border-color: #0088cc;">'),
  
  h1("Flowchart of the application"),
  div(
    br(),
    img(src = "programFlow.png", width = "90%"),
    br(),
    br(),
    style = "background-color: #f5f5f5; border: 1px solid #e3e3e3; width: 90%"
  )
)
