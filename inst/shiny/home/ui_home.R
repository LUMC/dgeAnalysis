
tab_home <- tabItem(
  tabName = "home",
  align="center",
  br(),
  
  h1("Welcome"),
  uiOutput("welcome"),
  
  HTML('<hr style="border-color: #0088cc;">'),
  
  h1("Get started"),
  uiOutput("get_started"),
  
  HTML('<hr style="border-color: #0088cc;">'),
  
  h1("Flowchart of the application"),
  br(),
  img(src="programFlow.png", width="80%"),
  br()
)