
tab_about <- tabItem(
  tabName = "about",
  align="center",
  br(),
  
  HTML('<hr style="border-color: #0088cc;">'),
  h2("Shiny DGE Analysis"),
  HTML('<hr style="border-color: #0088cc;">'),
  br(),
  br(),
  h3("Version:", as.character(packageVersion("dgeAnalysis"))),
  h3("Author: Tom Kuipers"),
  h3(a(href="https://github.com/LUMC/DGE_analysis", "GitHub page")),
  img(src="lumcLogo.png", width="300px"),
  br(),br(),br(),
  HTML('<hr style="border-color: #0088cc;">'),
  h2("Session info"),
  verbatimTextOutput("currentSession")
)