
tab_about <- tabItem(
  tabName = "about",
  align="center",
  br(),
  
  HTML('<hr style="border-color: #0088cc;">'),
  h2("dgeAnalysis"),
  h4("R shiny tool to perform differential gene expression"),
  HTML('<hr style="border-color: #0088cc;">'),
  br(),
  br(),
  
  h3("Version:", as.character(packageVersion("dgeAnalysis"))),
  h2("Developed by LUMC-SASC team"),
  h3("Tom Kuipers"),
  h4(
    "Leon Mei", tags$br(),
    "Davy Cats", tags$br()
  ),
  br(),
  
  h3(a(href="https://github.com/LUMC/DGE_analysis", "GitHub page")),
  img(src="lumcLogo.png", width="300px"),
  br(),br(),br(),
  HTML('<hr style="border-color: #0088cc;">'),
  
  h2("Session info"),
  verbatimTextOutput("currentSession")
)
