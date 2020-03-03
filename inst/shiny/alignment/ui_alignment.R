
tab_alignment <- tabItem(
  tabName = "alignment",
  align="center",
  br(),
  
  tabsetPanel(
    tabPanel("Summary",
             HTML('<hr style="border-color: #0088cc;">'),
             radioButtons(
               "setSummary",
               "Set distribution:",
               inline = TRUE,
               c("Actual" = "actual",
                 "Percentage" = "percent")
             ),
             plotlyOutput("align_sum", height = "600px") %>% withSpinner(),
             HTML('<hr style="border-color: #0088cc;">')
    ),
    tabPanel("Complexity",
             HTML('<hr style="border-color: #0088cc;">'),
             radioButtons(
               "setComplexity",
               "Set distribution:",
               inline = TRUE,
               c("Actual" = "actual",
                 "Percentage" = "percent")
             ),
             plotlyOutput("complex", height = "600px") %>% withSpinner(),
             HTML('<hr style="border-color: #0088cc;">')
    )
  )
)