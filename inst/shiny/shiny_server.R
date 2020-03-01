
server <- function(input, output, session) {
  
  checkReload <- function(){
    is.null(input$design_value)
    is.null(input$matrix_value)
    is.null(input$cpm_value)
    is.null(input$alpha_value)
    is.null(input$setGeneName)
    is.null(input$analysis_method)
    is.null(input$sample_data_rows_selected)
    is.null(input$choose_analysis)
    is.null(input$run_button)
  }

  output[["sidebar_tabs"]] <- renderMenu({
    sidebarMenu(id = "sidebar",
      menuItem(
        "Home", tabName = "home",
        icon = icon("binoculars")
      ),
      menuItem(
        "Data upload", tabName = "upload",
        icon = icon("upload")
      ),
      menuItem(
        "Run Analysis", tabName = "run_analysis",
        icon = icon("upload")
      ),
      menuItem(
        "Alignment", tabName = "alignment",
        icon = icon("upload")
      )
    )
  })

  
  source("shiny/home/server.R", local = TRUE)
  source("shiny/upload/server.R", local = TRUE)
  source("shiny/run_analysis/server.R", local = TRUE)
  source("shiny/alignment/server.R", local = TRUE)

}
