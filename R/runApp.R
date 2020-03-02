#' @export
#' @return Shiny application.
#' @import shiny
#' @import shinydashboard
#' @import shinyWidgets
#' @import shinycssloaders
#' @import shinyjs
#' #' @examples
#' if ( interactive() ) {
#'   startApp()
#' }
startApp <- function() {
  print(system.file(
    'shiny/mainUI.R',
    package = "dgeAnalysis"
  ))
  source(
    system.file(
      'shiny/mainUI.R',
      package = "dgeAnalysis"
      ),
    local = TRUE
  )
  print("test1")
  source(
    system.file(
      'shiny/mainServer.R',
      package = "dgeAnalysis"),
    local = TRUE
  )
  print("test2")
  
  shiny::shinyApp(ui = ui, server = server)
  
  #shiny::runApp(appDir, host="0.0.0.0", port=1402, launch.browser=TRUE)
}
