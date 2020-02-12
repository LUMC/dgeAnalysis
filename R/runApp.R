#' @export
startApp <- function() {
  appDir <- system.file("Application", package = "dgeAnalysis")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `dgeAnalysis`.", call. = FALSE)
  }
  
  suppressMessages(source(system.file("Application/librarys.R", package = "dgeAnalysis")))
  shiny::runApp(appDir, host="0.0.0.0", port=1402, launch.browser=TRUE)
}
