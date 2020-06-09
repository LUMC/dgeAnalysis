#' Start dgeAnalysis application.
#'
#' @return Opens application in default web browser
#' 
#' @export

startApp <- function() {
  appDir <- system.file("src", package = "dgeAnalysis")
  
  if (appDir == "") {
    stop("Could not find 'dgeAnalysis'. Try re-installing 'dgeAnalysis'.", call. = FALSE)
  }
  
  library("dgeAnalysis")
  options(shiny.maxRequestSize = 50*1024^2)
  options(spinner.color = "#0088cc")
  options(warn = -1)
  
  message("Initializing dgeAnalysis...")
  suppressWarnings(rm(list=c("deTab", "normDge", "inUse_deTab", "inUse_normDge"), envir=.GlobalEnv))
  suppressMessages(source(system.file("src/lib/libraries.R", package = "dgeAnalysis")))
  
  shiny::runApp(appDir, host="0.0.0.0", port=1402, launch.browser=TRUE)
}
