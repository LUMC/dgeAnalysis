#' @export

startApp <- function() {
  appDir <- system.file("src", package = "dgeAnalysis")
  
  if (appDir == "") {
    stop("Could not find 'dgeAnalysis'. Try re-installing 'dgeAnalysis'.", call. = FALSE)
  }
  
  options(shiny.maxRequestSize = 50*1024^2)
  options(spinner.color = "#0088cc")
  options(warn = -1)
  
  suppressWarnings(rm(list=c("deTab", "normDge", "inUse_deTab", "inUse_normDge"), envir=.GlobalEnv))
  suppressMessages(source(system.file("src/scripts/libraries.R", package = "dgeAnalysis")))
  
  source(system.file("src/scripts/de.R", package = "dgeAnalysis"), local = TRUE)
  source(system.file("src/scripts/enrichment.R", package = "dgeAnalysis"), local = TRUE)
  source(system.file("src/scripts/markdown.R", package = "dgeAnalysis"), local = TRUE)
  source(system.file("src/scripts/plots.R", package = "dgeAnalysis"), local = TRUE)
  
  shiny::runApp(appDir, host="0.0.0.0", port=1402, launch.browser=TRUE)
}
