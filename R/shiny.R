
#' Start dgeAnalysis application.
#'
#' @return Opens application in default web browser
#'
#' @export

startApp <- function(launch.browser = TRUE) {
  appDir <- system.file("src", package = "dgeAnalysis")
  
  if (appDir == "") {
    stop("Could not find 'dgeAnalysis'. Try re-installing 'dgeAnalysis'.",
         call. = FALSE)
  }
  
  require("dgeAnalysis")
  options(shiny.maxRequestSize = 100 * 1024 ^ 2)
  options(spinner.color = "#0088cc")
  options(warn = -1)
  
  message("Initializing dgeAnalysis...")
  suppressWarnings(rm(
    list = c(
      "deTab",
      "normDge",
      "enrich",
      "inUse_deTab",
      "inUse_normDge",
      "inUse_enrich"
    ),
    envir = .GlobalEnv
  ))
  suppressMessages(source(system.file("src/lib/libraries.R", package = "dgeAnalysis")))
  
  shiny::runApp(
    appDir = appDir,
    host = "0.0.0.0",
    port = 1402,
    launch.browser = launch.browser
  )
}


#' Template for plot information.
#'
#' @param infoText String, Explanation of a plot
#'
#' @return Shiny Box object
#'
#' @export

informationBox <- function(infoText) {
  tryCatch({
    box(
      title = icon("circle-info"),
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      collapsed = FALSE,
      span(infoText,
           style = "padding-left: 5px; text-align: justify; display: block;"),
      style = "padding-left: unset;"
    )
  }, error = function(err) {
    return(NULL)
  })
}
