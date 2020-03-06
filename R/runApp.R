#' @export
#' @return Shiny application.
#' @import shiny
#' @import shinydashboard
#' @import shinyWidgets
#' @import shinycssloaders
#' @import shinyjs
#' @import BiocManager
#' @import knitr
#' @import SummarizedExperiment
#' @import edgeR
#' @import limma
#' @import DESeq2
#' @import tidyr
#' @import scales
#' @import broom
#' @import plotly
#' @import clusterProfiler
#' @import DOSE
#' @import graphite
#' @import ReactomePA
#' @import igraph
#' @import org.Hs.eg.db
#' @import org.Mm.eg.db
#' @import reshape2
#' 
#' #' @examples
#' if ( interactive() ) {
#'   startApp()
#' }
startApp <- function() {
  source("shiny/ui_main.R", local = TRUE)
  source("shiny/svr_main.R", local = TRUE)
  source("scripts/de.R", local = TRUE)
  source("scripts/enrichment.R", local = TRUE)
  source("scripts/markdown.R", local = TRUE)
  source("scripts/plots.R", local = TRUE)
  
  options(shiny.maxRequestSize = 50*1024^2)
  options(spinner.color="#0088cc")
  # suppressWarnings(rm(list=c("deTab", "normDge", "inUse_deTab", "inUse_normDge"), envir=.GlobalEnv))
  # options(warn = -1)
  
  shiny::shinyApp(ui = ui, server = server, options=list(host="0.0.0.0", port=1402, launch.browser=TRUE))
}
