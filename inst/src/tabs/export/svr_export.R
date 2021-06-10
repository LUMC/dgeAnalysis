
## Check if data is available
observe({
  checkReload()
  if (exists("inUse_deTab")) {
    shinyjs::enable("downloadTSV")
    shinyjs::enable("downloadCSV")
  } else {
    shinyjs::disable("downloadTSV")
    shinyjs::disable("downloadCSV")
  }
  
  if (exists("enrich") && !exists("inUse_deTab") && input$dataset_select == "enrichment") {
    shinyjs::enable("downloadTSV")
    shinyjs::enable("downloadCSV")
  }
  
  if (exists("deTab")) {
    shinyjs::enable("downloadDGE_HTML")
  } else {
    shinyjs::disable("downloadDGE_HTML")
  }
  
  if (exists("enrich")) {
    shinyjs::enable("downloadENRICH_HTML")
  } else {
    shinyjs::disable("downloadENRICH_HTML")
  }
})

## get the right dataset for download
datasetInput <- reactive({
  checkReload()
  switch(
    input$dataset_select,
    "normCounts" = 2 ^ (inUse_normDge$counts),
    "normCountslog2" = inUse_normDge$counts,
    "deTab" = inUse_deTab,
    "deg" = inUse_deTab[inUse_deTab$DE != 0, ],
    "pcGene" = pc_gene_table(),
    "filtered" = filter_deTab(),
    "enrichment" = clean_enrich()
  )
})

## create filename and save data as CSV
output$downloadCSV <- downloadHandler(
  filename = function() {
    paste0("dgeAnalysis_", gsub(" ", "_", tolower(input$dataset_select)), ".csv")
  },
  content = function(file) {
    write.table(
      datasetInput(),
      file,
      row.names = TRUE,
      col.names = NA,
      sep = ",",
      quote = FALSE
    )
  }
)

## create filename and save data as TSV
output$downloadTSV <- downloadHandler(
  filename = function() {
    paste0("dgeAnalysis_", gsub(" ", "_", tolower(input$dataset_select)), ".tsv")
  },
  content = function(file) {
    write.table(
      datasetInput(),
      file,
      row.names = TRUE,
      col.names = NA,
      sep = "\t",
      quote = FALSE
    )
  }
)

## Download markdown HTML report DGE
output[["downloadDGE_HTML"]] <- downloadHandler(
  filename = function() {
    paste0(input$analysis_method, '_', gsub('-', '', Sys.Date()),'.html')
  },
  content <- function(file) {
    file.copy(paste0('markdown/', input$analysis_method, '.html'), file)
  }
)

## Download markdown HTML report Enrichment
output[["downloadENRICH_HTML"]] <- downloadHandler(
  filename = function() {
    paste0('gprofiler_', gsub('-', '', Sys.Date()), '.html')
  },
  content <- function(file) {
    file.copy(paste0('markdown/enrichmentGProfiler2.html'), file)
  }
)
