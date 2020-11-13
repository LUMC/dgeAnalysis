
## Check if data is available
observe({
  checkReload()
  if (exists("deTab")) {
    shinyjs::enable("downloadDGE_HTML")
    shinyjs::enable("downloadTSV")
    shinyjs::enable("downloadCSV")
  } else {
    shinyjs::disable("downloadDGE_HTML")
    shinyjs::disable("downloadTSV")
    shinyjs::disable("downloadCSV")
  }
  
  if (exists("enrich") && !exists("deTab") && input$dataset_select == "enrichment") {
    shinyjs::enable("downloadTSV")
    shinyjs::enable("downloadCSV")
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
    paste("shiny_analysis_", gsub(" ", "_", tolower(input$dataset_select)), ".csv", sep = "")
  },
  content = function(file) {
    write.table(
      datasetInput(),
      file,
      row.names = TRUE,
      col.names = NA,
      sep = ","
    )
  }
)

## create filename and save data as TSV
output$downloadTSV <- downloadHandler(
  filename = function() {
    paste("shiny_analysis_", gsub(" ", "_", tolower(input$dataset_select)), ".tsv", sep = "")
  },
  content = function(file) {
    write.table(
      datasetInput(),
      file,
      row.names = TRUE,
      col.names = NA,
      sep = "\t"
    )
  }
)

## Download markdown HTML report DGE
output[["downloadDGE_HTML"]] <- downloadHandler(
  filename = paste(input$analysis_method,
                   '_',
                   gsub('-', '', Sys.Date()),
                   '.html',
                   sep = ''),
  content <- function(file) {
    file.copy(paste0('markdown/', input$analysis_method, '.html'), file)
  }
)

## Download markdown HTML report Enrichment
output[["downloadENRICH_HTML"]] <- downloadHandler(
  filename = paste('gprofiler_',
                   gsub('-', '', Sys.Date()),
                   '.html',
                   sep = ''),
  content <- function(file) {
    file.copy(paste0('markdown/enrichmentGProfiler2.html'), file)
  }
)
