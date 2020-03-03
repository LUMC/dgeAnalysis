
## get the right dataset for download
datasetInput <- reactive({
  checkReload()
  switch(input$dataset_select,
         "normCounts" = inUse_normDge$counts,
         "deTab" = inUse_deTab,
         "deg" = inUse_deTab[inUse_deTab$DE != 0,],
         "kegg" = as.data.frame(get_kegg())[ , -c(1, 10, 11)],
         "reactome" = as.data.frame(get_reactome())[ , -c(1, 10, 11)],
         "go" = as.data.frame(get_go())[ , -c(1, 10, 11)],
         "do" = as.data.frame(get_do())[ , -c(1, 10, 11)])
})

## create filename and save data as CSV
output$downloadCSV <- downloadHandler(
  filename = function() {
    paste("shiny_analysis_", gsub(" ", "_", tolower(input$dataset_select)), ".csv", sep = "")
  },
  content = function(file) {
    write.table(datasetInput(), file, row.names = TRUE, col.names=NA, sep = ",")
  }
)

## create filename and save data as TSV
output$downloadTSV <- downloadHandler(
  filename = function() {
    paste("shiny_analysis_", gsub(" ", "_", tolower(input$dataset_select)), ".tsv", sep = "")
  },
  content = function(file) {
    write.table(datasetInput(), file, row.names = TRUE, col.names=NA, sep = "\t")
  }
)

## create filename and save data as XLSX
output$downloadXLSX <- downloadHandler(
  filename = function() {
    paste("shiny_analysis_", gsub(" ", "_", tolower(input$dataset_select)), ".xlsx", sep = "")
  },
  content = function(file) {
    write.table(datasetInput(), file, row.names = TRUE, col.names=NA)
  }
)

output[["downloadMHTML"]] <- downloadHandler(
  filename = paste(input$analysis_method, '_', gsub('-', '', Sys.Date()), '.html', sep=''),
  content <- function(file) {
    file.copy(paste("markdown/",input$analysis_method, '.html', sep=''), file)
  }
)
