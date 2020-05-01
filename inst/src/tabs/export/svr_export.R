
## get the right dataset for download
datasetInput <- reactive({
  checkReload()
  switch(input$dataset_select,
         "normCounts" = 2^(inUse_normDge$counts),
         "normCountslog2" = inUse_normDge$counts,
         "deTab" = inUse_deTab,
         "deg" = inUse_deTab[inUse_deTab$DE != 0,],
         "pcGene" = pc_gene_table(),
         "kegg" = as.data.frame(get_kegg())[,!(colnames(as.data.frame(get_kegg())) %in% c("ID", "leading_edge", "core_enrichment", "geneID"))],
         "reactome" = as.data.frame(get_reactome())[,!(colnames(as.data.frame(get_reactome())) %in% c("ID", "leading_edge", "core_enrichment", "geneID"))],
         "go" = as.data.frame(get_go())[,!(colnames(as.data.frame(get_go())) %in% c("ID", "leading_edge", "core_enrichment", "geneID"))],
         "do" = as.data.frame(get_do())[,!(colnames(as.data.frame(get_do())) %in% c("ID", "leading_edge", "core_enrichment", "geneID"))])
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

## Download markdown HTML report
output[["downloadMHTML"]] <- downloadHandler(
  filename = paste(input$analysis_method, '_', gsub('-', '', Sys.Date()), '.html', sep=''),
  content <- function(file) {
    file.copy(paste0("markdown/", input$analysis_method, '.html'), file)
  }
)
