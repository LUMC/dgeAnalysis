
output[["downloadMHTML"]] <- downloadHandler(
  filename = paste(input$analysis_method, '_', gsub('-', '', Sys.Date()), '.html', sep=''),
  content <- function(file) {
    file.copy(paste(input$analysis_method, '.html', sep=''), file)
  }
)
