
## Read sample data file ##
data_samples <- reactive({
  if (is.null(input$file_samples$datapath)){return(NULL)}
  data_samples <- read.csv(input$file_samples$datapath, row.names=1, header = TRUE, sep = "\t", check.names = FALSE)
  if (ncol(data_samples) < 1) {
    data_samples <- read.csv(input$file_samples$datapath, row.names=1, header = TRUE, check.names = FALSE)
  }
  data_samples
})

## Render sample data to ui
output[["sample_data"]] <- DT::renderDataTable({
  tryCatch({
    DT::datatable(data_samples(), options = list(pageLength = 50, scrollX = TRUE))
  }, error = function(err) {
    return(NULL)
  })
})

## Read count data file ##
data_counts <- reactive({
  if (is.null(input$file_counts$datapath)){return(NULL)}
  data_counts <- read.csv(input$file_counts$datapath, row.names=1, header = TRUE, sep = "\t", check.names = FALSE)
  if (ncol(data_counts) < 1) {
    data_counts <- read.csv(input$file_counts$datapath, row.names=1, header = TRUE, check.names = FALSE)
  }
  data_counts
})

## Render count data to ui
output$count_data <- DT::renderDataTable({
  tryCatch({
    DT::datatable(data_counts(), options = list(pageLength = 50, scrollX = TRUE))
  }, error = function(err) {
    return(NULL)
  })
})