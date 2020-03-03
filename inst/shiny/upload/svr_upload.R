
observeEvent(input$switch_new, {
  toggleState("switch_new")
  toggleState("switch_view")
  runjs('$("#show_new_analysis").css({"border-radius": "25px", "border": "3px solid #0088cc"});')
  runjs('$("#show_view_analysis").css({"border": "3px solid rgba(0,0,0,0)"});')
  toggle("hide_new_analysis")
  toggle("hide_view_analysis")
  toggle("new_tabs")
  toggle("view_tabs")
})

observeEvent(input$switch_view, {
  toggleState("switch_new")
  toggleState("switch_view")
  runjs('$("#show_new_analysis").css({"border": "3px solid rgba(0,0,0,0)"});')
  runjs('$("#show_view_analysis").css({"border-radius": "25px", "border": "3px solid #0088cc"});')
  toggle("hide_new_analysis")
  toggle("hide_view_analysis")
  toggle("new_tabs")
  toggle("view_tabs")
})


## --------------------------------------------------------------------------

## ----- New analysis files -----


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
output[["count_data"]] <- DT::renderDataTable({
  tryCatch({
    DT::datatable(data_counts(), options = list(pageLength = 50, scrollX = TRUE))
  }, error = function(err) {
    return(NULL)
  })
})

## Read annotation data file ##
data_annotation <- reactive({
  if (is.null(input$file_annotation$datapath)){return(NULL)}
  data_annotation <- read.csv(input$file_annotation$datapath, row.names=1, header = TRUE, sep = "\t", check.names = FALSE)
  if (ncol(data_annotation) < 1) {
    data_annotation <- read.csv(input$file_annotation$datapath, row.names=1, header = TRUE, check.names = FALSE)
  }
  data_annotation
})

## Render annotation data to ui
output[["annotation_data"]] <- DT::renderDataTable({
  tryCatch({
    DT::datatable(data_annotation(), options = list(pageLength = 50, scrollX = TRUE))
  }, error = function(err) {
    return(NULL)
  })
})


## --------------------------------------------------------------------------

## ----- View analysis files -----

## Read sample data file ##
data_samples_view <- reactive({
  if (is.null(input$file_samples_view$datapath)){return(NULL)}
  data_samples_view <- read.csv(input$file_samples_view$datapath, row.names=1, header = TRUE, sep = "\t", check.names = FALSE)
  if (ncol(data_samples_view) < 1) {
    data_samples_view <- read.csv(input$file_samples_view$datapath, row.names=1, header = TRUE, check.names = FALSE)
  }
  data_samples_view
})

## Render sample data to ui
output[["sample_data_view"]] <- DT::renderDataTable({
  tryCatch({
    DT::datatable(data_samples_view(), options = list(pageLength = 50, scrollX = TRUE))
  }, error = function(err) {
    return(NULL)
  })
})

## Read count data file ##
data_counts_view <- reactive({
  if (is.null(input$file_counts_view$datapath)){return(NULL)}
  data_counts_view <- read.csv(input$file_counts_view$datapath, row.names=1, header = TRUE, sep = "\t", check.names = FALSE)
  if (ncol(data_counts_view) < 1) {
    data_counts_view <- read.csv(input$file_counts_view$datapath, row.names=1, header = TRUE, check.names = FALSE)
  }
  data_counts_view
})

## Render count data to ui
output[["count_data_view"]] <- DT::renderDataTable({
  tryCatch({
    DT::datatable(data_counts_view(), options = list(pageLength = 50, scrollX = TRUE))
  }, error = function(err) {
    return(NULL)
  })
})

## Read normalized data file ##
data_norm_view <- reactive({
  if (is.null(input$file_norm_view$datapath)){return(NULL)}
  data_norm_view <- read.csv(input$file_norm_view$datapath, row.names=1, header = TRUE, sep = "\t", check.names = FALSE)
  if (ncol(data_norm_view) < 1) {
    data_norm_view <- read.csv(input$file_norm_view$datapath, row.names=1, header = TRUE, check.names = FALSE)
  }
  data_norm_view
})

## Render normalized data to ui
output[["norm_data_view"]] <- DT::renderDataTable({
  tryCatch({
    DT::datatable(data_norm_view(), options = list(pageLength = 50, scrollX = TRUE))
  }, error = function(err) {
    return(NULL)
  })
})

## Read normalized data file ##
data_detab_view <- reactive({
  if (is.null(input$file_detab_view$datapath)){return(NULL)}
  data_detab_view <- read.csv(input$file_detab_view$datapath, row.names=1, header = TRUE, sep = "\t", check.names = FALSE)
  if (ncol(data_detab_view) < 1) {
    data_detab_view <- read.csv(input$file_detab_view$datapath, row.names=1, header = TRUE, check.names = FALSE)
  }
  data_detab_view
})

## Render normalized data to ui
output[["detab_data_view"]] <- DT::renderDataTable({
  tryCatch({
    DT::datatable(data_detab_view(), options = list(pageLength = 50, scrollX = TRUE))
  }, error = function(err) {
    return(NULL)
  })
})
