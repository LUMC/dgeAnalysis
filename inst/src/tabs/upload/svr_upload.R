
## Change application mode New <-> View
observeEvent(input$app_mode, {
  if (input$app_mode == "new") {
    showNotification(ui = "New analysis mode selected!",
                     duration = 3,
                     type = "message")
    runjs(
      '$("#show_view_analysis").css({"border-radius": "25px","border": "3px solid rgba(0,0,0,0)"});'
    )
    runjs(
      '$("#show_new_analysis").css({"border-radius": "25px", "border": "3px solid #0088cc"});'
    )
    tryCatch({
      if (exists("normDge")) {
        inUse_normDge <<- normDge
      } else {
        inUse_normDge <<- data.frame()
      }
      if (exists("deTab")) {
        inUse_deTab <<- deTab
      } else {
        inUse_deTab <<- data.frame()
      }
    }, error = function(err) {
      return(NULL)
    })
  } else {
    showNotification(ui = "View analysis mode selected!",
                     duration = 3,
                     type = "message")
    runjs(
      '$("#show_new_analysis").css({"border-radius": "25px", "border": "3px solid rgba(0,0,0,0)"});'
    )
    runjs(
      '$("#show_view_analysis").css({"border-radius": "25px", "border": "3px solid #0088cc"});'
    )
    inUse_normDge <<- get_normDge()
    inUse_deTab <<- data_detab()
  }
  toggle("hide_new_analysis")
  toggle("hide_view_analysis")
  toggle("new_tabs")
  toggle("view_tabs")
}, ignoreInit = TRUE)

## --------------------------------------------------------------------------

## ----- Read files -----

## Read data files
readFiles <- function(input) {
  df <- read.table(
    file = input,
    row.names = 1,
    header = TRUE,
    sep = "\t",
    check.names = FALSE
  )
  if (ncol(df) < 1) {
    df <- read.table(
      file = input,
      row.names = 1,
      header = TRUE,
      sep = ",",
      check.names = FALSE
    )
  }
  return(df)
}

## Read sample data file
data_samples <- reactive({
  if (input$app_mode == "new") {
    if (is.null(input$file_samples)) {
      return(NULL)
    }
    data_samples <- readFiles(input = input$file_samples$datapath)
  }
  if (input$app_mode == "view") {
    if (is.null(input$file_samples_view)) {
      return(NULL)
    }
    data_samples <- readFiles(input = input$file_samples_view$datapath)
  }
  data_samples
})

## Read count data file
data_counts <- reactive({
  if (input$app_mode == "new") {
    if (is.null(input$file_counts)) {
      return(NULL)
    }
    data_counts <- readFiles(input = input$file_counts$datapath)
  }
  if (input$app_mode == "view") {
    if (is.null(input$file_counts_view)) {
      return(NULL)
    }
    data_counts <- readFiles(input = input$file_counts_view$datapath)
  }
  data_counts
})

## Read annotation data file
data_annotation <- reactive({
  if (is.null(input$file_annotation)) {
    return(NULL)
  }
  data_annotation <- readFiles(input = input$file_annotation$datapath)
  data_annotation
})

## Read normalized data file
data_norm <- reactive({
  if (is.null(input$file_norm_view)) {
    return(NULL)
  }
  data_norm <- readFiles(input = input$file_norm_view$datapath)
  data_norm
})

## Read detab data file
data_detab <- reactive({
  if (is.null(input$file_detab_view)) {
    return(NULL)
  }
  data_detab <- readFiles(input = input$file_detab_view$datapath)
  data_detab
})

## --------------------------------------------------------------------------

## ----- Create data objects -----

## Create normDge from table
get_normDge <- reactive({
  tryCatch({
    se <- readCountsFromTable(data_norm(),
                              data_samples())
    se <- addSamplesFromTableToSE(se, data_samples())
    normDge <- tryCatch({
      temp <- DGEList(counts = assay(se), samples = colData(se))
      temp$counts <- log2(temp$counts)
      return(temp)
    }, error = function(err) {
      temp <- DGEList(counts = 2 ^ (assay(se)), samples = colData(se))
      temp$counts <- log2(temp$counts)
      return(temp)
    })
    normDge
  }, error = function(err) {
    return(NULL)
  })
})

## Create raw dge from raw count table
get_raw_dge <- reactive({
  data_counts <- data_counts()
  data_counts <- data_counts[!grepl('^__', rownames(data_counts)), ]
  se <- readCountsFromTable(data_counts,
                            data_samples())
  se <- addSamplesFromTableToSE(se, data_samples())
  if (!is.null(data_annotation())) {
    se <- addAnnotationsFromTableToSE(se, data_annotation())
  }
  dge <- DGEList(counts = assay(se),
                 samples = colData(se),
                 genes = rowData(se))
  dge <- dge[rowSums(abs(dge$counts)) > 1,]
  dge$counts <- cpm(dge, log = TRUE, prior.count = 1)
  dge
})

## --------------------------------------------------------------------------

## ----- New analysis files -----

## Render sample data to ui
output[["sample_data"]] <- DT::renderDataTable({
  tryCatch({
    DT::datatable(data_samples(), options = list(pageLength = 50, scrollX = TRUE))
  }, error = function(err) {
    return(DT::datatable(data.frame(c(
      "No data available in table"
    )), rownames = FALSE, colnames = ""))
  })
})

## Render count data to ui
output[["count_data"]] <- DT::renderDataTable({
  tryCatch({
    DT::datatable(data_counts(), options = list(pageLength = 50, scrollX = TRUE))
  }, error = function(err) {
    return(DT::datatable(data.frame(c(
      "No data available in table"
    )), rownames = FALSE, colnames = ""))
  })
})

## Render annotation data to ui
output[["annotation_data"]] <- DT::renderDataTable({
  tryCatch({
    DT::datatable(data_annotation(), options = list(pageLength = 50, scrollX = TRUE))
  }, error = function(err) {
    return(DT::datatable(data.frame(c(
      "No data available in table"
    )), rownames = FALSE, colnames = ""))
  })
})

## --------------------------------------------------------------------------

## ----- View analysis files -----

## Render sample data to ui
output[["sample_data_view"]] <- DT::renderDataTable({
  tryCatch({
    DT::datatable(data_samples(), options = list(pageLength = 50, scrollX = TRUE))
  }, error = function(err) {
    return(DT::datatable(data.frame(c(
      "No data available in table"
    )), rownames = FALSE, colnames = ""))
  })
})

## Render count data to ui
output[["count_data_view"]] <- DT::renderDataTable({
  tryCatch({
    DT::datatable(data_counts(), options = list(pageLength = 50, scrollX = TRUE))
  }, error = function(err) {
    return(DT::datatable(data.frame(c(
      "No data available in table"
    )), rownames = FALSE, colnames = ""))
  })
})

## Render normalized data to ui
output[["norm_data_view"]] <- DT::renderDataTable({
  tryCatch({
    DT::datatable(data_norm(), options = list(pageLength = 50, scrollX = TRUE))
  }, error = function(err) {
    return(DT::datatable(data.frame(c(
      "No data available in table"
    )), rownames = FALSE, colnames = ""))
  })
})

## Render normalized data to ui
output[["detab_data_view"]] <- DT::renderDataTable({
  tryCatch({
    DT::datatable(data_detab(), options = list(pageLength = 50, scrollX = TRUE))
  }, error = function(err) {
    return(DT::datatable(data.frame(c(
      "No data available in table"
    )), rownames = FALSE, colnames = ""))
  })
})
