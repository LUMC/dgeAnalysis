
## Change application mode New <-> View
observeEvent(input$app_mode, {
  if (input$app_mode == "new") {
    runjs('$("#show_view_analysis").css({"border-radius": "25px","border": "3px solid rgba(0,0,0,0)"});')
    runjs('$("#show_new_analysis").css({"border-radius": "25px", "border": "3px solid #0088cc"});')
    tryCatch({
      inUse_normDge <<- normDge
      inUse_deTab <<- deTab
    }, error = function(err) {
      return(NULL)
    })
  } else {
    runjs('$("#show_new_analysis").css({"border-radius": "25px", "border": "3px solid rgba(0,0,0,0)"});')
    runjs('$("#show_view_analysis").css({"border-radius": "25px", "border": "3px solid #0088cc"});')
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

## Read sample data file
data_samples <- reactive({
  if (input$app_mode == "new") {
    if (is.null(input$file_samples$datapath)){return(NULL)}
    data_samples <- read.csv(input$file_samples$datapath, row.names=1, header = TRUE, sep = "\t", check.names = FALSE)
    if (ncol(data_samples) < 1) {
      data_samples <- read.csv(input$file_samples$datapath, row.names=1, header = TRUE, check.names = FALSE)
    }
  }
  if (input$app_mode == "view") {
    if (is.null(input$file_samples_view$datapath)){return(NULL)}
    data_samples <- read.csv(input$file_samples_view$datapath, row.names=1, header = TRUE, sep = "\t", check.names = FALSE)
    if (ncol(data_samples) < 1) {
      data_samples <- read.csv(input$file_samples_view$datapath, row.names=1, header = TRUE, check.names = FALSE)
    }
  }
  data_samples
})

## Read count data file
data_counts <- reactive({
  if (input$app_mode == "new") {
    if (is.null(input$file_counts$datapath)){return(NULL)}
    data_counts <- read.csv(input$file_counts$datapath, row.names=1, header = TRUE, sep = "\t", check.names = FALSE)
    if (ncol(data_counts) < 1) {
      data_counts <- read.csv(input$file_counts$datapath, row.names=1, header = TRUE, check.names = FALSE)
    }
  }
  if (input$app_mode == "view") {
    if (is.null(input$file_counts_view$datapath)){return(NULL)}
    data_counts <- read.csv(input$file_counts_view$datapath, row.names=1, header = TRUE, sep = "\t", check.names = FALSE)
    if (ncol(data_counts) < 1) {
      data_counts <- read.csv(input$file_counts_view$datapath, row.names=1, header = TRUE, check.names = FALSE)
    }
  }
  data_counts
})

## Read annotation data file
data_annotation <- reactive({
  if (is.null(input$file_annotation$datapath)){return(NULL)}
  data_annotation <- read.csv(input$file_annotation$datapath, row.names=1, header = TRUE, sep = "\t", check.names = FALSE)
  if (ncol(data_annotation) < 1) {
    data_annotation <- read.csv(input$file_annotation$datapath, row.names=1, header = TRUE, check.names = FALSE)
  }
  data_annotation
})

## Read normalized data file
data_norm <- reactive({
  if (is.null(input$file_norm_view$datapath)){return(NULL)}
  data_norm <- read.csv(input$file_norm_view$datapath, row.names=1, header = TRUE, sep = "\t", check.names = FALSE)
  if (ncol(data_norm) < 1) {
    data_norm <- read.csv(input$file_norm_view$datapath, row.names=1, header = TRUE, check.names = FALSE)
  }
  data_norm
})

## Read detab data file
data_detab <- reactive({
  if (is.null(input$file_detab_view$datapath)){return(NULL)}
  data_detab <- read.csv(input$file_detab_view$datapath, row.names=1, header = TRUE, sep = "\t", check.names = FALSE)
  if (ncol(data_detab) < 1) {
    data_detab <- read.csv(input$file_detab_view$datapath, row.names=1, header = TRUE, check.names = FALSE)
  }
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
      return(DGEList(counts = assay(se), samples = colData(se)))
    }, error = function(err) {
      temp <- DGEList(counts = 2^(assay(se)), samples = colData(se))
      temp$counts <- log2(temp$counts)
      return(temp)
    }
    )
    normDge
  }, error = function(err) {
    return(NULL)
  })
})

## Create raw dge from raw count table
get_raw_dge <- reactive({
  data_counts <- data_counts()
  data_counts <- data_counts[!grepl('^__', rownames(data_counts)),]
  se <- readCountsFromTable(data_counts(),
                            data_samples())
  se <- addSamplesFromTableToSE(se, data_samples())
  if (!is.null(data_annotation())) {
    se <- addAnnotationsFromTableToSE(se, data_annotation())
  }
  dge <- DGEList(counts = assay(se), samples = colData(se), genes = rowData(se))
  dge <- dge[ rowSums( abs( dge$counts ) ) > 1, ]
  dge$counts <- cpm(dge, log = TRUE)
  dge
})

## --------------------------------------------------------------------------

## ----- New analysis files -----

## Render sample data to ui
output[["sample_data"]] <- DT::renderDataTable({
  tryCatch({
    DT::datatable(data_samples(), options = list(pageLength = 50, scrollX = TRUE))
  }, error = function(err) {
    return(DT::datatable(data.frame(c("No data available in table")), rownames = FALSE, colnames = ""))
  })
})

## Render count data to ui
output[["count_data"]] <- DT::renderDataTable({
  tryCatch({
    DT::datatable(data_counts(), options = list(pageLength = 50, scrollX = TRUE))
  }, error = function(err) {
    return(DT::datatable(data.frame(c("No data available in table")), rownames = FALSE, colnames = ""))
  })
})

## Render annotation data to ui
output[["annotation_data"]] <- DT::renderDataTable({
  tryCatch({
    DT::datatable(data_annotation(), options = list(pageLength = 50, scrollX = TRUE))
  }, error = function(err) {
    return(DT::datatable(data.frame(c("No data available in table")), rownames = FALSE, colnames = ""))
  })
})

## --------------------------------------------------------------------------

## ----- View analysis files -----

## Render sample data to ui
output[["sample_data_view"]] <- DT::renderDataTable({
  tryCatch({
    DT::datatable(data_samples(), options = list(pageLength = 50, scrollX = TRUE))
  }, error = function(err) {
    return(DT::datatable(data.frame(c("No data available in table")), rownames = FALSE, colnames = ""))
  })
})

## Render count data to ui
output[["count_data_view"]] <- DT::renderDataTable({
  tryCatch({
    DT::datatable(data_counts(), options = list(pageLength = 50, scrollX = TRUE))
  }, error = function(err) {
    return(DT::datatable(data.frame(c("No data available in table")), rownames = FALSE, colnames = ""))
  })
})

## Render normalized data to ui
output[["norm_data_view"]] <- DT::renderDataTable({
  tryCatch({
    DT::datatable(data_norm(), options = list(pageLength = 50, scrollX = TRUE))
  }, error = function(err) {
    return(DT::datatable(data.frame(c("No data available in table")), rownames = FALSE, colnames = ""))
  })
})

## Render normalized data to ui
output[["detab_data_view"]] <- DT::renderDataTable({
  tryCatch({
    DT::datatable(data_detab(), options = list(pageLength = 50, scrollX = TRUE))
  }, error = function(err) {
    return(DT::datatable(data.frame(c("No data available in table")), rownames = FALSE, colnames = ""))
  })
})
