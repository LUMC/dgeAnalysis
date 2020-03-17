
## Start an analysis
observeEvent(input$run_button, {
  if (input$app_mode != "new") {
    showNotification(ui = 'Wrong analysis mode!', duration = 5, type = "error")
    return(NULL)
  }
  
  showModal(modalDialog("Analysis is running...",
                        footer=NULL))
  results <- tryCatch({
    rmarkdown::render(
      paste("markdown/", input$analysis_method, ".Rmd", sep=''),
      params = list(
        data_samples = data_samples(),
        data_counts = data_counts(),
        data_annotation = data_annotation(),
        excluded_samples = input$exclude_samples,
        setGeneName = input$setGeneName,
        cpm_value = input$cpm_value,
        design_base = input$design_base,
        design_value = input$design_value,
        matrix_v1 = input$matrix_val1,
        matrix_v2 = input$matrix_val2,
        alpha = input$alpha_value),
      output_file = paste(input$analysis_method, '.html', sep=''))
    load("markdown/analysis.RData", envir=.GlobalEnv)
    inUse_deTab <<- deTab
    inUse_normDge <<- normDge
    showNotification(ui = "Analysis has been succesful!", duration = 10, type = "message")
  }, error = function(err) {
    showNotification(ui = "The analysis failed with an error!", duration = 10, type = "error")
    print(err)
    return(NULL)
  }
  )
  removeModal()
},ignoreInit = TRUE)

output[["design_base"]] <- renderUI({
  tryCatch({
    if (is.null(input$file_samples)){return(NULL)}
    
    selectInput("design_base", "Set base column for comparison:",
                colnames(data_samples())
    )
  }, error = function(err) {
    return(NULL)
  })
})

output[["design_value"]] <- renderUI({
  tryCatch({
    if (is.null(input$design_base)){return(NULL)}
    checkboxGroupInput("design_value",
                       "Select columns for comparison (pairwise analysis):",
                       choices = colnames(data_samples())[!colnames(data_samples()) %in% input$design_base],
                       inline = TRUE
    )
  }, error = function(err) {
    return(NULL)
  })
})

AllInputs <- reactive({
  x <- reactiveValuesToList(input)
})

output[["matrix"]] <- renderUI({
  if (!is.null(input$vs_mode)){
    fluidRow(
      column(
        width = 5,
        uiOutput("matrix_value1")
      ),
      column(
        width = 2,
        h2("VS")
      ),
      column(
        width = 5,
        uiOutput("matrix_value2")
      )
    )
  } else {
    uiOutput("matrix_value2")
  }
})

output[["matrix_value1"]] <- renderUI({
  columns <- c(input$design_base, input$design_value)
  
  selectInput(inputId = "matrix_val1",
              label = "Select values for comparison:",
              multiple = TRUE,
              selected = AllInputs()[["matrix_val1"]],
              choices = unique(data_samples()[columns])
  )
})

output[["matrix_value2"]] <- renderUI({
  columns <- c(input$design_base, input$design_value)
  
  selectInput(inputId = "matrix_val2",
              label = "Select values for comparison:",
              multiple = TRUE,
              selected = AllInputs()[["matrix_val2"]],
              choices = unique(data_samples()[columns])
  )
})

output[["show_design"]] <- renderUI({
  if (!is.null(input$vs_mode)) {
    design <- createDesign(data_samples(), input$design_base, input$design_value, input$matrix_val1, input$matrix_val2)
  } else {
    design <- createDesign(data_samples(), input$design_base, input$design_value, NULL, input$matrix_val2)
  }
  design
})

output[["show_matrix"]] <- renderUI({
  if (!is.null(input$vs_mode)) {
    tagList(
      "Find genes that respond to:",
      matrix_vs_mode()
    )
  } else {
    tagList(
      "Find genes that respond diffently to:",
      matrix_single()
    )
  }
})

matrix_single <- reactive({
  columns <- c(input$design_base, input$design_value)
  
  total_matrix2 <- NULL
  for (column in columns) {
    temp2 <- NULL
    for (value in input$matrix_val2) {
      if (grepl(value, data_samples()[column])) {
        temp2 <- c(temp2, value)
      }
    }
    if (!is.null(temp2)) {
      total_matrix2 <- c(total_matrix2, paste(temp2, collapse = ", "))
    }
  }
  total_matrix2 <- paste(total_matrix2, collapse = " and ")
})

matrix_vs_mode <- reactive({
  columns <- c(input$design_base, input$design_value)
  
  total_matrix1 <- NULL
  total_matrix2 <- NULL
  for (column in columns) {
    temp1 <- NULL
    temp2 <- NULL
    for (value in input$matrix_val1) {
      if (grepl(value, data_samples()[column])) {
        temp1 <- c(temp1, value)
      }
    }
    for (value in input$matrix_val2) {
      if (grepl(value, data_samples()[column])) {
        temp2 <- c(temp2, value)
      }
    }
    if (!is.null(temp1)) {
      total_matrix1 <- c(total_matrix1, paste(temp1, collapse = ", "))
    }
    if (!is.null(temp2)) {
      total_matrix2 <- c(total_matrix2, paste(temp2, collapse = ", "))
    }
  }
  total_matrix1 <- paste(total_matrix1, collapse = " and ")
  total_matrix2 <- paste(total_matrix2, collapse = " and ")
  if (total_matrix1 == total_matrix2 && total_matrix1 != "") {
    showNotification(ui = "This is not a good comparison!", duration = 3, type = "message")
  }
  total_matrix <- paste(total_matrix1, total_matrix2, sep = " VS ")
})

output[["excludeSamples"]] <- renderUI({
  tryCatch({
    box(
      title = "Exclude samples",
      status = "primary",
      solidHeader = TRUE,
      width = 10,
      collapsible = TRUE,
      collapsed = TRUE,
      checkboxGroupInput(
        inputId = "exclude_samples",
        label = "",
        inline = TRUE,
        choices = rownames(data_samples())
      )
    )
  }, error = function(err) {
    return(NULL)
  })
})

output[["setGeneName"]] <- renderUI({
  tryCatch({
    if (is.null(input$file_annotation)) {
      return(NULL)
    }
    
    radioButtons("setGeneName", "Use gene ID or gene symbols:",
                 inline = TRUE,
                 c("Gene ID" = "id",
                   "Gene Symbol" = "symbol")
    )
  }, error = function(err) {
    return(NULL)
  })
})
