
## Start an analysis
observeEvent(input$run_button, {
  if (!is.null(preMarkdownChecks())) {
    showNotification(ui = preMarkdownChecks(),
                     duration = 5,
                     type = "error")
    return(NULL)
  }
  
  showModal(modalDialog(
    h1("Analysis is running..."),
    img(src = "loading.gif", width = "50%"),
    footer = NULL
  ))
  
  results <- tryCatch({
    rmarkdown::render(
      input = paste0("markdown/", input$analysis_method, ".Rmd"),
      params = list(
        md5sum = getMD5(),
        data_samples = data_samples(),
        data_counts = data_counts(),
        data_annotation = data_annotation(),
        excluded_samples = input$exclude_samples,
        setGeneName = input$setGeneName,
        cpm_value = input$cpm_value,
        cpm_perc = input$cpm_perc,
        design_base = input$design_base,
        design_value = input$design_value,
        matrix_v1 = input$matrix_val1,
        matrix_v2 = input$matrix_val2,
        alpha = input$alpha_value
      ),
      output_file = paste0(input$analysis_method, '.html')
    )
    load("markdown/analysis.RData", envir = .GlobalEnv)
    inUse_deTab <<- deTab
    inUse_normDge <<- normDge
    showNotification(ui = "Analysis has been succesful!",
                     duration = 5,
                     type = "message")
  }, error = function(err) {
    showNotification(ui = "The analysis failed with an error!",
                     duration = 5,
                     type = "error")
    showNotification(ui = as.character(err),
                     duration = 10,
                     type = "error")
    print(err)
    return(NULL)
  })
  removeModal()
}, ignoreInit = TRUE)

## This reactive checks if input values are valid
preMarkdownChecks <- reactive ({
  if (input$app_mode == "view") {
    return("Wrong analysis mode!")
  } else if (is.null(input$matrix_val1) | is.null(input$matrix_val2)) {
    return("One of the contrast is empty!")
  } else if (all(sort(input$matrix_val1) == sort(input$matrix_val2))) {
    return("Contrasts cant be the same!")
  } else if (!is.null(input$setGeneName)) {
    if (input$setGeneName == "symbol" & !("geneName" %in% colnames(data_annotation()))) {
      return("The annotation file is missing a column: 'geneName'!")
    }
  } else if (is.null(input$file_counts)) {
    return("No expression data uploaded!")
  }
  return(NULL)
})

## Get MD5 info from uploaded files
getMD5 <- reactive({
  md5 <- tryCatch({
    c(
      paste0(input$file_samples$name, ": ", tools::md5sum(files = input$file_samples$datapath)),
      paste0(input$file_counts$name, ": ", tools::md5sum(files = input$file_counts$datapath)),
      paste0(input$file_annotation$name, ": ", tools::md5sum(files = input$file_annotation$datapath))
    )
  }, error = function(err) {
    c(
      paste0(input$file_samples$name, ": ", tools::md5sum(files = input$file_samples$datapath)),
      paste0(input$file_counts$name, ": ", tools::md5sum(files = input$file_counts$datapath))
    )
  })
  as.matrix(md5)
})

## Render base column for design
output[["design_base"]] <- renderUI({
  tryCatch({
    if (is.null(input$file_samples)) {
      return(NULL)
    }
    
    selectInput(
      inputId = "design_base",
      label = "Select base column for comparison:",
      choices = colnames(data_samples())
    )
  }, error = function(err) {
    return(NULL)
  })
})

## Render nested design values
output[["design_value"]] <- renderUI({
  tryCatch({
    if (is.null(input$design_base)) {
      return(NULL)
    }
    if (input$design_type == "basic") {
      checkboxGroupInput(
        inputId = "design_value",
        label = "",
        choices = character(0),
        inline = TRUE
      )
    } else {
      showNotification(ui = "WARNING: This type of analysis is 'more advanced'! Make sure you know what you are doing!",
                       duration = 5,
                       type = "warning")
      checkboxGroupInput(
        inputId = "design_value",
        label = "Select nested columns, relative to base column:",
        choices = colnames(data_samples())[!colnames(data_samples()) %in% input$design_base],
        inline = TRUE
      )
    }
  }, error = function(err) {
    return(NULL)
  })
})

## All input values currently active
AllInputs <- reactive({
  x <- reactiveValuesToList(input)
})

## Show matrix selectizes in current mode
output[["matrix"]] <- renderUI({
  fluidRow(column(width = 5,
                  uiOutput("matrix_value1")),
           column(width = 2,
                  h2("VS")),
           column(width = 5,
                  uiOutput("matrix_value2")))
})

## Select items for left matrix
output[["matrix_value1"]] <- renderUI({
  tryCatch({
    columns <- c(input$design_base, input$design_value)
    
    selectInput(
      inputId = "matrix_val1",
      label = "Select values for comparison:",
      multiple = TRUE,
      selected = AllInputs()[["matrix_val1"]],
      choices = c("(Treatment)" = "", unique(data_samples()[columns]))
    )
  }, error = function(err) {
    return(NULL)
  })
})

## Select items for right/main matrix
output[["matrix_value2"]] <- renderUI({
  tryCatch({
    columns <- c(input$design_base, input$design_value)
    
    selectInput(
      inputId = "matrix_val2",
      label = "Select values for comparison:",
      multiple = TRUE,
      selected = AllInputs()[["matrix_val2"]],
      choices = c("(Control)" = "", unique(data_samples()[columns]))
    )
  }, error = function(err) {
    return(NULL)
  })
})

## Show the current design in use
output[["show_design"]] <- renderUI({
  tryCatch({
    design <- createDesign(
      dge = data_samples(),
      dbase = input$design_base,
      dvalue = input$design_value,
      matrix_v1 = input$matrix_val1,
      matrix_v2 = input$matrix_val2
    )
    design <- gsub("\\+", " + ", design)
    if (design == "~" | design == "~0 + ") {
      design <- "No values selected"
    }
    design
  }, error = function(err) {
    return("No values selected")
  })
})

## Show the current matrix in use
output[["show_matrix"]] <- renderUI({
  tryCatch({
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
    
    total_matrix1 <- paste(total_matrix1, collapse = " in ")
    total_matrix2 <- paste(total_matrix2, collapse = " in ")
    
    if (total_matrix1 == "") {
      total_matrix1 <- "NaN"
    }
    if (total_matrix2 == "") {
      total_matrix2 <- "NaN"
    }
    
    total_matrix <- paste("Genes are up/down regulated in", total_matrix1, "compared to", total_matrix2)
  }, error = function(err) {
    return("No values selected")
  })
})

## Exclude samples box
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

## Use genename or geneID
output[["setGeneName"]] <- renderUI({
  tryCatch({
    if (is.null(input$file_annotation)) {
      return(NULL)
    }
    
    radioButtons(
      inputId = "setGeneName",
      label = "Use gene ID or gene symbols:",
      inline = TRUE,
      choices = c("Gene ID" = "id",
                  "Gene Symbol" = "symbol")
    )
  }, error = function(err) {
    return(NULL)
  })
})
