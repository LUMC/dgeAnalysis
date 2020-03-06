
## Start an analysis
observeEvent(input$run_button, {
  if (input$app_mode != "new") {
    showNotification(ui = 'Wrong analysis mode!', duration = 5, type = "error")
    return(NULL)
  }
  
  showModal(modalDialog("Analysis is running...",
                        footer=NULL))
  results <- tryCatch(
    {
      rmarkdown::render(paste("markdown/", input$analysis_method, ".Rmd", sep=''),
                        params = list(data_samples = data_samples(),
                                      data_counts = data_counts(),
                                      data_annotation = data_annotation(),
                                      excluded_samples = input$exclude_samples,
                                      setGeneName = input$setGeneName,
                                      cpm_value = input$cpm_value,
                                      design_value = paste("~0 +", gsub(",", " +", toString(c(input$design_value)))),
                                      matrix_value = if (!is.null(input$design_value)){input$matrix_value},
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

output[["design_value"]] <- renderUI({
  tryCatch({
    if (is.null(input$file_samples$datapath)){return(NULL)}
    checkboxGroupInput("design_value",
                       "Select columns to compare:",
                       choices = colnames(data_samples()),
                       inline = TRUE
    )
  }, error = function(err) {
    return(NULL)
  })
})

output[["matrix_value"]] <- renderUI({
  tryCatch({
    if (is.null(input$design_value)) {
      return(NULL)
    }
    
    choice <- NULL
    for (col in input$design_value) {
      for (x in combn(unique(data_samples()[[col]]), 2, simplify = FALSE)) {
        choice <- c(choice, paste(as.character(x), collapse=" - "))
        choice <- c(choice, paste(as.character(rev(x)), collapse=" - "))
      }
    }
  
    selectInput("matrix_value",
                "Select values to compare:",
                choices = choice
    )
  }, error = function(err) {
    return(NULL)
  })
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
