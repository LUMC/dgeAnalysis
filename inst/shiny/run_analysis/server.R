
## Start an analysis
observeEvent(input$run_button, {
  tryCatch({
    if (isTRUE(bookmark_loaded)) {
      bookmark_loaded <<- FALSE
      return(NULL)
    }
  }, error = function(err) {
  })
  
  showModal(modalDialog("Analysis is running...",
                        footer=NULL))
  results <- tryCatch(
    {
      rmarkdown::render(paste("markdown/", input$analysis_method, ".Rmd", sep=''),
                        params = list(data_samples = data_samples(),
                                      data_counts = data_counts(),
                                      data_annotation = data_annotation(),
                                      #excluded_samples = rownames(excluded_selected()),
                                      setGeneName = input$setGeneName,
                                      cpm_value = input$cpm_value,
                                      design_value = paste("~0 +", gsub(",", " +", toString(c(input$design_value)))),
                                      matrix_value = if (!is.null(input$design_value)){input$matrix_value},
                                      alpha = input$alpha_value),
                        output_file = paste(input$analysis_method, '.html', sep=''))
      load("markdown/analysis.RData", envir=.GlobalEnv)
      inUse_deTab <<- deTab
      inUse_normDge <<- normDge
      f <- "Analysis succesfull"
    }, error = function(err) {
      f <- "Analysis failed with an error"
      print(err)
      return(f)
    }
  )
  output$analysis_state <- renderText({
    results
  })
  removeModal()
})

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
    print(err)
    return(NULL)
  })
})



