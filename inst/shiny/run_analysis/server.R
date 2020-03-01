
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
      for (x in combn(levels(droplevels(data_samples())[[col]]), 2, simplify = FALSE)) {
        choice <- c(choice, gsub(",", " -", toString(x)))
      }
      for (x in combn(rev(levels(droplevels(data_samples())[[col]])), 2, simplify = FALSE)) {
        choice <- c(choice, gsub(",", " -", toString(x)))
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



