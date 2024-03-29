
## Create se from samples and counts
get_se <- reactive({
  se <- readCountsFromTable(data_counts(), data_samples())
  se <- addSamplesFromTableToSE(se, data_samples())
})

## Select a group to sort summary plot
output[["group_sum"]] <- renderUI({
  tryCatch({
    checkReload()
    selectInput(
      inputId = "group_sum",
      label = "Group by:",
      choices = c("Samples" = "none", colnames(data_samples()))
    )
  }, error = function(err) {
    return(NULL)
  })
})

## Create alignment summary plot
output[["align_sum"]] <- renderPlotly({
  tryCatch({
    checkReload()
    
    ## Only plot if UI is loaded
    if(is.null(input$group_sum)) {
      break
    }
    
    ## Get input data
    se <- get_se()
    plot_data <- alignment_summary(se, input$setSummary)
    text <- 'paste("Sample:", sample,
                  "\nClass:", feature,
                  "\nSize:", round(count, 2))'

    ## Create plot
    ggplotly(
      bar_plot(
        df = plot_data,
        x = "count",
        y = "sample",
        text = text,
        group = input$group_sum,
        fill = "feature",
        rev = TRUE,
        facet = input$group_sum,
        title = "Count assignments",
        xlab = "Counts",
        ylab = ""
      ),
      tooltip = "text"
    )
  }, error = function(err) {
    return(NULL)
  })
})

## Create complexity plot
output[["complex"]] <- renderPlotly({
  tryCatch({
    checkReload()
    
    ## Only plot if UI is loaded
    if(is.null(input$group_color)) {
      break
    }
    
    ## Get input data
    se <- get_se()
    plot_data <- complexity(se, rank = input$comp_rank)
    text <- 'paste("Sample:", sample,
                  "\nGenes:", rank,
                  "\nSize:", round(get(y), 2))'
    
    ## Create plot
    ggplotly(
      line_plot(
        df = plot_data,
        x = "rank",
        y = input$setComplexity,
        text = text,
        group = input$group_color,
        plot = "complexity",
        title = "Gene complexity",
        xlab = "Number of genes",
        ylab = "Cumulative reads per number of genes"
      ),
      tooltip = "text"
    )
  }, error = function(err) {
    return(NULL)
  })
})

## Select a group to color complexity plot
output[["group_color"]] <- renderUI({
  tryCatch({
    checkReload()
    selectInput(
      inputId = "group_color",
      label = "Group by:",
      choices = c("Samples" = "sample", colnames(data_samples()))
    )
  }, error = function(err) {
    return(NULL)
  })
})

## INFORMATION BOXES

output[["align_sum_info"]] <- renderUI({
  infoText <-
    "The summary plots provide a general overview of the counting data. These plots show a
        bar chart showing the alignment results and a complexity plot. The alignment plot is a stacked bar
        plot, where each 'row' represents a sample and the stacked bars per row are different
        tags. These tags indicate the distribution of read counts given by a read counter (eg.
        HTSeq)."
  informationBox(infoText)
})

output[["complex_info"]] <- renderUI({
  infoText <-
    "The complexity graphs show how many reads cover a given number of genes. On the X-axis,
        there is a ranking from zero to a thousand. This arrangement represents the number of genes.
        The Y-axis shows the proportion of reads in relation to the total number of gene counts.
        Large variances between samples of the number or reads for a gene, will be visible."
  informationBox(infoText)
})
