
output[["enrich_table"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    enrich <- clean_enrich()
    colnames(enrich) <-
      c(
        "source",
        "enrichedTerm",
        "P.Value",
        "termSize",
        "querySize",
        "geneCount",
        "significant",
        "genes"
      )
    DT::datatable(enrich, options = list(pageLength = 50, scrollX = TRUE))
  }, error = function(err) {
    return(DT::datatable(data.frame(c(
      "No data available in table"
    )), rownames = FALSE, colnames = ""))
  })
})

output[["enrich_gprofiler"]] <- renderPlotly({
  tryCatch({
    checkReload()
    gostplot(inUse_enrich, capped = FALSE, interactive = TRUE)
  }, error = function(err) {
    return(NULL)
  })
})

clean_enrich <- reactive({
  tryCatch({
    checkReload()
    enrich_processed <- inUse_enrich$result
    enrich_processed <- enrich_processed[c("source", "term_name", "p_value", "term_size", "query_size", "intersection_size", "significant", "intersection")]
    rownames(enrich_processed) <- inUse_enrich$result$term_id
    enrich_processed <- enrich_processed[order(enrich_processed$p_value),]
    enrich_processed
  }, error = function(err) {
    return(NULL)
  })
})

## create barplot with reactome enrichment terms
output[["enrich_barplot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    enrich <- clean_enrich()
    enrichBarplot(enrich, input$terms_slider)
  }, error = function(err) {
    return(NULL)
  })
})

## barplot slider, set number of values
output[["terms_slider"]] <- renderUI({
  tryCatch({
    checkReload()
    enrich <- clean_enrich()
    sliderInput(
      inputId = "terms_slider",
      label = "Amount of shown pathways:",
      value = nrow(enrich) / 2,
      min = 1,
      max = nrow(enrich),
      step = 1
    )
  }, error = function(err) {
    return(NULL)
  })
})

## create cnet plot with reactome input
output[["cnet_plot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    enrich <- clean_enrich()
    geneSets <- extract_geneSets(enrich,
                                 input$cnet_slider,
                                 input$select_pathway)

    graphData <- cnetPlotly(enrich, geneSets, inUse_deTab)
    plotlyGraph(
      graphData,
      "Gene-Concept Network",
      "Log2FC",
      length(geneSets),
      input$cnet_annoP,
      input$cnet_annoG
    )
  }, error = function(err) {
    return(NULL)
  })
})

## Add specific pathway to cnet plot
output[["cnet_select_pathway"]] <- renderUI({
  tryCatch({
    enrich <- clean_enrich()
    selectInput(
      inputId = "select_pathway",
      label = "Add specific pathway:",
      multiple = TRUE,
      choices = c("Click to add pathway" = "", enrich$term_name)
    )
  }, error = function(err) {
    return(NULL)
  })
})

## get all genes of cnet plot
output[["cnet_table"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    enrich <- clean_enrich()
    geneSets <- extract_geneSets(enrich,
                                 input$cnet_slider,
                                 input$select_pathway)
    
    graphData <- cnetPlotly(enrich, geneSets, inUse_deTab)
    DT::datatable(inUse_deTab[rownames(inUse_deTab) %in% names(V(graphData)),], options = list(pageLength = 15, scrollX = TRUE))
  }, error = function(err) {
    return(DT::datatable(data.frame(c(
      "No data available in table"
    )), rownames = FALSE, colnames = ""))
  })
})

## create heatmap with reactome input
output[["heat_plot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    enrich <- clean_enrich()
    geneSets <- extract_geneSets(enrich,
                                 input$heat_slider,
                                 input$select_heat)
    
    heatplotly(geneSets, inUse_deTab)
  }, error = function(err) {
    return(NULL)
  })
})

output[["heat_select_pathway"]] <- renderUI({
  tryCatch({
    enrich <- clean_enrich()
    selectInput(
      inputId = "select_heat",
      label = "Add specific pathway:",
      multiple = TRUE,
      choices = c("Click to add pathway" = "", enrich$term_name)
    )
  }, error = function(err) {
    return(NULL)
  })
})

output[["heat_table"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    enrich <- clean_enrich()
    geneSets <- extract_geneSets(enrich,
                                 input$heat_slider,
                                 input$select_heat)
    
    graphData <- cnetPlotly(enrich, geneSets, inUse_deTab)
    DT::datatable(inUse_deTab[rownames(inUse_deTab) %in% names(V(graphData)),], options = list(pageLength = 15, scrollX = TRUE))
  }, error = function(err) {
    return(DT::datatable(data.frame(c(
      "No data available in table"
    )), rownames = FALSE, colnames = ""))
  })
})
