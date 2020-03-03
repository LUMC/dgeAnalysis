
# ONLY SUPPORTS HUMAN!!!

## get and create disease enrichment
get_do <- reactive({
  tryCatch({
    checkReload()
    if (isTRUE(input$choose_do)) {
      enrich <- DOSE::enrichDO(inUse_deTab$entrez[inUse_deTab$DE!=0], pvalueCutoff=0.05)
    } else {
      set.seed(1234)
      geneList <- get_geneList(inUse_deTab)
      enrich <- DOSE::gseDO(geneList, nPerm=10000, pvalueCutoff=0.05, verbose=FALSE, seed=TRUE)
    }
    enrich
  }, error = function(err) {
    return(NULL)
  })
})

## create disease enrichment table
output[["do_data_table"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    enrich <- as.data.frame(get_do())
    enrich <- enrich[ , -c(1, (ncol(enrich)-1):ncol(enrich))]
    DT::datatable(enrich, options = list(pageLength = 15, scrollX = TRUE))
  }, error = function(err) {
    return(NULL)
  })
})

## create barplot with disease enrichment terms
output[["do_barplot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    enrich <- as.data.frame(get_do())
    enrichBarplot(enrich, input$bar_do_slider, input$bar_do_value)
  }, error = function(err) {
    return(NULL)
  })
})

## create cnet plot with disease input
output[["cnet_do_plot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    enrich <- get_do()
    
    geneSets <- extract_geneSets(enrich, input$cnet_do_slider)
    graphData <- cnetPlotly(enrich, inUse_deTab, input$cnet_do_slider)
    plotlyGraph(graphData, "Gene-Concept Network", "Log2FC", length(geneSets))
  }, error = function(err) {
    return(NULL)
  })
})

## get all genes of cnet plot
output[["cnet_do_table"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    enrich <- get_do()
    
    geneSets <- extract_geneSets(enrich, input$cnet_do_slider)
    graphData <- cnetPlotly(enrich, inUse_deTab, input$cnet_do_slider)
    DT::datatable(inUse_deTab[inUse_deTab$geneName %in% names(V(graphData)), ], options = list(pageLength = 15, scrollX = TRUE))
  }, error = function(err) {
    return(NULL)
  })
})

## create disease plot with all pathways
output[["gsea_do_plot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    enrich <- get_do()
    graphData <- emap_plotly(enrich)
    plotlyGraph(graphData, "Disease ontology", "P-Value", 0)
  }, error = function(err) {
    return(NULL)
  })
})

## Show dropdown of all found gene ontology pathways
output[["select_do_pathway"]] <- renderUI({
  tryCatch({
    checkReload()
    enrich <- as.data.frame(get_do())
    selectInput("do_select", "Select a pathway:",
                enrich$Description
    )
  }, error = function(err) {
    return(NULL)
  })
})

## show selected pathway from gene ontology
output[["pathway_from_do"]] <- renderUI({
  tryCatch({
    checkReload()
    enrich <- as.data.frame(get_do())
    getpathway <- rownames(enrich)[enrich$Description %in% input$do_select]
    
    getFromDo <- includeHTML(paste("https://rgd.mcw.edu/rgdweb/ontology/view.html?acc_id=", getpathway, sep=""))
    getFromDo <- gsub('.*<div id="browser_graph" style="width:800px;overflow:auto">\\s*|\\s*</div>.*', "", getFromDo)
    if (grepl('<img id="termHierarchy"', getFromDo)) {
      getFromDo <- gsub('src="', 'src="https://rgd.mcw.edu', getFromDo)
      getFromDo <- gsub('href="', 'href="https://rgd.mcw.edu/rgdweb/ontology/', getFromDo)
    } else {
      getFromDo <- NULL
    }
    getFromDo
  }, error = function(err) {
    return(NULL)
  })
})
