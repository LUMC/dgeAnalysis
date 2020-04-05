
# ONLY SUPPORTS HUMAN!!!

## get and create disease enrichment
get_do <- reactive({
  tryCatch({
    checkReload()
    if (input$choose_do == "enrich") {
      suppressMessages(enrich <- DOSE::enrichDO(inUse_deTab$entrez[inUse_deTab$DE!=0], pvalueCutoff=0.05))
    } else {
      set.seed(1234)
      geneList <- get_geneList(inUse_deTab)
      suppressMessages(enrich <- DOSE::gseDO(geneList, nPerm=10000, pvalueCutoff=0.05, verbose=FALSE, seed=TRUE))
    }
    if (nrow(as.data.frame(enrich)) == 0) {
      showNotification(ui = "DO enrichment has not found any enriched terms!", duration = 5, type = "warning")
    } else {
      showNotification(ui = "DO enrichment has been succesful!", duration = 5, type = "message")
    }
    enrich
  }, error = function(err) {
    showNotification(ui = "DO enrichment failed with an error!", duration = 5, type = "error")
    showNotification(ui = as.character(err), duration = 10, type = "error")
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
    return(DT::datatable(data.frame(c("No data available in table")), rownames = FALSE, colnames = ""))
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

## barplot slider, set number of values
output[["bar_do_slider"]] <- renderUI({
  tryCatch({
    checkReload()
    enrich <- as.data.frame(get_do())
    sliderInput("bar_do_slider", "Amount of shown pathways:", nrow(enrich)/2, min = 1, max = nrow(enrich), step=1)
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
    return(DT::datatable(data.frame(c("No data available in table")), rownames = FALSE, colnames = ""))
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

## INFORMATION BOXES

output[["do_barplot_info"]] <- renderUI({
  infoText <- "The bar plot shows a sorted list of the most enriched terms. The bar plot is sorted based on
  the selected value (p-value, q-value or adjusted p-value). The colors of the bars are also
  generated based on the earlier selected value. On the X-axis, the amount of genes linked to
  the pathway is shown."
  informationBox(infoText)
})

output[["cnet_do_plot_info"]] <- renderUI({
  infoText <- "The concept network can show which genes are involved in the most significant terms. The
  most enriched terms together with all corresponding genes are collected and shown in a
  network plot. Some pathways may contain some matching genes. These genes will also be
  connected. The color given to genes is based on the log fold change determined after the
  expression analysis. In the end, this plot shows the connection of genes between the most
  significant pathways."
  informationBox(infoText)
})

output[["do_network_info"]] <- renderUI({
  infoText <- "The pathway network shows connections between all found pathways. Every dot that the
  plot shows represents a pathway. The color given to the pathways is based on the adjusted
  p-value."
  informationBox(infoText)
})
