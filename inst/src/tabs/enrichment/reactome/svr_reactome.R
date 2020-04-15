
## get and create reactome enrichment
get_reactome <- reactive({
  tryCatch({
    checkReload()
    
    organism <- get_organismID(inUse_deTab)
    org <- list(ENSCEL="celegans",
                ENS="human",
                ENSMUS="mouse",
                ENSRNO="rat")
    organism <- org[[organism]]
    
    if (input$choose_reactome == "enrich") {
      showModal(
        modalDialog(
          h1("Enrichment is running..."),
          h4("Reactome enrichment based on DE genes (with entrezID)"),
          img(src="loading.gif", width = "50%"),
          footer=NULL
        )
      )
      suppressMessages(enrich <- ReactomePA::enrichPathway(inUse_deTab$entrez[inUse_deTab$DE!=0], organism=organism, pvalueCutoff=0.05))
    } else {
      showModal(
        modalDialog(
          h1("Enrichment is running..."),
          h4("Reactome enrichment based on all genes (with entrezID) and Log2FC"),
          img(src="loading.gif", width = "50%"),
          footer=NULL
        )
      )
      set.seed(1234)
      geneList <- get_geneList(inUse_deTab)
      suppressMessages(enrich <- ReactomePA::gsePathway(geneList, organism=organism, nPerm=10000, pvalueCutoff=0.05, verbose=FALSE, seed=TRUE))
    }
    
    removeModal()
    if (nrow(as.data.frame(enrich)) == 0) {
      showNotification(ui = "Reactome enrichment has not found any enriched terms!", duration = 5, type = "warning")
    } else {
      showNotification(ui = "Reactome enrichment has been succesful!", duration = 5, type = "message")
    }
    enrich
  }, error = function(err) {
    removeModal()
    showNotification(ui = "Reactome enrichment failed with an error!", duration = 5, type = "error")
    showNotification(ui = "Reactome enrichment supports: ENSCEL, ENS, ENSMUS and ENSRNO", duration = 10, type = "error")
    showNotification(ui = as.character(err), duration = 10, type = "error")
    print(err)
    return(NULL)
  })
})

## create reactome enrichment table
output[["reactome_data_table"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    enrich <- as.data.frame(get_reactome())
    enrich <- enrich[ , -c(1, (ncol(enrich)-1):ncol(enrich))]
    DT::datatable(enrich, options = list(pageLength = 15, scrollX = TRUE))
  }, error = function(err) {
    return(DT::datatable(data.frame(c("No data available in table")), rownames = FALSE, colnames = ""))
  })
})

## create barplot with reactome enrichment terms
output[["reactome_barplot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    enrich <- as.data.frame(get_reactome())
    enrichBarplot(enrich, input$bar_reactome_slider, input$bar_reactome_value)
  }, error = function(err) {
    return(NULL)
  })
})

## barplot slider, set number of values
output[["bar_reactome_slider"]] <- renderUI({
  tryCatch({
    checkReload()
    enrich <- as.data.frame(get_reactome())
    sliderInput("bar_reactome_slider", "Amount of shown pathways:", nrow(enrich)/2, min = 1, max = nrow(enrich), step=1)
  }, error = function(err) {
    return(NULL)
  })
})

## create cnet plot with reactome input
output[["cnet_reactome_plot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    enrich <- get_reactome()
    
    geneSets <- extract_geneSets(enrich, input$cnet_reactome_slider, input$reactome_select_pathway)
    graphData <- cnetPlotly(enrich, geneSets, inUse_deTab)
    plotlyGraph(graphData, "Gene-Concept Network", "Log2FC", length(geneSets), input$cnet_reactome_annoP, input$cnet_reactome_annoG)
  }, error = function(err) {
    return(NULL)
  })
})

## Add specific pathway to cnet plot
output[["cnet_reactome_select_pathway"]] <- renderUI({
  tryCatch({
    enrich <- as.data.frame(get_reactome())
    selectInput(inputId = "reactome_select_pathway",
                label = "Add specific pathway:",
                multiple = TRUE,
                choices = c("Click to add pathway" = "", enrich$Description)
    )
  }, error = function(err) {
    return(NULL)
  })
})

## get all genes of cnet plot
output[["cnet_reactome_table"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    enrich <- get_reactome()
    
    geneSets <- extract_geneSets(enrich, input$cnet_reactome_slider, input$reactome_select_pathway)
    graphData <- cnetPlotly(enrich, geneSets, inUse_deTab)
    if ("geneName" %in% colnames(inUse_deTab)) {
      DT::datatable(inUse_deTab[inUse_deTab$geneName %in% names(V(graphData)), ], options = list(pageLength = 15, scrollX = TRUE))
    } else {
      DT::datatable(inUse_deTab[rownames(inUse_deTab) %in% names(V(graphData)), ], options = list(pageLength = 15, scrollX = TRUE))
    }
  }, error = function(err) {
    return(DT::datatable(data.frame(c("No data available in table")), rownames = FALSE, colnames = ""))
  })
})

## create heatmap with reactome input
output[["heat_reactome_plot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    enrich <- get_reactome()
    
    geneSets <- extract_geneSets(enrich, input$heat_reactome_slider, input$reactome_select_heat)
    heatplotly(geneSets, inUse_deTab)
  }, error = function(err) {
    return(NULL)
  })
})

## Add specific pathway to heatmap
output[["heat_reactome_select_pathway"]] <- renderUI({
  tryCatch({
    enrich <- as.data.frame(get_reactome())
    selectInput(inputId = "reactome_select_heat",
                label = "Add specific pathway:",
                multiple = TRUE,
                choices = c("Click to add pathway" = "", enrich$Description)
    )
  }, error = function(err) {
    return(NULL)
  })
})

## get all genes of heatmap
output[["heat_reactome_table"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    enrich <- get_reactome()
    
    geneSets <- extract_geneSets(enrich, input$heat_reactome_slider, input$reactome_select_heat)
    graphData <- cnetPlotly(enrich, geneSets, inUse_deTab)
    if ("geneName" %in% colnames(inUse_deTab)) {
      DT::datatable(inUse_deTab[inUse_deTab$geneName %in% names(V(graphData)), ], options = list(pageLength = 15, scrollX = TRUE))
    } else {
      DT::datatable(inUse_deTab[rownames(inUse_deTab) %in% names(V(graphData)), ], options = list(pageLength = 15, scrollX = TRUE))
    }
  }, error = function(err) {
    return(DT::datatable(data.frame(c("No data available in table")), rownames = FALSE, colnames = ""))
  })
})

## create reactome plot with all pathways
output[["gsea_reactome_plot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    enrich <- get_reactome()
    graphData <- emap_plotly(enrich)
    plotlyGraph(graphData, "Reactome", "P-Value", 0, input$reactome_network_annoP, FALSE)
  }, error = function(err) {
    return(NULL)
  })
})

## Show dropdown of all found reactome pathways
output[["select_reactome_pathway"]] <- renderUI({
  tryCatch({
    checkReload()
    enrich <- as.data.frame(get_reactome())
    selectInput("reactome_select", "Select a pathway:",
                enrich$Description
    )
  }, error = function(err) {
    return(NULL)
  })
})

## show selected pathway from reactome
output[["pathway_from_reactome"]] <- renderUI({
  tryCatch({
    checkReload()
    enrich <- as.data.frame(get_reactome())
    getpathway <- rownames(enrich)[enrich$Description %in% input$reactome_select]
    getFromReactome <- includeHTML(paste("https://reactome.org/ContentService/exporter/diagram/", getpathway, ".svg?title=false", sep=""))
    getFromReactome <- HTML(paste('<a href="https://reactome.org/PathwayBrowser/#/', getpathway, '" target="_blank">\n', getFromReactome, '\n</a>', sep=""))
    getFromReactome
  }, error = function(err) {
    return(NULL)
  })
})

output[["reactome_barplot_info"]] <- renderUI({
  infoText <- "The bar plot shows a sorted list of the most enriched terms. The bar plot is sorted based on
        the selected value (p-value, q-value or adjusted p-value). The colors of the bars are also
        generated based on the earlier selected value. On the X-axis, the amount of genes linked to
        the pathway is shown."
  informationBox(infoText)
})

output[["cnet_reactome_plot_info"]] <- renderUI({
  infoText <- "The concept network can show which genes are involved in the most significant terms. The
        most enriched terms together with all corresponding genes are collected and shown in a
        network plot. Some pathways may contain some matching genes. These genes will also be
        connected. The color given to genes is based on the log fold change determined after the
        expression analysis. In the end, this plot shows the connection of genes between the most
        significant pathways."
  informationBox(infoText)
})

output[["heat_reactome_plot_info"]] <- renderUI({
  infoText <- "The heatmap visualizes pathways and the corresponding genes. The genes are sorted based on 
        frequeny. The more a genes is present in a pathway the lower it's listed. The pathways are
        sorted on number of genes, listing pathway with the highest number of genes on the left. The 
        color is given based on the Log2FC value of a gene. With this plot genes can be compared on sight
        between pathways."
  informationBox(infoText)
})

output[["reactome_network_info"]] <- renderUI({
  infoText <- "The pathway network shows connections between all found pathways. Every dot that the
        plot shows represents a pathway. The color given to the pathways is based on the adjusted
        p-value."
  informationBox(infoText)
})
