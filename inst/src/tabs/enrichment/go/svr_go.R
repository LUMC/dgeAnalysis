
## Biological Process (BP)
## Cellular Component (CC)
## Molecular Function (MF)

# ONLY SUPPORTS HUMAN and MOUSE (depending on provided databases)!!!

## get and create gene ontology enrichment
get_go <- reactive({
  tryCatch({
    checkReload()
    
    organism <- get_organismID(inUse_deTab)
    org <- list(ENS=org.Hs.eg.db,
                ENSMUS=org.Mm.eg.db)
    #ENSRNO=org.Rn.eg.db)
    organism <- org[[organism]]
    
    if (input$choose_go == "enrich") {
      showModal(
        modalDialog(
          h1("Enrichment is running..."),
          h4("GO enrichment based on DE genes (with entrezID)"),
          img(src="loading.gif", width = "50%"),
          footer=NULL
        )
      )
      suppressMessages(enrich <- clusterProfiler::enrichGO(inUse_deTab$entrez[inUse_deTab$DE!=0],  ont = input$selectOntology, organism, pvalueCutoff=0.05))
    } else {
      showModal(
        modalDialog(
          h1("Enrichment is running..."),
          h4("GO enrichment based on all genes (with entrezID) and Log2FC"),
          img(src="loading.gif", width = "50%"),
          footer=NULL
        )
      )
      set.seed(1234)
      geneList <- get_geneList(inUse_deTab)
      suppressMessages(enrich <- clusterProfiler::gseGO(geneList, ont = input$selectOntology, organism, nPerm=10000, pvalueCutoff=0.05, verbose=FALSE, seed=TRUE))
      enrich@result$Count <- lengths(strsplit(enrich$core_enrichment, "/"))
    }
    
    removeModal()
    if (nrow(as.data.frame(enrich)) == 0) {
      showNotification(ui = "GO enrichment has not found any enriched terms!", duration = 5, type = "warning")
    } else {
      showNotification(ui = "GO enrichment has been succesful!", duration = 5, type = "message")
    }
    enrich
  }, error = function(err) {
    removeModal()
    showNotification(ui = "GO enrichment failed with an error!", duration = 5, type = "error")
    showNotification(ui = "GO enrichment supports: ENS, ENSMUS and ENSRNO", duration = 10, type = "error")
    showNotification(ui = as.character(err), duration = 10, type = "error")
    print(err)
    return(NULL)
  })
})

## create gene ontology enrichment table
output[["go_data_table"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    enrich <- as.data.frame(get_go())
    enrich <- enrich[,!(colnames(enrich) %in% c("ID", "leading_edge", "core_enrichment", "geneID"))]
    DT::datatable(enrich, options = list(pageLength = 15, scrollX = TRUE))
  }, error = function(err) {
    return(DT::datatable(data.frame(c("No data available in table")), rownames = FALSE, colnames = ""))
  })
})

## create barplot with gene ontology enrichment terms
output[["go_barplot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    enrich <- as.data.frame(get_go())
    enrichBarplot(enrich, input$bar_go_slider, input$bar_go_value)
  }, error = function(err) {
    return(NULL)
  })
})

## barplot slider, set number of values
output[["bar_go_slider"]] <- renderUI({
  tryCatch({
    checkReload()
    enrich <- as.data.frame(get_go())
    sliderInput("bar_go_slider", "Amount of shown pathways:", nrow(enrich)/2, min = 1, max = nrow(enrich), step=1)
  }, error = function(err) {
    return(NULL)
  })
})

## create cnet plot with gene ontology input
output[["cnet_go_plot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    enrich <- get_go()
    
    geneSets <- extract_geneSets(enrich, input$cnet_go_slider, input$go_select_pathway)
    graphData <- cnetPlotly(enrich, geneSets, inUse_deTab)
    plotlyGraph(graphData, "Gene-Concept Network", "Log2FC", length(geneSets), input$cnet_go_annoP, input$cnet_go_annoG)
  }, error = function(err) {
    return(NULL)
  })
})

## Add specific pathway to cnet plot
output[["cnet_go_select_pathway"]] <- renderUI({
  tryCatch({
    enrich <- as.data.frame(get_go())
    selectInput(inputId = "go_select_pathway",
                label = "Add specific pathway:",
                multiple = TRUE,
                choices = c("Click to add pathway" = "", enrich$Description)
    )
  }, error = function(err) {
    return(NULL)
  })
})

## get all genes of cnet plot
output[["cnet_go_table"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    enrich <- get_go()
    
    geneSets <- extract_geneSets(enrich, input$cnet_go_slider, input$go_select_pathway)
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

## create heatmap with go input
output[["heat_go_plot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    enrich <- get_go()
    
    geneSets <- extract_geneSets(enrich, input$heat_go_slider, input$go_select_heat)
    heatplotly(geneSets, inUse_deTab)
  }, error = function(err) {
    return(NULL)
  })
})

## Add specific pathway to heatmap
output[["heat_go_select_pathway"]] <- renderUI({
  tryCatch({
    enrich <- as.data.frame(get_go())
    selectInput(inputId = "go_select_heat",
                label = "Add specific pathway:",
                multiple = TRUE,
                choices = c("Click to add pathway" = "", enrich$Description)
    )
  }, error = function(err) {
    return(NULL)
  })
})

## get all genes of heatmap
output[["heat_go_table"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    enrich <- get_go()
    
    geneSets <- extract_geneSets(enrich, input$heat_go_slider, input$go_select_heat)
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

## create gene ontology plot with all pathways
output[["gsea_go_plot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    enrich <- get_go()
    graphData <- emap_plotly(enrich)
    plotlyGraph(graphData, input$selectOntology, "P-Value", 0, input$go_network_annoP, FALSE)
  }, error = function(err) {
    return(NULL)
  })
})

## Show dropdown of all found gene ontology pathways
output[["select_go_pathway"]] <- renderUI({
  tryCatch({
    checkReload()
    enrich <- as.data.frame(get_go())
    selectInput("go_select", "Select a pathway:",
                enrich$Description
    )
  }, error = function(err) {
    return(NULL)
  })
})

## show selected pathway from gene ontology
output[["pathway_from_go"]] <- renderUI({
  tryCatch({
    checkReload()
    enrich <- as.data.frame(get_go())
    getpathway <- rownames(enrich)[enrich$Description %in% input$go_select]
    getFromGo <- paste('<img src="https://www.ebi.ac.uk/QuickGO/services/ontology/go/terms/', getpathway, '/chart">', sep="")
    getFromGo <- HTML(paste('<a href="https://www.ebi.ac.uk/QuickGO/GTerm?id=', getpathway, '" target="_blank">\n', getFromGo, '\n</a>', sep=""))
    getFromGo
  }, error = function(err) {
    return(NULL)
  })
})

## INFORMATION BOXES

output[["go_barplot_info"]] <- renderUI({
  infoText <- "The bar plot shows a sorted list of the most enriched terms. The bar plot is sorted based on
  the selected value (p-value, q-value or adjusted p-value). The colors of the bars are also
  generated based on the earlier selected value. On the X-axis, the amount of genes linked to
  the pathway is shown."
  informationBox(infoText)
})

output[["cnet_go_plot_info"]] <- renderUI({
  infoText <- "The concept network can show which genes are involved in the most significant terms. The
  most enriched terms together with all corresponding genes are collected and shown in a
  network plot. Some pathways may contain some matching genes. These genes will also be
  connected. The color given to genes is based on the log fold change determined after the
  expression analysis. In the end, this plot shows the connection of genes between the most
  significant pathways."
  informationBox(infoText)
})

output[["heat_go_plot_info"]] <- renderUI({
  infoText <- "The heatmap visualizes pathways and the corresponding genes. The genes are sorted based on 
        Log2FC. The pathways are sorted on number of gene mathes between other pathways, listing pathways with
        the most gene matches on the left. With this plot genes present in pathways can be compared on sight."
  informationBox(infoText)
})

output[["go_network_info"]] <- renderUI({
  infoText <- "The pathway network shows connections between all found pathways. Every dot that the
  plot shows represents a pathway. The color given to the pathways is based on the adjusted
  p-value."
  informationBox(infoText)
})
