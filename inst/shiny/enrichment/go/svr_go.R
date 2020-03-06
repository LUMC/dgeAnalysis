
## Biological Process (BP)
## Cellular Component (CC)
## Molecular Function (MF)

# ONLY SUPPORTS HUMAN and MOUSE (depending on provided databases)!!!

## get and create gene ontology enrichment
get_go <- reactive({
  tryCatch({
    checkReload()
    organism <- get_organismID(inUse_deTab)
    org <- list(ENS="org.Hs.eg.db",
                ENSMUS="org.Mm.eg.db")
    organism <- org[[organism]]
    if (input$choose_go == "enrich") {
      enrich <- clusterProfiler::enrichGO(inUse_deTab$entrez[inUse_deTab$DE!=0],  ont = input$selectOntology, organism, pvalueCutoff=0.05)
    } else {
      set.seed(1234)
      geneList <- get_geneList(inUse_deTab)
      enrich <- clusterProfiler::gseGO(geneList, ont = input$selectOntology, organism, nPerm=10000, pvalueCutoff=0.05, verbose=FALSE, seed=TRUE)
    }
    enrich
  }, error = function(err) {
    return(NULL)
  })
})

## create gene ontology enrichment table
output[["go_data_table"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    enrich <- as.data.frame(get_go())
    enrich <- enrich[ , -c(1, (ncol(enrich)-1):ncol(enrich))]
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
    
    geneSets <- extract_geneSets(enrich, input$cnet_go_slider)
    graphData <- cnetPlotly(enrich, inUse_deTab, input$cnet_go_slider)
    plotlyGraph(graphData, "Gene-Concept Network", "Log2FC", length(geneSets))
  }, error = function(err) {
    return(NULL)
  })
})

## get all genes of cnet plot
output[["cnet_go_table"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    enrich <- get_go()
    
    geneSets <- extract_geneSets(enrich, input$cnet_go_slider)
    graphData <- cnetPlotly(enrich, inUse_deTab, input$cnet_go_slider)
    DT::datatable(inUse_deTab[inUse_deTab$geneName %in% names(V(graphData)), ], options = list(pageLength = 15, scrollX = TRUE))
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
    plotlyGraph(graphData, input$selectOntology, "P-Value", 0)
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
