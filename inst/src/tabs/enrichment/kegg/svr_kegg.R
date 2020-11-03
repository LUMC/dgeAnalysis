
## get and create kegg enrichment
get_kegg <- reactive({
  tryCatch({
    checkReload()
    
    organism <- get_organismID(inUse_deTab)
    org <- list(
      ENSCEL = "cel",
      ENSCAF = "cfa",
      ENSDAR = "dre",
      ENS = "hsa",
      ENSMUS = "mmu",
      ENSRNO = "rno"
    )
    organism <- org[[organism]]
    
    if (input$choose_kegg == "enrich") {
      showModal(modalDialog(
        h1("Enrichment is running..."),
        h4("KEGG enrichment based on DE genes (with entrezID)"),
        img(src = "loading.gif", width = "50%"),
        footer = NULL
      ))
      suppressMessages(
        enrich <- clusterProfiler::enrichKEGG(
          inUse_deTab$entrez[inUse_deTab$DE != 0],
          organism = organism,
          pvalueCutoff = 0.05
        )
      )
    } else {
      showModal(modalDialog(
        h1("Enrichment is running..."),
        h4("KEGG enrichment based on all genes (with entrezID) and Log2FC"),
        img(src = "loading.gif", width = "50%"),
        footer = NULL
      ))
      set.seed(1234)
      geneList <- get_geneList(inUse_deTab)
      suppressMessages(
        enrich <- clusterProfiler::gseKEGG(
          geneList,
          organism = organism,
          nPerm = 10000,
          pvalueCutoff = 0.05,
          verbose = FALSE,
          seed = TRUE
        )
      )
      enrich@result$Count <- lengths(strsplit(enrich$core_enrichment, "/"))
    }
    
    removeModal()
    if (nrow(as.data.frame(enrich)) == 0) {
      showNotification(ui = "KEGG enrichment has not found any enriched terms!",
                       duration = 5,
                       type = "warning")
    } else {
      showNotification(ui = "KEGG enrichment has been succesful!",
                       duration = 5,
                       type = "message")
    }
    enrich
  }, error = function(err) {
    removeModal()
    showNotification(ui = "KEGG enrichment failed with an error!",
                     duration = 5,
                     type = "error")
    showNotification(ui = "KEGG enrichment supports: ENSCEL, ENSCAF, ENSDAR, ENS, ENSMUS and ENSRNO",
                     duration = 10,
                     type = "error")
    showNotification(ui = as.character(err),
                     duration = 10,
                     type = "error")
    print(err)
    return(NULL)
  })
})

## create kegg enrichment table
output[["kegg_data_table"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    enrich <- as.data.frame(get_kegg())
    enrich <- enrich[, !(colnames(enrich) %in% c("ID", "leading_edge", "core_enrichment", "geneID"))]
    DT::datatable(enrich, options = list(pageLength = 15, scrollX = TRUE))
  }, error = function(err) {
    return(DT::datatable(data.frame(c(
      "No data available in table"
    )), rownames = FALSE, colnames = ""))
  })
})

## create barplot with kegg enrichment terms
output[["kegg_barplot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    enrich <- as.data.frame(get_kegg())
    enrichBarplot(enrich, input$bar_kegg_slider, input$bar_kegg_value)
  }, error = function(err) {
    return(NULL)
  })
})

## barplot slider, set number of values
output[["bar_kegg_slider"]] <- renderUI({
  tryCatch({
    checkReload()
    enrich <- as.data.frame(get_kegg())
    sliderInput(
      inputId = "bar_kegg_slider",
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

## create cnet plot with kegg input
output[["cnet_kegg_plot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    enrich <- get_kegg()
    
    geneSets <- extract_geneSets(enrich,
                                 input$cnet_kegg_slider,
                                 input$kegg_select_pathway)
    graphData <- cnetPlotly(enrich, geneSets, inUse_deTab)
    plotlyGraph(
      graphData,
      "Gene-Concept Network",
      "Log2FC",
      length(geneSets),
      input$cnet_kegg_annoP,
      input$cnet_kegg_annoG
    )
  }, error = function(err) {
    return(NULL)
  })
})

## Add specific pathway to cnet plot
output[["cnet_kegg_select_pathway"]] <- renderUI({
  tryCatch({
    enrich <- as.data.frame(get_kegg())
    selectInput(
      inputId = "kegg_select_pathway",
      label = "Add specific pathway:",
      multiple = TRUE,
      choices = c("Click to add pathway" = "", enrich$Description)
    )
  }, error = function(err) {
    return(NULL)
  })
})

## get all genes of cnet plot
output[["cnet_kegg_table"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    enrich <- get_kegg()
    
    geneSets <- extract_geneSets(enrich,
                                 input$cnet_kegg_slider,
                                 input$kegg_select_pathway)
    graphData <- cnetPlotly(enrich, geneSets, inUse_deTab)
    if ("geneName" %in% colnames(inUse_deTab)) {
      DT::datatable(inUse_deTab[inUse_deTab$geneName %in% names(V(graphData)),], options = list(pageLength = 15, scrollX = TRUE))
    } else {
      DT::datatable(inUse_deTab[rownames(inUse_deTab) %in% names(V(graphData)),], options = list(pageLength = 15, scrollX = TRUE))
    }
  }, error = function(err) {
    return(DT::datatable(data.frame(c(
      "No data available in table"
    )), rownames = FALSE, colnames = ""))
  })
})

## create heatmap with kegg input
output[["heat_kegg_plot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    enrich <- get_kegg()
    
    geneSets <- extract_geneSets(enrich, input$heat_kegg_slider, input$kegg_select_heat)
    heatplotly(geneSets, inUse_deTab)
  }, error = function(err) {
    return(NULL)
  })
})

## Add specific pathway to heatmap
output[["heat_kegg_select_pathway"]] <- renderUI({
  tryCatch({
    enrich <- as.data.frame(get_kegg())
    selectInput(
      inputId = "kegg_select_heat",
      label = "Add specific pathway:",
      multiple = TRUE,
      choices = c("Click to add pathway" = "", enrich$Description)
    )
  }, error = function(err) {
    return(NULL)
  })
})

## get all genes of heatmap
output[["heat_kegg_table"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    enrich <- get_kegg()
    
    geneSets <- extract_geneSets(enrich, input$heat_kegg_slider, input$kegg_select_heat)
    graphData <- cnetPlotly(enrich, geneSets, inUse_deTab)
    if ("geneName" %in% colnames(inUse_deTab)) {
      DT::datatable(inUse_deTab[inUse_deTab$geneName %in% names(V(graphData)),], options = list(pageLength = 15, scrollX = TRUE))
    } else {
      DT::datatable(inUse_deTab[rownames(inUse_deTab) %in% names(V(graphData)),], options = list(pageLength = 15, scrollX = TRUE))
    }
  }, error = function(err) {
    return(DT::datatable(data.frame(c(
      "No data available in table"
    )), rownames = FALSE, colnames = ""))
  })
})

## create kegg plot with all pathways
output[["gsea_kegg_plot"]] <- renderPlotly({
  tryCatch({
    checkReload()
    enrich <- get_kegg()
    graphData <- emap_plotly(enrich)
    plotlyGraph(graphData,
                "KEGG",
                "P-Value",
                0,
                input$kegg_network_annoP,
                FALSE)
  }, error = function(err) {
    return(NULL)
  })
})

## Show dropdown of all found kegg pathways
output[["select_kegg_pathway"]] <- renderUI({
  tryCatch({
    checkReload()
    enrich <- as.data.frame(get_kegg())
    selectInput(inputId = "kegg_select",
                label = "Select a pathway:",
                choices = enrich$Description)
  }, error = function(err) {
    return(NULL)
  })
})

## show selected pathway from kegg
output[["pathway_from_kegg"]] <- renderUI({
  tryCatch({
    checkReload()
    enrich <- as.data.frame(get_kegg())
    getpathway <- rownames(enrich)[enrich$Description %in% input$kegg_select]
    getpathway <- "hsa05152"
    getFromKegg <- a(
      href = paste0("https://www.genome.jp/kegg-bin/show_pathway?", getpathway),
      target = "_blank",
      img(
        src = paste0(
          "https://www.genome.jp/kegg/pathway/",
          substr(getpathway, 1, 3),
          "/",
          getpathway,
          ".png"
        )
      )
    )
    getFromKegg
  }, error = function(err) {
    return(NULL)
  })
})

## INFORMATION BOXES

output[["kegg_barplot_info"]] <- renderUI({
  infoText <-
    "The bar plot shows a sorted list of the most enriched terms. The bar plot is sorted based on
        the selected value (p-value, q-value or adjusted p-value). The colors of the bars are also
        generated based on the earlier selected value. On the X-axis, the amount of genes linked to
        the pathway is shown."
  informationBox(infoText)
})

output[["cnet_kegg_plot_info"]] <- renderUI({
  infoText <-
    "The concept network can show which genes are involved in the most significant terms. The
        most enriched terms together with all corresponding genes are collected and shown in a
        network plot. Some pathways may contain some matching genes. These genes will also be
        connected. The color given to genes is based on the log fold change determined after the
        expression analysis. In the end, this plot shows the connection of genes between the most
        significant pathways."
  informationBox(infoText)
})

output[["heat_kegg_plot_info"]] <- renderUI({
  infoText <-
    "The heatmap visualizes pathways and the corresponding genes. The genes are sorted based on
        Log2FC. The pathways are sorted on number of gene mathes between other pathways, listing pathways with
        the most gene matches on the left. With this plot genes present in pathways can be compared on sight."
  informationBox(infoText)
})

output[["kegg_network_info"]] <- renderUI({
  infoText <-
    "The pathway network shows connections between all found pathways. Every dot that the
        plot shows represents a pathway. The color given to the pathways is based on the adjusted
        p-value."
  informationBox(infoText)
})
