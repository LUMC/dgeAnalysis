

## Start an enrichment analysis
observeEvent(input$run_enrichment, {
  showModal(modalDialog(
    h1("Enrichment is running..."),
    img(src = "loading.gif", width = "50%"),
    footer = NULL
  ))
  
  results <- tryCatch({
    rmarkdown::render(
      input = paste0("markdown/enrichmentGProfiler2.Rmd"),
      params = list(
        geneList = input$geneInput,
        organism = input$enrich_organism,
        significant = input$enrich_sign,
        alfa = input$enrich_pvalue,
        database = c(input$source_go, input$source_hp, input$source_bp)
      ),
      output_file = "enrichmentGProfiler2.html"
    )
    load("markdown/enrichment.RData", envir = .GlobalEnv)
    inUse_enrich <<- enrich
    showNotification(ui = "Enrichment has been succesful!",
                     duration = 5,
                     type = "message")
  }, error = function(err) {
    showNotification(ui = "Enrichment failed with an error!",
                     duration = 5,
                     type = "error")
    showNotification(ui = as.character(err),
                     duration = 10,
                     type = "error")
    print(err)
    return(NULL)
  })
  removeModal()
}, ignoreInit = TRUE)

## Create table with the filtered deTab
output[["enrich_detab"]] <- DT::renderDataTable({
  tryCatch({
    checkReload()
    
    DT::datatable(filter_deTab(),
                  options = list(pageLength = 50, scrollX = TRUE))
  }, error = function(err) {
    return(DT::datatable(data.frame(c(
      "No data available in table"
    )), rownames = FALSE, colnames = ""))
  })
})

## Get all filters and values to filter deTab
get_filters <- reactive({
  tryCatch({
    all_filters <- c()
    exp_values <- list(
      all = c(-1, 0, 1),
      deg = c(-1, 1),
      up = 1,
      down = -1
    )
    
    all_filters <- list(
      exp = exp_values[[input$filter_exp]],
      pvalue = input$filter_pvalue,
      fdr = input$filter_fdr,
      fc = input$filter_fc
    )
  }, error = function(err) {
    return(NULL)
  })
})

## Perform filtering of deTab
filter_deTab <- reactive({
  tryCatch({
    checkReload()
    
    filters <- get_filters()
    filtered <- inUse_deTab[inUse_deTab$DE %in% filters$exp &
                              inUse_deTab$P.Value < filters$pvalue &
                              inUse_deTab$FDR < filters$fdr &
                              (inUse_deTab$avgLog2FC < filters$fc[1] |
                                 inUse_deTab$avgLog2FC > filters$fc[2]) ,]
    filtered
  }, error = function(err) {
    return(NULL)
  })
})

## Select input with all available organisms
output[["enrich_organism"]] <- renderUI({
  tryCatch({
    selectInput(
      inputId = "enrich_organism",
      label = "Select organism",
      choices = c(
        "Homo sapiens (Human)" = "hsapiens",
        "Mus musculus (Mouse)" = "mmusculus",
        "Rattus norvegicus (Rat)" = "rnorvegicus",
        "Caenorhabditis elegans" = "celegans",
        "Canis lupus familiaris (Dog)" = "clfamiliaris",
        "Danio rerio (Zebrafish)" = "drerio",
        "Pongo abelii (Orangutan)" = "pabelii"
      ),
      selected = get_organism()
    )
  }, error = function(err) {
    return(NULL)
  })
})

## Find organism from current dataset
get_organism <- reactive({
  tryCatch({
    checkReload()
    
    organism <- get_organismID(inUse_deTab)
    org <- list(
      ENS = "hsapiens",
      ENSMUS = "mmusculus",
      ENSRNO = "rnorvegicus",
      ENSCEL = "celegans",
      ENSCAF = "clfamiliaris",
      ENSDAR = "drerio",
      ENSPPY = "pabelii"
    )
    org[[organism]]
  }, error = function(err) {
    return(NULL)
  })
})

## Show amount of genes left after filtering
output[["enrich_ngenes"]] <- renderUI({
  tryCatch({
    checkReload()
    
    h2("After filtering:", br(), nrow(filter_deTab()), "Genes")
  }, error = function(err) {
    return(NULL)
  })
})

## Text input for genes to analyze
output[["enrich_input"]] <- renderUI({
  tryCatch({
    textAreaInput(
      inputId = "geneInput",
      label = "Genes to analyze",
      value = paste(rownames(filter_deTab()), collapse = "\n"),
      placeholder = "Put your genes of interest here",
      resize = "none",
      height = "600px"
    )
  }, error = function(err) {
    return(NULL)
  })
})
