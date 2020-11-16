
## ----- UTIL FUNCTIONS GENE SET ENRICHMENT -----


#' Collect genes within a pathway.
#' Gets genes from enrichment result.
#' Gene names are retrieved from terms
#' duplicated pathways are removed from the set.
#'
#' @param enrich Enrichment object, containing enrichment results
#' @param slider Integer, Updated number of pathways to show
#' @param selected Vector, Contains manually selected pathways
#'
#' @return geneSets, (vector) Genes found in corresponding pathways
#'
#' @export

extract_geneSets <- function(enrich, slider, selected) {
  splitTerms <- split(x = enrich$intersection, f = enrich$term_name)
  geneSets <- splitTerms[0:slider]
  geneSets <- append(geneSets, splitTerms[names(splitTerms) %in% selected])
  geneSets <- lapply(geneSets, function(geneSets) unique(unlist(strsplit(geneSets, ",", perl = T))))
  geneSets <- geneSets[unique(names(geneSets))]
  geneSets
}


#' The inputList is converted to a dataframe.
#' The dataframe is converted to a graph.
#' Igraph object is created based on a vector/list.
#'
#' @param inputList Vector, containing Gene names and LogFC
#'
#' @return g, (Igraph object) Graph with links between genes
#'
#' @export

list2graph <- function(inputList) {
  ldf <- list2df(inputList)
  g <- igraph::graph.data.frame(ldf, directed = FALSE)
  return(g)
}


#' The inputList is converted to a dataframe format.
#' Dataframe object is created based on a vector/list.
#' Igraph object is created based on a vector/list.
#'
#' @param inputList Vector, containing Gene names and LogFC
#'
#' @return ldf, (Dataframe) With all in use genes
#'
#' @export

list2df <- function(inputList) {
  ldf <- lapply(1:length(inputList), function(i) {
    data.frame(categoryID = rep(names(inputList[i]),
                                length(inputList[[i]])),
               Gene = inputList[[i]])
  })
  
  do.call('rbind', ldf)
}


#' The gene IDs are gathered from DE table.
#' The first part of the ID name is kept.
#'
#' @param deTab Dataframe, with all analysis results
#'
#' @return id, (String) Organism ID value
#'
#' @export

get_organismID <- function(deTab) {
  tryCatch({
    if ("geneName" %in% colnames(inUse_deTab)) {
      id <- rownames(deTab)[nrow(deTab)]
    } else {
      id <- deTab$geneId[nrow(deTab)]
    }
    id <- gsub("[^A-Za-z]", "", id)
    id <- sub(".{1}$", "", id)
    id
  }, error = function(err) {
    return(NULL)
  })
}

## --------------------------------------------------------------------------

## ----- PLOT FUNCTIONS GENE SET ENRICHMENT -----


#' The enrichment results is filtered based on the number shown of pathways.
#' Descriptions of the pathways are added and the color scale is calculated.
#' A Barplot is created with a bar per pathway.
#'
#' @param enrich Enrich result, A enrichment results
#' @param amount Integer, Value with the number of pathways to show
#'
#' @return p, (Plotly object) plot
#'
#' @export

enrichBarplot <- function(enrich, amount) {
  enrich <- na.omit(enrich[0:amount, ])
  enrich$term_name <- factor(enrich$term_name,
                             levels = unique(enrich$term_name)[order(enrich$p_value,
                                                                     enrich$term_name,
                                                                     decreasing = TRUE)])
  color <- seq(
    from = min(enrich$p_value),
    to = max(enrich$p_value),
    length.out = 10
  )[2:9]
  
  p <- plot_ly(
    data = enrich,
    x = ~ intersection_size,
    y = ~ term_name,
    orientation = 'h',
    type = "bar",
    hoverinfo = 'text',
    text = paste("Source:", enrich$source, "\n# Genes:", enrich$intersection_size),
    marker = list(
      color = -enrich$p_value,
      colorscale = 'Viridis',
      colorbar = list(
        title = "P-value",
        tickmode = "array",
        tickvals = -color,
        ticktext = floor(color) + signif(color %% 1, 4)
      ),
      reversescale = FALSE
    )
  ) %>%
    plotly::layout(
      xaxis = list(title = 'Gene counts'),
      title = "Enrichment barplot",
      yaxis = list(title = '')
    ) %>%
    config(
      toImageButtonOptions = list(
        format = "png",
        filename = "enrichbar",
        width = 1500,
        height = 1000
      )
    )
  p
}


#' The enrichment results is filtered based on the number shown of pathways.
#' Number of up/down regulated genes is retrieved and added per term.
#' A Barplot is created with a bar per pathway.
#' For each pathway there is bar with up/down regulated genes.
#'
#' @param enrich Enrich result, A enrichment results
#' @param deTab Dataframe, with all analysis results
#' @param amount Integer, Value with the number of pathways to show
#'
#' @return p, (Plotly object) plot
#'
#' @export

enrichDE <- function(enrich, deTab, amount) {
  enrich <- na.omit(enrich[0:amount,])
  enrich$term_name <- factor(enrich$term_name,
                             levels = unique(enrich$term_name)[order(enrich$p_value,
                                                                     enrich$term_name,
                                                                     decreasing = TRUE)])
  enrich[c("down", "not_sign", "up")] <- NA
  for (row in 1:nrow(enrich)) {
    genes <- unlist(strsplit(enrich[row, ]$intersection, split = ","))
    de <- table(deTab$DE[rownames(deTab) %in% genes])
    names(de) <- c("down", "not_sign", "up")[match(names(de), c(-1, 0, 1))]
    enrich[row, ][names(de)] <- de
  }
  
  p <- plot_ly(
    data = enrich,
    x = ~ up,
    y = ~ term_name,
    orientation = 'h',
    base = 0,
    type = "bar",
    name = "Up regulated",
    hoverinfo = 'text',
    text = paste("Source:", enrich$source, "\n# Genes up:", enrich$up)
  ) %>%
    plotly::layout(
      barmode = 'stack',
      xaxis = list(title = 'Gene counts'),
      title = "DE genes in terms",
      yaxis = list(title = '')
    ) %>%
    config(
      toImageButtonOptions = list(
        format = "png",
        filename = "enrichbar",
        width = 1500,
        height = 1000
      )
    )
  p <- add_trace(
    p,
    x = if (all(is.na(enrich$up))) {
      ~ down
    } else {
      ~ down * -1
    }, 
    y = ~ term_name,
    name = "Down regulated",
    orientation = 'h',
    hoverinfo = 'text',
    text = paste("Source:", enrich$source, "\n# Genes down:", enrich$down)
  )
  p
}


#' Genes are gathered from enrichment result from specific pathway.
#' The geneList is prepared with the right LogFC values and links of genes between multiple pathways.
#' The number of pathways that are shown is defined with cnet_slider.
#' Igraph object is made to create links of genes between multiple pathways.
#' Colors of dots are based on LogFC.
#'
#' @param enrich Enrich result, A enrichment results
#' @param deTab Dataframe, with all analysis results
#' @param geneSets Martrix object, containing all pathways with corresponding genes
#'
#' @return g, (Igraph object) containing links of genes multiple pathways
#'
#' @export

cnetPlotly <- function(enrich, geneSets, deTab) {
  g <- list2graph(geneSets)
  
  g_id <- as.data.frame(V(g)$name[V(g)$name %in% rownames(deTab)])
  colnames(g_id) <- "gene"
  
  g_id <- merge(g_id, deTab[c("avgLog2FC"), drop=FALSE], by.x = "gene", by.y = 0,  all.x = TRUE)
  g_id <-
    g_id[order(match(g_id$gene, V(g)$name[!V(g)$name %in% names(geneSets)])),]
  
  V(g)$color <- g_id$avgLog2FC
  V(g)$name[V(g)$name %in% g_id$entrez] <-
    as.character(g_id$geneName)
  
  g
}


#' The genesets are gathered.
#' Side inforamtion like fc is added.
#' Frequency of genes and pathways is calculated and sorted upon.
#' Heatmap is created with genes, pathways, and log2FC.
#'
#' @param deTab Dataframe, with all analysis results
#' @param geneSets Martrix object, containing all pathways with corresponding genes
#'
#' @return p, (Plotly object) plot
#'
#' @export

heatplotly <- function(geneSets, deTab) {
  genelist <- list2df(geneSets)
  genelist <- merge(
    genelist,
    deTab[c("avgLog2FC"), drop=FALSE],
    by.x = "Gene",
    by.y = 0,
    all.x = TRUE
  )
  genelist <-
    merge(genelist, rev(sort(table(genelist$Gene))), by.x = "Gene", by.y = "Var1")
  if (length(rev(sort(table(genelist$categoryID)))) == 1) {
    genelist <- merge(genelist, rev(sort(table(
      genelist$categoryID
    ))), by.x = "categoryID", by.y = "row.names")
    colnames(genelist)[colnames(genelist) %in% c("Freq", "y")] <-
      c("Freq.x", "Freq.y")
  } else {
    genelist <- merge(genelist, rev(sort(table(
      genelist$categoryID
    ))), by.x = "categoryID", by.y = "Var1")
  }
  
  for (pathway in unique(genelist$categoryID)) {
    entrezID <- genelist$Gene[genelist$categoryID == pathway]
    entrez_matches <- count(genelist$Gene[genelist$categoryID != pathway] %in% entrezID)
    genelist$match[genelist$categoryID == pathway] <- entrez_matches
  }
  
  genelist <-
    genelist[order(-genelist$match,-genelist$Freq.y, genelist$avgLog2FC),]
  
  p <- plot_ly(
    x = ~ genelist$categoryID,
    y = ~ genelist$Gene,
    z = ~ genelist$avgLog2FC,
    colorbar = list(title = "Log2FC", len = 1),
    type = "heatmap",
    hoverinfo = 'text',
    text = paste(
      "Pathway:",
      genelist$categoryID,
      "<br>Gene:",
      genelist$Gene,
      "<br>Log2FC:",
      genelist$avgLog2FC
    )
  ) %>%
    plotly::layout(
      title = "Genes in pathway",
      xaxis = list(
        title = '',
        categoryorder = "array",
        categoryarray = genelist$categoryID
      ),
      yaxis = list(
        title = '',
        categoryorder = "array",
        categoryarray = genelist$Gene
      )
    ) %>%
    config(
      toImageButtonOptions = list(
        format = "png",
        filename = "enrichheatmap",
        width = 1500,
        height = 1000
      )
    )
  p
}


#' Graph layout is set with kamada kawai (kk).
#' Plots are created with dots as a gene or pathway.
#' Lines between dots show connection of these genes and/or pathways.
#' The geneList is prepared with the right LogFC values and links between genes between multiple pathways.
#' The number of pathways that are shown is defined with cnet_slider.
#' Igraph object is made to create links of genes between multiple pathways.
#' Colors of dots are based on LogFC or P-value.
#'
#' @param g Igraph object, containing graph data from genes and/or pathays
#' @param pwName String, Name of the shown pathway
#' @param getColor String, Color given to dots (LogFC or P-Values)
#' @param cnet Integer, Number of cnet pathways to show
#' @param annoP Boolean, Show pathway labels yes or no
#' @param annoG Boolean, Show gene labels yes or no
#'
#' @return p, (Plotly object) plot
#'
#' @export

plotlyGraph <- function(g, pwName, getColor, cnet, annoP, annoG) {
  G <- g
  L <- as.data.frame(layout.kamada.kawai(G))
  vs <- V(G)
  es <- as.data.frame(get.edgelist(G))
  rownames(L) <- names(vs)
  
  L_cnet <- L[0:cnet, ]
  L_genes <- L[(cnet + 1):nrow(L), ]
  vs <- vs[!is.na(vs$color)]
  
  Ne <- length(es[1]$V1)
  network <- plot_ly(
    x = ~ L_genes$V1,
    y = ~ L_genes$V2,
    type = "scattergl",
    mode = "markers",
    marker = list(
      size = 12,
      color = as.numeric(vs$color),
      colorscale = 'Viridis',
      colorbar = list(title = getColor)
    ),
    text = rownames(L_genes),
    key = rownames(L_genes),
    hoverinfo = "text",
    showlegend = FALSE
  ) %>%
    add_trace(
      x = L_cnet$V1,
      y = L_cnet$V2,
      marker = list(
        size = 20,
        color = "red",
        colorbar = FALSE
      ),
      text = rownames(L_cnet),
      key = rownames(L_cnet),
      hoverinfo = "text",
      showlegend = FALSE
    )
  
  edge_shapes <- list()
  for (i in 1:Ne) {
    v0 <- L[as.character(es[i, ]$V1), ]
    v1 <- L[as.character(es[i, ]$V2), ]
    
    edge_shape = list(
      type = "line",
      line = list(color = "#030303", width = 0.2),
      layer = 'below',
      showarrow = TRUE,
      x0 = v0$V1,
      y0 = v0$V2,
      x1 = v1$V1,
      y1 = v1$V2
    )
    
    edge_shapes[[i]] <- edge_shape
  }
  
  axis <-
    list(
      title = "",
      showgrid = FALSE,
      showticklabels = FALSE,
      zeroline = FALSE
    )
  
  p <- plotly::layout(
    network,
    title = pwName,
    shapes = edge_shapes,
    xaxis = axis,
    yaxis = axis
  ) %>%
    config(
      toImageButtonOptions = list(
        format = "png",
        filename = "enrichnet",
        width = 1500,
        height = 1000
      )
    )
  if (isTRUE(annoP)) {
    p <- add_annotations(
      p,
      x = L_cnet$V1,
      y = L_cnet$V2,
      text = sprintf("<b>%s</b>", rownames(L_cnet)),
      showarrow = TRUE,
      arrowwidth = 2,
      arrowhead = 0
    )
  }
  if (isTRUE(annoG)) {
    p <- add_annotations(
      p,
      x = ~ L_genes$V1,
      y = ~ L_genes$V2,
      text = rownames(L_genes),
      showarrow = TRUE,
      arrowwidth = 1,
      arrowhead = 0
    )
  }
  p
}

## --------------------------------------------------------------------------
