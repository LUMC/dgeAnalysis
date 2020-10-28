
## ----- UTIL FUNCTIONS GENE SET ENRICHMENT -----


#' Get LogFC from deTab.
#' Add entrez id to geneList.
#' Sort geneList based on LogFC (decreasing).
#' Remove NA and duplicate names.
#'
#' @param deTab Dataframe, with all analysis results
#'
#' @return geneList, (Vector) Named vector containing gene names and LogFC values
#'
#' @export

get_geneList <- function(deTab) {
  geneList <- deTab$avgLog2FC
  names(geneList) <- as.character(deTab$entrez)
  geneList <- sort(geneList, decreasing = TRUE)
  geneList <- geneList[na.omit(names(geneList))]
  geneList <- geneList[!duplicated(names(geneList))]
  geneList
}


#' Sets a new n value.
#' n stands for the number of pathways to show.
#'
#' @param x Enrichment object, containing enrichment results
#' @param showCategory Integer, number of pathways
#'
#' @return n, (Integer) Updated number of pathways to show
#'
#' @export

update_n <- function(x, showCategory) {
  if (!is.numeric(showCategory)) {
    return(showCategory)
  }
  
  n <- showCategory
  if (nrow(x) < n) {
    n <- nrow(x)
  }
  
  return(n)
}


#' Collect genes within a pathway.
#' Gets genes from enrichment result.
#' geneSets names are set based on ID and description.
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
  slider <- update_n(enrich, slider)
  geneSets <- geneInCategory(enrich)
  y <- as.data.frame(enrich)
  geneSets <- geneSets[y$ID]
  names(geneSets) <- y$Description
  geneSets <- append(head(geneSets, slider), geneSets[selected])
  geneSets <- geneSets[!duplicated(geneSets)]
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


#' Overlap is calculated and define between x and y.
#' The number of overlaps is returned.
#' Igraph object is created based on a vector/list.
#'
#' @param x Vector, genes relative to x axis
#' @param y Vector, genes relative to y axis
#'
#' @return n, (Integer) Number of overlapping values
#'
#' @export

overlap_ratio <- function(x, y) {
  x <- unlist(x)
  y <- unlist(y)
  n <- length(intersect(x, y)) / length(unique(c(x, y)))
  n
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
#' @param value String, Value with the column name to be used (p-, q- or adjP value)
#'
#' @return p, (Plotly object) plot
#'
#' @export

enrichBarplot <- function(enrich, amount, value) {
  enrich <- na.omit(enrich[0:amount, ])
  enrich$Description <- factor(enrich$Description,
                               levels = unique(enrich$Description)[order(enrich[[value]],
                                                                         enrich$Description,
                                                                         decreasing = TRUE)])
  color <- seq(
    from = min(enrich[[value]]),
    to = max(enrich[[value]]),
    length.out = 10
  )[2:9]
  p <- plot_ly(
    enrich,
    x = ~ Count,
    y = ~ Description,
    orientation = 'h',
    type = "bar",
    hoverinfo = 'text',
    text = paste("# Genes:", enrich$Count),
    marker = list(
      color = -enrich[[value]],
      colorscale = 'Viridis',
      colorbar = list(
        title = value,
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


#' Pathways are gathered from enrichment result with pathway description.
#' A Igraph object is created with links between pathways.
#' Colors of dots are based on p-value.
#'
#' @param enrich Enrich result, A enrichment results
#'
#' @return g, (Igraph object) containing links between pathways
#'
#' @export

emap_plotly <- function(enrich) {
  geneSets <- geneInCategory(enrich)
  if (is.null(dim(enrich)) | nrow(enrich) == 1) {
    g <- graph.empty(0, directed = FALSE)
    g <- add_vertices(g, nv = 1)
    V(g)$name <- as.character(enrich$Description)
    V(g)$color <- "red"
  } else {
    id <- enrich[, "ID"]
    geneSets <- geneSets[id]
    n <- nrow(enrich) #
    w <- matrix(NA, nrow = n, ncol = n)
    colnames(w) <- rownames(w) <- enrich$Description
    
    for (i in seq_len(n - 1)) {
      for (j in (i + 1):n) {
        w[i, j] <- overlap_ratio(geneSets[id[i]], geneSets[id[j]])
      }
    }
    
    wd <- melt(w)
    wd <- wd[wd[, 1] != wd[, 2], ]
    wd <- wd[!is.na(wd[, 3]), ]
    g <- graph.data.frame(wd[, -3], directed = FALSE)
    E(g)$width = sqrt(wd[, 3] * 5)
    g <- delete.edges(g, E(g)[wd[, 3] < 0.2])
    idx <-
      unlist(sapply(V(g)$name, function(x)
        which(x == enrich$Description)))
    
    cnt <- sapply(geneSets[idx], length)
    V(g)$size <- cnt
    
    colVar <- enrich[idx, "pvalue"]
    V(g)$color <- colVar
  }
  g
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
  
  g_id <- as.data.frame(V(g)$name[V(g)$name %in% deTab$entrez])
  colnames(g_id) <- "entrez"
  
  if (!"geneName" %in% colnames(deTab)) {
    deTab$geneName <- rownames(deTab)
  }
  
  g_id <-
    merge(g_id, deTab[c("entrez", "geneName", "avgLog2FC")], by = "entrez", all.x =
            TRUE)
  g_id <-
    g_id[order(match(g_id$entrez, V(g)$name[!V(g)$name %in% names(geneSets)])),]
  
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
  genelist <-
    merge(genelist,
          deTab[c("entrez", "avgLog2FC")],
          by.x = "Gene",
          by.y = "entrez",
          all.x = TRUE)
  genelist <-
    merge(genelist, rev(sort(table(genelist$Gene))), by.x = "Gene", by.y = "Var1")
  if (length(rev(sort(table(
    genelist$categoryID
  )))) == 1) {
    genelist <-
      merge(genelist, rev(sort(table(
        genelist$categoryID
      ))), by.x = "categoryID", by.y = "row.names")
    colnames(genelist)[colnames(genelist) %in% c("Freq", "y")] <-
      c("Freq.x", "Freq.y")
  } else {
    genelist <-
      merge(genelist, rev(sort(table(
        genelist$categoryID
      ))), by.x = "categoryID", by.y = "Var1")
  }
  
  for (pathway in unique(genelist$categoryID)) {
    entrezID <- genelist$Gene[genelist$categoryID == pathway]
    entrez_matches <- count(genelist$Gene[genelist$categoryID != pathway] %in% entrezID)
    genelist$match[genelist$categoryID == pathway] <- entrez_matches
  }
  
  if (!"geneName" %in% colnames(deTab)) {
    deTab$geneName <- rownames(deTab)
  }
  
  genelist <-
    merge(genelist,
          deTab[c("entrez", "geneName")],
          by.x = "Gene",
          by.y = "entrez",
          all.x = TRUE)
  genelist <-
    genelist[order(-genelist$match,-genelist$Freq.y, genelist$avgLog2FC),]
  
  p <- plot_ly(
    x = ~ genelist$categoryID,
    y = ~ genelist$geneName,
    z = ~ genelist$avgLog2FC,
    colorbar = list(title = "Log2FC", len = 1),
    type = "heatmap",
    hoverinfo = 'text',
    text = paste(
      "Pathway:",
      genelist$categoryID,
      "<br>Gene:",
      genelist$geneName,
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
        categoryarray = genelist$geneName
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

#add_annotations(
#  x = L[as.character(es$V2),]$V1,
#  y = L[as.character(es$V2),]$V2,
#  xref = "x", yref = "y",
#  axref = "x", ayref = "y",
#  text = "",
#  arrowcolor = "#030303",
#  captureevents = TRUE,
#  arrowwidth = 0.1,
#  arrowsize = 15,
#  showarrow = T,
#  ax = L[as.character(es$V1),]$V1,
#  ay = L[as.character(es$V1),]$V2,
#  standoff=5
#)

## --------------------------------------------------------------------------
