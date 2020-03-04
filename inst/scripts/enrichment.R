
## ----- GENE SET ENRICHMENT -----

# util functions needed for enrichment ---

get_geneList <- function(deTab){
  geneList <- deTab$avgLog2FC
  names(geneList) <- as.character(deTab$entrez)
  geneList <- sort(geneList, decreasing = TRUE)
  geneList <- geneList[na.omit(names(geneList))]
  geneList <- geneList[!duplicated(names(geneList))]
  geneList
}

update_n <- function(x, showCategory) {
  if (!is.numeric(showCategory)) {
    return(showCategory)
  }
  
  ## geneSets <- geneInCategory(x) ## use core gene for gsea result
  n <- showCategory
  if (nrow(x) < n) {
    n <- nrow(x)
  }
  
  return(n)
}

extract_geneSets <- function(x, n) {
  n <- update_n(x, n)
  geneSets <- geneInCategory(x) ## use core gene for gsea result
  y <- as.data.frame(x)
  geneSets <- geneSets[y$ID]
  names(geneSets) <- y$Description
  if (is.numeric(n)) {
    return(geneSets[1:n])
  }
  return(geneSets[n]) ## if n is a vector of Description
}

list2graph <- function(inputList) {
  x <- list2df(inputList)
  g <- igraph::graph.data.frame(x, directed=FALSE)
  return(g)
}

list2df <- function(inputList) {
  ldf <- lapply(1:length(inputList), function(i) {
    data.frame(categoryID=rep(names(inputList[i]),
                              length(inputList[[i]])),
               Gene=inputList[[i]])
  })
  
  do.call('rbind', ldf)
}

overlap_ratio <- function(x, y) {
  x <- unlist(x)
  y <- unlist(y)
  length(intersect(x, y))/length(unique(c(x,y)))
}

get_organismID <- function(deTab){
  id <- rownames(deTab)[nrow(deTab)]
  id <- gsub("[^A-Za-z]","", id)
  id <- sub(".{1}$", "", id)
  id
}
# --- util functions needed for enrichment

enrichBarplot <- function(enrich, amount, value){
  enrich <- na.omit(enrich[0:amount,])
  enrich <- as.data.frame(enrich)
  tryCatch({
    enrich$Count <- lengths(strsplit(enrich$core_enrichment, "/"))
  }, error = function(err) {
    value <<- sub("s$", "", value)
  })
  enrich$Description <- factor(enrich$Description, levels = unique(enrich$Description)[order(enrich[[value]], enrich$Description, decreasing = TRUE)])
  color <- seq(from=min(enrich[[value]]), to=max(enrich[[value]]), length.out = 10)[2:9]
  p <- plot_ly(enrich,
               x = ~Count,
               y = ~Description,
               orientation='h',
               type = "bar",
               marker=list(color=-enrich[[value]],
                           colorscale='Viridis',
                           colorbar=list(title=value,
                                         tickmode="array",
                                         tickvals=-color,
                                         ticktext=floor(color) + signif(color %% 1, 4)),
                           reversescale=FALSE)
                                         
               ) %>% 
    plotly::layout(xaxis = list(title = 'Counts'),
                   title = "Enrichment barplot",
           yaxis = list(title = '')) %>%
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

emap_plotly <- function(enrich){
  geneSets <- geneInCategory(enrich)
  if (is.null(dim(enrich)) | nrow(enrich) == 1) {
    g <- graph.empty(0, directed=FALSE)
    g <- add_vertices(g, nv = 1)
    V(g)$name <- as.character(enrich$Description)
    V(g)$color <- "red"
  } else {
    id <- enrich[,"ID"]
    geneSets <- geneSets[id]
    n <- nrow(enrich) #
    w <- matrix(NA, nrow=n, ncol=n)
    colnames(w) <- rownames(w) <- enrich$Description
    
    for (i in seq_len(n-1)) {
      for (j in (i+1):n) {
        w[i,j] <- overlap_ratio(geneSets[id[i]], geneSets[id[j]])
      }
    }
    
    wd <- melt(w)
    wd <- wd[wd[,1] != wd[,2],]
    wd <- wd[!is.na(wd[,3]),]
    g <- graph.data.frame(wd[,-3], directed=FALSE)
    E(g)$width=sqrt(wd[,3] * 5)
    g <- delete.edges(g, E(g)[wd[,3] < 0.2])
    ## g <- delete.edges(g, E(g)[wd[,3] < 0.05])
    idx <- unlist(sapply(V(g)$name, function(x) which(x == enrich$Description)))
    
    cnt <- sapply(geneSets[idx], length)
    V(g)$size <- cnt
    
    colVar <- enrich[idx, "pvalue"]
    V(g)$color <- colVar
  }
  g
}

viewPathwayPlot <- function(deTab, db, pwName){
  organism <- get_organismID(deTab)
  org2org <- list(ENS="hsapiens",
                  ENSMUS="mmusculus")
  pathways <- eval(parse(text="pathways"))
  pw <- pathways(org2org[[organism]], db)[[pwName]]
  pw <- suppressMessages(convertIdentifiers(pw, "symbol"))
  
  setGeneList <- deTab$avgLog2FC
  names(setGeneList) <- as.character(deTab$geneName)
  setGeneList <- sort(setGeneList, decreasing = TRUE)
  setGeneList <- setGeneList[na.omit(names(setGeneList))]
  setGeneList <- setGeneList[!duplicated(names(setGeneList))]
  
  g <- graphite::pathwayGraph(pw)
  gg <- igraph::igraph.from.graphNEL(g)
  gg <- igraph::as.undirected(gg)
  V(gg)$name <- sub("[^:]+:", "", V(gg)$name)
  
  fc <- setGeneList[V(gg)$name]
  V(gg)$color <- fc
  gg
}

cnetPlotly <- function(enrich, deTab, cnet_slider){
  setGeneList <- deTab$avgLog2FC
  names(setGeneList) <- as.character(deTab$geneName)
  setGeneList <- sort(setGeneList, decreasing = TRUE)
  setGeneList <- setGeneList[na.omit(names(setGeneList))]
  setGeneList <- setGeneList[!duplicated(names(setGeneList))]
  
  geneSets <- extract_geneSets(enrich, cnet_slider)
  
  g <- list2graph(geneSets)
  
  V(g)$name[V(g)$name %in% as.character(deTab$entrez)] <- as.character(deTab$geneName[as.character(deTab$entrez) %in% V(g)$name])
  fc <- setGeneList[V(g)$name]
  V(g)$color <- fc
  
  g
}

plotlyGraph <- function(g, pwName, getColor, cnet){
  G <- g
  L <- as.data.frame(layout.kamada.kawai(G))
  vs <- V(G)
  es <- as.data.frame(get.edgelist(G))
  rownames(L) <- names(vs)
  
  L_cnet <- L[0:cnet,]
  L_genes <- L[(cnet+1):nrow(L),][names(vs)[!is.na(vs$color)],]
  L_genesNA <- L[names(vs)[is.na(vs$color)],]
  vs <- vs[!is.na(vs$color)]
    
  Ne <- length(es[1]$V1)
  network <- plot_ly(
    x = ~L_genesNA$V1,
    y = ~L_genesNA$V2,
    type = "scattergl",
    mode = "markers",
    marker=list(size=12,
                color="white",
                line = list(color = '#999999',
                            width = 1),
                colorbar=FALSE),
    text = rownames(L_genesNA),
    key = rownames(L_genesNA),
    hoverinfo = "text",
    showlegend=FALSE,
    source=pwName) %>%
    add_trace(
      x = ~L_genes$V1,
      y = ~L_genes$V2,
      type = "scattergl",
      mode = "markers",
      marker=list(size=12,
                  color=as.numeric(vs$color),
                  colorscale='Viridis',
                  colorbar=list(title=getColor)),
      text = rownames(L_genes),
      key = rownames(L_genes),
      hoverinfo = "text",
      showlegend=FALSE
    ) %>%
    add_trace(
      x = L_cnet$V1,
      y = L_cnet$V2,
      marker=list(size=20,
                  color="red",
                  colorbar=FALSE),
      text = rownames(L_cnet),
      key = rownames(L_cnet),
      hoverinfo = "text",
      showlegend=FALSE
    )
  
  edge_shapes <- list()
  for(i in 1:Ne) {
    v0 <- L[as.character(es[i,]$V1),]
    v1 <- L[as.character(es[i,]$V2),]
    
    edge_shape = list(
      type = "line",
      line = list(color = "#030303", width = 0.15),
      layer='below',
      showarrow = TRUE,
      x0 = v0$V1,
      y0 = v0$V2,
      x1 = v1$V1,
      y1 = v1$V2
    )
    
    edge_shapes[[i]] <- edge_shape
  }
  
  axis <- list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
  
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

