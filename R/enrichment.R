
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
  terms <- enrich$term_name[0:slider]
  geneSets <- splitTerms[terms]
  geneSets <- append(geneSets, splitTerms[names(splitTerms) %in% selected])
  geneSets <- lapply(geneSets, function(geneSets)
      unique(unlist(strsplit(geneSets, ",", perl = T))))
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
