
#' Prepare data for alignment plot
#'
#' @param se Summarized Experiment, SE to use as input with samplesheet & counts
#' @param percent String, Should output value be in percentages or not
#'
#' @return plot_data, Dataframe with cleaned data
#'
#' @export

alignment_summary <- function(se, percent = "percent") {
  features <- rownames(se)[grepl("^__", rownames(se))]
  plot_data <-  expand.grid(feature = c("aligned", features), sample = colnames(se))
  plot_data$count <- apply(plot_data, 1, getCount, assays(se)$counts)
  plot_data$feature <- gsub("_", " ", gsub("__", "", plot_data$feature))
  
  if (percent == "percent") {
    for (var in unique(plot_data$sample)) {
      temp <- plot_data[plot_data$sample == var, ]
      plot_data$count[plot_data$sample == var] <- temp$count / (sum(temp$count)) * 100
    }
  }
  
  plot_data <- merge(
    x = plot_data,
    y = as.data.frame(colData(se)),
    by.x = "sample",
    by.y = "row.names",
    all.x = TRUE
  )
  plot_data$sample <- factor(plot_data$sample, levels = rev(unique(plot_data$sample)))
  
  return(plot_data)
}


#' Prepare data for complexity plot
#'
#' @param se Summarized Experiment, SE to use as input with samplesheet & counts
#' @param rank Numeric, How many genes should be plotted
#'
#' @return plot_data, Dataframe with cleaned data
#'
#' @export

complexity <- function(se, rank = 1000) {
  compData <- complexityData(se, rank)
  compData <- merge(
    x = compData,
    y = as.data.frame(colData(se)),
    by.x = "sample",
    by.y = "row.names",
    all.x = TRUE
  )
  
  compData$sample <- factor(compData$sample, levels = unique(sort(as.character(compData$sample))))
  return(compData)
}


#' Prepare data for count distribution plot
#'
#' @param dge DGE List, DGE List with samplesheet, count data & annotation
#'
#' @return plot_data, Dataframe with cleaned data
#'
#' @export

count_dist <- function(dge) {
  dens <- apply(dge$counts, 2, density)
  
  data <- data.frame(
    sample = character(),
    x = numeric(),
    y = numeric()
  )
  for (item in 1:length(dens)) {
    temp <- as.data.frame(dens[[item]][c("x", "y")])
    temp$sample <- names(dens[item])
    data <- rbind(data, temp)
  }
  
  data <- merge(
    x = data,
    y = dge$samples,
    by.x = "sample",
    by.y = "row.names",
    all.x = TRUE
  )
  
  return(data)
}


#' Prepare data for violin distribution plot
#'
#' @param dge DGE List, DGE List with samplesheet, count data & annotation
#' @param group String, Value to group/sort values in dataframe by
#'
#' @return plot_data, Dataframe with cleaned data
#'
#' @export

violin_dist <- function(dge, group) {
  stackCounts <- data.frame(stackDge(dge))
  stackCounts <- merge(
    x = stackCounts,
    y = dge$samples,
    by.x = "sample",
    by.y = "row.names",
    all.x = TRUE
  )
  
  stackCounts <- stackCounts[order(as.character(stackCounts[[group]]), as.character(stackCounts$sample)), ]
  stackCounts$sample <- factor(stackCounts$sample, levels = unique(stackCounts$sample))
  return(stackCounts)
}


#' Prepare data for MDS plot
#'
#' @param dge DGE List, DGE List with samplesheet, count data & annotation
#'
#' @return plot_data, Dataframe with cleaned data
#'
#' @export

mds_clust <- function(dge) {
  logFC <- plotMDS(dge$counts, ndim = ncol(dge) - 1)
  for_plots <- data.frame(logFC[c("x", "y")])
  for_plots$sample <- rownames(logFC$distance.matrix.squared)
  
  for_plots <- merge(
    x = for_plots,
    y = dge$samples,
    by.x = "sample",
    by.y = "row.names",
    all.x = TRUE
  )
  
  return(for_plots)
}


#' Prepare data for voom plot
#'
#' @param dge DGE List, DGE List with samplesheet, count data & annotation
#'
#' @return plot_data, Dataframe with cleaned data
#'
#' @export

voom_data <- function(dge) {
  v <- voom(2 ^ (dge$counts), save.plot = TRUE)
  v <- data.frame(
    x = v$voom.xy$x,
    y = v$voom.xy$y,
    gene = names(v$voom.xy$x),
    Genes = "Genes"
  )
  v <- v[order(v$x), ]
  
  return(v)
}


#' Prepare data for PCA plot
#'
#' @param dge DGE List, DGE List with samplesheet, count data & annotation
#'
#' @return plot_data, Dataframe with cleaned data
#'
#' @export

pca_data <- function(dge) {
  tdge <- t(dge$counts)
  tdge[!is.finite(tdge)] <- 0
  pc <- prcomp(tdge, center = TRUE)
  
  pca <- data.frame(scale(tdge, center = T, scale = F) %*% pc$rotation)
  pca$percent <- round(summary(pc)$importance[2, ] * 100, 2)
  pca$sample <- rownames(pca)
  pca$pc <- paste0("PC", 1:nrow(pca))
  pca$pc <- factor(pca$pc, levels = pca$pc)
  
  pca <- merge(
    x = pca,
    y = dge$samples,
    by.x = "sample",
    by.y = "row.names",
    all.x = TRUE
  )
  
  return(pca)
}


#' Prepare data for tSNE plot
#'
#' @param dge DGE List, DGE List with samplesheet, count data & annotation
#'
#' @return plot_data, Dataframe with cleaned data
#'
#' @export

tsne_data <- function(dge) {
  set.seed(1234)
  
  perplexity <- 30
  while (perplexity > 0) {
    try({
      tsne_model <- Rtsne(
        t(dge$counts),
        perplexity = perplexity,
        check_duplicates = FALSE,
        normalize = FALSE
      )
      break
    }, silent = TRUE)
    perplexity <- perplexity - 1
  }
  
  tsne_data <- as.data.frame(tsne_model$Y)
  tsne_data$sample <- colnames(dge$counts)
  
  tsne_data <- merge(
    x = tsne_data,
    y = dge$samples,
    by.x = "sample",
    by.y = "row.names",
    all.x = TRUE
  )
  
  return(tsne_data)
}


#' Prepare data for dendrogram plot
#'
#' @param dge DGE List, DGE List with samplesheet, count data & annotation
#'
#' @return plot_data, Dataframe with cleaned data
#'
#' @export

dendro_data <- function(dge) {
  sampleTree <- hclust(dist(t(dge$counts)), method = "average")
  dendro <- get_dendrogram_data(sampleTree)
  dendro$sample[dendro$label != ""] <- dendro$label[dendro$label != ""]
  
  dendro <- merge(
    x = dendro,
    y = dge$samples,
    by.x = "sample",
    by.y = "row.names",
    all.x = TRUE
  )
  
  return(dendro)
}


#' Prepare data for heatmap plot
#'
#' @param dge DGE List, DGE List with samplesheet, count data & annotation
#' @param amount Numeric, Number of genes that should be present in plot
#'
#' @return plot_data, Dataframe with cleaned data
#'
#' @export

heat_var <- function(dge, amount) {
  lcpm <- dge$counts
  var_genes <- apply(lcpm, 1, var)
  select_var <- names(sort(var_genes, decreasing = TRUE))[1:amount]
  high_var_cpm <- lcpm[select_var, ]
  high_var_cpm <- as.data.frame(stack(high_var_cpm))
  high_var_cpm$row <- factor(x = high_var_cpm$row, levels = rev(unique(high_var_cpm$row)))
  
  high_var_cpm <- merge(
    x = high_var_cpm,
    y = dge$samples,
    by.x = "col",
    by.y = "row.names",
    all.x = TRUE
  )
  
  high_var_cpm$col <- factor(high_var_cpm$col, levels = unique(sort(as.character(high_var_cpm$col))))
  
  return(high_var_cpm)
}


#' Prepare data for heatmap plot
#'
#' @param dge DGE List, DGE List with samplesheet, count data & annotation
#' @param deTab Dataframe, Dataframe with expression results from analysis
#' @param amount Numeric, Number of genes that should be present in plot
#'
#' @return plot_data, Dataframe with cleaned data
#'
#' @export

heat_de <- function(dge, deTab, amount) {
  sortdeTab <- deTab[order(rank(deTab$FDR)), ]
  sortdeTab <- head(sortdeTab, amount)
  getnorm <- dge[rownames(sortdeTab), ]
  getnorm <- getnorm$counts
  getnorm <- as.data.frame(stack(getnorm))
  getnorm$row <- factor(x = getnorm$row, levels = rev(unique(getnorm$row)))
  
  getnorm <- merge(
    x = getnorm,
    y = dge$samples,
    by.x = "col",
    by.y = "row.names",
    all.x = TRUE
  )
  
  getnorm$col <- factor(getnorm$col, levels = unique(sort(as.character(getnorm$col))))
  
  return(getnorm)
}


#' Prepare data for expression ratio plot
#'
#' @param deTab Dataframe, Dataframe with expression results from analysis
#'
#' @return plot_data, Dataframe with cleaned data
#'
#' @export

de_ratio <- function(deTab) {
  defeatures <- data.frame(table(deTab$DE))
  defeatures$perc <- defeatures[, 2] / sum(defeatures[, 2]) * 100
  defeatures$Var1 <- factor(
    x = defeatures$Var1,
    levels = c(-1, 0, 1),
    labels = c(
      "Down-regulated",
      "Not significant",
      "Up-regulated"
    )
  )
  
  return(defeatures)
}


#' Prepare data for MA plot
#'
#' @param deTab Dataframe, Dataframe with expression results from analysis
#'
#' @return plot_data, Dataframe with cleaned data
#'
#' @export

ma <- function(deTab) {
  plot_data <- deTab[order(deTab$avgLog2CPM), ]
  plot_data$DE <- factor(
    x = plot_data$DE,
    levels = c(0, 1, -1),
    labels = c(
      "Not significant",
      "Up-regulated",
      "Down-regulated"
    )
  )
  plot_data$gene <- rownames(plot_data)
  
  return(plot_data)
}


#' Prepare data for volcano plot
#'
#' @param deTab Dataframe, Dataframe with expression results from analysis
#'
#' @return plot_data, Dataframe with cleaned data
#'
#' @export

volcano <- function(deTab) {
  deTab$DE <- factor(
    x = deTab$DE,
    levels = c(0, 1, -1),
    labels = c(
      "Not significant",
      "Up-regulated",
      "Down-regulated"
    )
  )
  deTab$FDR <- -log10(deTab$FDR)
  deTab$gene <- rownames(deTab)
  
  return(deTab)
}


#' Prepare data for barcode plot
#'
#' @param deTab Dataframe, Dataframe with expression results from analysis
#' @param dge DGE List, DGE List with samplesheet, count data & annotation
#' @param amount Numeric, Number of genes that should be present in plot
#' @param select Vector, Manual selected genes that should be added to plot
#'
#' @return plot_data, Dataframe with cleaned data
#'
#' @export

barcode <- function(deTab, dge, amount, select) {
  sortdeTab <- deTab[order(rank(deTab$FDR)), ]
  sortdeTab <- head(sortdeTab, amount)
  getnorm <- inUse_normDge[c(rownames(sortdeTab), select), ]
  stack1 <- as.data.frame(stack(getnorm$counts))
  stack1$sample <- stack1$col
  stack1$row <- factor(x = stack1$row, levels = rev(unique(stack1$row)))
  
  stack1 <- merge(
    x = stack1,
    y = dge$samples,
    by.x = "sample",
    by.y = "row.names",
    all.x = TRUE
  )
  
  return(stack1)
}


#' Prepare data for pvalue plot
#'
#' @param deTab Dataframe, Dataframe with expression results from analysis
#'
#' @return plot_data, Dataframe with cleaned data
#'
#' @export

pvalue_data <- function(deTab) {
  p <- round(deTab$P.Value, digits = 2)
  p <- aggregate(p, by = list(p = p), FUN = length)
  
  return(p)
}


#' Prepare data for GC bias plot
#'
#' @param deTab Dataframe, Dataframe with expression results from analysis
#' @param select String, GC Bias to be selected from annotation, e.g., gene, transcript or exon
#'
#' @return plot_data, Dataframe with cleaned data
#'
#' @export

bias_gc <- function(deTab, select) {
  plot_data <- deTab[order(deTab[[select]]), ]
  plot_data[[select]] <- plot_data[[select]] * 100
  plot_data$gene <- rownames(plot_data)
  
  return(plot_data)
}


#' Prepare data for gene strand bias plot
#'
#' @param deTab Dataframe, Dataframe with expression results from analysis
#'
#' @return plot_data, Dataframe with cleaned data
#'
#' @export

gene_strand <- function(deTab) {
  geneStrand <- as.data.frame(table(deTab$geneStrand, deTab$DE, dnn = c("strand", "DE")))
  geneStrand$DE <- factor(
    x = geneStrand$DE,
    levels = c(-1, 0, 1),
    labels = c(
      "Down-regulated",
      "Not significant",
      "Up-regulated"
    )
  )
  geneStrand$perc <- geneStrand$Freq / sum(geneStrand$Freq) * 100
  geneStrand$strand <- factor(
    x = geneStrand$strand,
    levels = c("+", "-"),
    labels = c(
      "Positive strand",
      "Negative strand"
    )
  )
  
  return(geneStrand)
}


#' Prepare data for enriched bar plot
#'
#' @param enrich Dataframe, Dataframe with enrichment results from analysis
#' @param amount Numeric, Number of terms that should be present in plot
#'
#' @return plot_data, Dataframe with cleaned data
#'
#' @export

enrich_bar <- function(enrich, amount) {
  enrich <- na.omit(enrich[0:amount, ])
  enrich$term_name <- stringr::str_wrap(enrich$term_name, 50)
  enrich$term_name <- factor(enrich$term_name,
                             levels = unique(enrich$term_name)[order(enrich$p_value,
                                                                     enrich$term_name,
                                                                     decreasing = TRUE)])
  enrich$p_value <- as.numeric(enrich$p_value)
  
  return(enrich)
}


#' Prepare data for enriched bar plot with DE genes
#'
#' @param enrich Dataframe, Dataframe with enrichment results from analysis
#' @param amount Numeric, Number of terms that should be present in plot
#' @param deTab Dataframe, Dataframe with expression results from analysis
#'
#' @return plot_data, Dataframe with cleaned data
#'
#' @export

enrich_barDE <- function(enrich, amount, deTab) {
  enrich <- na.omit(enrich[0:amount, ])
  enrich$term_name <- stringr::str_wrap(enrich$term_name, 50)
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
  
  plot_data <- enrich[c("term_name", "down", "up")]
  plot_data$down <- plot_data$down * -1
  plot_data <- stack(plot_data)
  plot_data$ind <- factor(
    x = plot_data$ind,
    levels = c("down", "up"),
    labels = c(
      "Down-regulated",
      "Up-regulated"
    )
  )
  plot_data$name <- enrich$term_name
  
  return(plot_data)
}


#' Prepare data for concept network plot
#'
#' @param deTab Dataframe, Dataframe with expression results from analysis
#' @param graphData Dataframe, Data object with graph location
#' @param terms Numeric, Number of terms that should be present in plot
#'
#' @return plot_data, Dataframe with cleaned data
#'
#' @export

cnet_data <- function(deTab, graphData, terms) {
  set.seed(1234)
  layout <- as.data.frame(layout.kamada.kawai(graphData))
  
  ## Fix layout
  layout <- as.data.frame(layout)
  layout$genes <- names(V(graphData))
  
  ## Expand graph
  layout[1:2] <- layout[1:2] * 10
  
  conns <- get.data.frame(graphData)
  conns$from.x <- layout$V1[match(conns$from, layout$genes)]
  conns$from.y <- layout$V2[match(conns$from, layout$genes)]
  conns$to.x <- layout$V1[match(conns$to, layout$genes)]
  conns$to.y <- layout$V2[match(conns$to, layout$genes)]
  
  term_layout <- layout[1:terms,]
  gene_layout <- layout[terms + 1:nrow(layout), ]
  
  gene_layout$fc <- deTab$avgLog2FC[match(gene_layout$genes, rownames(deTab))]
  
  cnet <- list(
    conns,
    term_layout,
    gene_layout
  )
  return(cnet)
}
