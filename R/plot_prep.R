
alignment_summary <- function(se) {
  plot_data <- alignmentSummary(se)
  plot_data$feature <- gsub("_", " ", gsub("__", "", plot_data$feature))
  
  for (var in unique(plot_data$sample)) {
    temp <- plot_data[plot_data$sample == var, ]
    plot_data$count[plot_data$sample == var] <- temp$count / (sum(temp$count)) * 100
  }
  
  return(plot_data)
}

complexity <- function(se, rank = 1000) {
  compData <- complexityData(se, rank)
  compData <- merge(
    x = compData,
    y = as.data.frame(colData(se)),
    by.x = "sample",
    by.y = "row.names",
    all.x = TRUE
  )
  
  return(compData)
}

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

violin_dist <- function(dge) {
  stackCounts <- data.frame(stackDge(dge))
  stackCounts <- merge(
    x = stackCounts,
    y = dge$samples,
    by.x = "sample",
    by.y = "row.names",
    all.x = TRUE
  )
  
  return(stackCounts)
}

mds_clust <- function(dge, group) {
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

pca_data <- function(dge) {
  tdge <- t(dge$counts)
  tdge[!is.finite(tdge)] <- 0
  pc <- prcomp(tdge, center = TRUE)
  
  pca <- data.frame(scale(tdge, center = T, scale = F) %*% pc$rotation)
  pca$percent <- round(summary(pc)$importance[2, ] * 100, 2)
  pca$pc <- paste0("PC", 1:nrow(pca))
  pca$sample <- rownames(pca)
  
  pca <- merge(
    x = pca,
    y = dge$samples,
    by.x = "sample",
    by.y = "row.names",
    all.x = TRUE
  )
  
  return(pca)
}

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

heat_var <- function(dge, amount) {
  lcpm <- dge$counts
  var_genes <- apply(lcpm, 1, var)
  select_var <- names(sort(var_genes, decreasing = TRUE))[1:amount]
  high_var_cpm <- lcpm[select_var, ]
  high_var_cpm <- as.data.frame(stack(high_var_cpm))
  
  high_var_cpm <- merge(
    x = high_var_cpm,
    y = dge$samples,
    by.x = "col",
    by.y = "row.names",
    all.x = TRUE
  )
  
  return(high_var_cpm)
}

heat_de <- function(dge, deTab, amount) {
  sortdeTab <- deTab[order(rank(deTab$FDR)), ]
  sortdeTab <- head(sortdeTab, amount)
  getnorm <- dge[rownames(sortdeTab), ]
  getnorm <- getnorm$counts
  getnorm <- as.data.frame(stack(getnorm))
  
  getnorm <- merge(
    x = getnorm,
    y = dge$samples,
    by.x = "col",
    by.y = "row.names",
    all.x = TRUE
  )
  
  return(getnorm)
}


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

barcode <- function(deTab, amount, select) {
  sortdeTab <- deTab[order(rank(deTab$FDR)), ]
  sortdeTab <- head(sortdeTab, amount)
  getnorm <- inUse_normDge[c(rownames(sortdeTab), select), ]
  stack1 <- as.data.frame(stack(getnorm$counts))
  stack1$sample <- stack1$col
  
  stack1 <- merge(
    x = stack1,
    y = dge$samples,
    by.x = "sample",
    by.y = "row.names",
    all.x = TRUE
  )
  
  return(stack1)
}

pvalue <- function(deTab) {
  p <- round(deTab$P.Value, digits = 2)
  p <- aggregate(p, by = list(p = p), FUN = length)
  
  return(p)
}

bias_gc <- function(deTab, select) {
  plot_data <- deTab[order(deTab[[select]]), ]
  plot_data[[select]] <- plot_data[[select]] * 100
  plot_data$gene <- rownames(plot_data)
  
  return(plot_data)
}

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

enrich_bar <- function(enrich, amount) {
  enrich <- na.omit(enrich[0:amount, ])
  enrich$term_name <- factor(enrich$term_name,
                             levels = unique(enrich$term_name)[order(enrich$p_value,
                                                                     enrich$term_name,
                                                                     decreasing = TRUE)])
  enrich$p_value <- as.numeric(enrich$p_value)
  
  return(enrich)
}

enrich_barDE <- function(enrich, amount) {
  enrich <- na.omit(enrich[0:amount, ])
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
  plot_data$name <- enrich$term_name
  
  return(plot_data)
}

cnet_data <- function(deTab, graphData) {
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
  
  term_layout <- layout[1:5, ]
  gene_layout <- layout[6:nrow(layout), ]
  
  gene_layout$fc <- deTab$avgLog2FC[match(gene_layout$genes, rownames(deTab))]
  
  cnet <- list(
    conns,
    term_layout,
    gene_layout
  )
  return(cnet)
}

heat_terms <- function(geneSets) {
  genelist <- list2df(geneSets)
  genelist <- merge(
    genelist,
    inUse_deTab[c("avgLog2FC"), drop = FALSE],
    by.x = "Gene",
    by.y = 0,
    all.x = TRUE
  )
  genelist <-
    merge(genelist, rev(sort(table(genelist$Gene))), by.x = "Gene", by.y = "Var1")
  if (length(rev(sort(table(
    genelist$categoryID
  )))) == 1) {
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
    entrez_matches <-
      count(genelist$Gene[genelist$categoryID != pathway] %in% entrezID)
    genelist$match[genelist$categoryID == pathway] <-
      entrez_matches
  }
  
  genelist <- genelist[order(-genelist$match, -genelist$Freq.y, genelist$avgLog2FC), ]
  genelist$Gene <- factor(genelist$Gene, levels = unique(genelist$Gene))
  genelist$categoryID <- factor(genelist$categoryID, levels = unique(genelist$categoryID))
  
  return(genelist)
}
