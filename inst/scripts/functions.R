

## All required functions of Shiny app ##

## --------------------------------------------------------------------------

## ----- Read files -----

## Add counts to se
readCountsFromTable <- function(data_counts, data_samples) {
  out <- as.matrix(data_counts[, colnames(data_counts) %in% rownames(data_samples)])
  se <- SummarizedExperiment(assays=list(counts=out))
  se
}

## Add samples to se
addSamplesFromTableToSE <- function(se, data_samples){
  data_samples <- droplevels(data_samples)
  samples <- intersect(colnames(se), rownames(data_samples))
  se <- se[,samples]
  colData(se) <- DataFrame(data_samples[samples,])
  names(colData(se)) <- colnames(data_samples)
  se
}

## Add annotation to se
addAnnotationsFromTableToSE <- function(se, data_annotation){
  features <- intersect(rownames(se), rownames(data_annotation))
  se <- se[features,]
  rowData(se) <- DataFrame(data_annotation[features,])
  se
}

## --------------------------------------------------------------------------

## ----- Analysis utility -----

getCount <- function(x, raw){
  if (x["feature"] %in% rownames(raw)) {
    return(raw[x["feature"], x["sample"]])
  } else {
    features <- rownames(raw)[! grepl("^__", rownames(raw))]
    return(sum(raw[features, x["sample"]]))
  }
}

alignmentSummary <- function(se){
  specialFeatures <- rownames(se)[ grepl( "^__", rownames(se) ) ]
  out <- expand.grid(feature=c("aligned", specialFeatures), sample=colnames(se))
  out$count <- apply(out, 1, getCount, assays(se)$counts)
  out
}

complexityData <- function(se, max=1000){
  features <- rownames(se)[ ! grepl( "^__", rownames(se) ) ]
  ranks <- c(1:max)
  out <- expand.grid(rank=ranks, sample=colnames(se))
  for (x in colnames(se)){
    values <- as.vector(assay(se)[features,x])
    sorted <- sort(values, T)
    total <- sum(sorted)
    out[out$sample == x, "value"] <- cumsum(sorted)[1:max]
    out[out$sample == x, "fraction"] <- out[out$sample == x, "value"] /total
  }
  out
}

stackDge <- function(dge){
  count <- stack(dge$counts)
  names(count) <- c("feature", "sample", "logCPM")
  count
}

gamConfidenceFit <- function(deTab, biasColumn) {
  method.args = list()
  method.args$method <- "REML"
  
  formula <- avgLog2FC ~ s(columnHere, bs = "cs")
  formula <- paste(gsub("columnHere", parse(text=biasColumn), formula))
  formula <- eval(parse(text = gsub("\\", "", paste(formula[2], formula[3], sep=" ~ "), fixed=TRUE)))
  
  base.args <- list(quote(formula), data = quote(deTab))
  gamModel <- do.call(mgcv::gam, c(base.args, method.args))
  prediction <- augment(gamModel)
  prediction <- prediction[order(prediction[[biasColumn]]), ]
  
  setRange <- range(1, nrow(prediction))
  result <- round(seq(setRange[1], setRange[2], length.out = 500))
  prediction <- prediction[c(result),]
}

quaternary_enrichment <- function(deTab){
  #IPA
  deTab <- na.omit(deTab)
  deTab <- deTab[!duplicated(deTab$entrez),]
  
  qPrepare <- deTab[order(rank(deTab$adj.P.Val)),]
  qPrepare <- head(qPrepare, 100)
  qPrepare <- qPrepare[,c ('entrez', 'P.Value', 'avgLog2FC')]
  
  colnames(qPrepare) <- c('entrez', 'pvalue', 'fc')
  
  quaternary_results <- RunCRE_HSAStringDB(qPrepare, method = "Quaternary",
                                           fc.thresh = log2(1.3), pval.thresh = 0.05,
                                           only.significant.pvalues = TRUE,
                                           significance.level = 0.05,
                                           epsilon = 1e-16,
                                           relations = NULL, entities = NULL)
  quaternary_results1 <- quaternary_results[c("uid","symbol","regulation","pvalue")]
  
  ternary_results <- RunCRE_HSAStringDB(qPrepare, method = "Ternary",
                                        fc.thresh = log2(1.3), pval.thresh = 0.05,
                                        only.significant.pvalues = TRUE,
                                        significance.level = 0.05,
                                        epsilon = 1e-16,
                                        relations = NULL, entities = NULL)
  ternary_results1 <- ternary_results[c("uid","symbol","regulation","pvalue")]
  
  enrichment_results <- RunCRE_HSAStringDB(qPrepare, method = "Enrichment",
                                           fc.thresh = log2(1.3), pval.thresh = 0.05,
                                           only.significant.pvalues = TRUE,
                                           significance.level = 0.05,
                                           epsilon = 1e-16,
                                           relations = NULL, entities = NULL)
  enrichment_results1 <- enrichment_results[c("uid","symbol","regulation","pvalue")]
}

## --------------------------------------------------------------------------

## ----- ALIGNMENT PLOTS -----

alignmentSummaryPlot <- function(se, perc=T){
  lse <- alignmentSummary(se)
  
  if (perc) {
    for (var in unique(lse$sample)) {
      temp <- lse[lse$sample == var, ]
      lse$count[lse$sample == var] <- temp$count/(sum(temp$count))
    }
    p <- plot_ly(lse,
                 x = ~count,
                 y = ~sample,
                 orientation='h',
                 color = ~gsub("_", " ", gsub("__", "", feature)),
                 type = "bar") %>% 
      plotly::layout(xaxis = list(title = 'Counts', tickformat = "%"),
                     title = "Count assignments %",
             yaxis = list(title = ''),
             barmode = 'stack') %>%
      config(
        toImageButtonOptions = list(
          format = "svg",
          filename = "alignment_perc",
          width = 750,
          height = 500
        )
      )
  } else {
    p <- plot_ly(lse,
                 x = ~count,
                 y = ~sample,
                 orientation='h',
                 color = ~gsub("_", " ", gsub("__", "", feature)),
                 type = "bar") %>% 
      plotly::layout(xaxis = list(title = 'Counts'),
                     title = "Count assignments",
             yaxis = list(title = ''),
             barmode = 'stack') %>%
      config(
        toImageButtonOptions = list(
          format = "svg",
          filename = "alignment",
          width = 750,
          height = 500
        )
      )
  }
  p
}

complexityPlot <- function(se, perc=T) {
  maxRank=1000
  data <- complexityData(se, maxRank)
  
  if (perc){
    p <- plot_ly(data,
                 x = ~rank,
                 y = ~fraction,
                 color = ~sample,
                 type = "scattergl",
                 mode="lines+markers") %>%
      plotly::layout(xaxis = list(title = 'Rank', type="log"),
                     title = "Gene complexity %",
             yaxis = list(tickformat = "%", title = 'Cumulative fraction of total reads till rank')) %>%
      config(
        toImageButtonOptions = list(
          format = "svg",
          filename = "complexity_perc",
          width = 750,
          height = 500
        )
      )
  } else {
    p <- plot_ly(data,
                 x = ~rank,
                 y = ~value,
                 color = ~sample,
                 type = "scattergl",
                 mode="lines+markers") %>%
      plotly::layout(xaxis = list(title = 'Rank', type="log"),
                     title = "Gene complexity",
             yaxis = list(title = 'Cumulative reads till rank')) %>%
      config(
        toImageButtonOptions = list(
          format = "svg",
          filename = "complexity",
          width = 750,
          height = 500
        )
      )
  }
  p
}

## --------------------------------------------------------------------------

## ----- RAW DATA PLOTS / NORMALIZATION PLOTS -----

countDistributionLinePlot <- function(dge){
  stackCounts <- data.frame(stackDge(dge))
  
  p <- plot_ly(type = 'scattergl',
               mode = 'lines',
               source="dist_line") %>%
    plotly::layout(xaxis = list(title = 'Log2CPM'),
                   title = "Gene count distribution",
           yaxis = list(title = 'Density')) %>%
    config(
      toImageButtonOptions = list(
        format = "svg",
        filename = "countdistline",
        width = 750,
        height = 500
      )
    )
  for (var in unique(stackCounts$sample)) {
    temp <- stackCounts[stackCounts$sample == var, ]
    density <- density(temp$logCPM)
    p <- add_trace(p,
                   x = density$x,
                   y = density$y,
                   name=var,
                   fill = 'tozeroy',
                   alpha = 0.05)
  }
  p
}

countDistributionBoxPlot <- function(dge){
  stackCounts <- data.frame(stackDge(dge))
  
  p <- plot_ly(type = 'box',
               boxpoints = FALSE) %>%
    config(
      toImageButtonOptions = list(
        format = "svg",
        filename = "countdistbox",
        width = 750,
        height = 500
      )
    )
  for (var in unique(stackCounts$sample)) {
    temp <- stackCounts[stackCounts$sample == var, ]
    p <- add_trace(p,
                   y = temp$logCPM,
                   name=var,
                   alpha = 0.5) %>%
      plotly::layout(xaxis = list(title = ''),
                     title = "Gene count distribution",
             yaxis = list(title = 'Log2CPM'))
  }
  p
}

#NOT IN AT THE MOMENT
#zonder log onoverzichtelijk
countPerSampleLinePlot <- function(dge){
  counts <- dge$counts
  
  p <- plot_ly(type = 'scattergl',
               mode = 'lines') %>% 
    plotly::layout(xaxis = list(title = 'Counts'),
           yaxis = list(title = 'Density'))
  for (var in unique(colnames(counts))) {
    temp <- counts[,var]
    density <- density(temp)
    p <- add_trace(p,
                   x=density$x,
                   y = density$y,
                   name=var,
                   fill = 'tozeroy',
                   alpha = 0.05)
  }
  p
}

#NOT IN AT THE MOMENT
#zonder log onoverzichtelijk
countPerSampleBoxPlot <- function(dge){
  counts <- dge$counts
  
  p <- plot_ly(type = 'box',
               boxpoints = FALSE) %>% 
    plotly::layout(yaxis = list(title = 'Counts'))
  for (var in unique(colnames(counts))) {
    temp <- counts[,var]
    p <- add_trace(p,
                   y = temp,
                   name=var,
                   alpha = 0.5)
  }
  p
}

multidimensionalScaling2dPlot <- function(dge, color, type){
  logFC <- plotMDS(dge$counts, ndim = ncol(dge)-1)
  for_plots <- data.frame(logFC$cmdscale.out)
  for_plots$group <- dge$samples[,color]
  
  p <- plot_ly(for_plots,
               x = ~X1,
               y = ~X2,
               type = "scattergl",
               mode = "markers",
               color = ~for_plots$group,
               text = rownames(for_plots),
               hoverinfo = 'text',
               marker = list(size=15,
                             line = list(color = '#999999',
                                         width = 1)),
               key = ~rownames(for_plots),
               source=paste(type, "un_cluster_2d", sep="")) %>%
    plotly::layout(title = paste("MDS Plot, Grouped By:", color),
                   xaxis = list(title = 'MDS1'),
                   yaxis = list(title = 'MDS2'),
           clickmode = "event+select",
           dragmode = "select") %>%
    config(
      toImageButtonOptions = list(
        format = "svg",
        filename = "mds2d",
        width = 750,
        height = 500
      )
    )
  p
}

multidimensionalScaling3dPlot <- function(dge, color){
  logFC <- plotMDS(dge$counts, ndim = ncol(dge)-1)
  for_plots <- data.frame(logFC$cmdscale.out)
  for_plots$group <- dge$samples[,color]
  
  p <- plot_ly(for_plots,
          x = ~X1,
          y = ~X2,
          z = ~X3,
          color = ~for_plots$group,
          text = rownames(for_plots),
          hoverinfo = 'text') %>%
    add_markers() %>%
    plotly::layout(title = paste("MDS Plot, Grouped By:", color),
                   xaxis = list(title = 'MDS1'),
                   yaxis = list(title = 'MDS2'),
                   zaxis = list(title = 'MDS3')) %>%
    config(
      toImageButtonOptions = list(
        format = "svg",
        filename = "mds3d",
        width = 750,
        height = 500
      )
    )
  p
}

## --------------------------------------------------------------------------

## ----- PCA PLOTS -----

variancePcaPlot <- function(dge){
  tdge <- t(dge$counts)
  tdge[!is.finite(tdge)] <- 0
  pca <- prcomp(tdge, center=TRUE)
  percent <- data.frame(summary( pca )$importance[2,])
  colnames(percent) <- "percent"
  
  p <- plot_ly(percent,
               x=rownames(percent),
               y=~percent,
               type="bar") %>% 
    plotly::layout(xaxis = list(title = 'Component', categoryorder='trace'),
                   title = "PCA Scree",
           yaxis = list(title = 'Percentage', tickformat = ".2%")) %>%
    config(
      toImageButtonOptions = list(
        format = "svg",
        filename = "pcascree",
        width = 750,
        height = 500
      )
    )
  p
}

samplePca2dPlot <- function(dge, color){
  tdge <- t(dge$counts)
  tdge[!is.finite(tdge)] <- 0
  pca <- prcomp(tdge, center=TRUE)
  
  pca <- data.frame(scale(tdge, center=T, scale=F)%*%pca$rotation)
  pca$group <- dge$samples[,color]
  
  color <- "phenotype"
  
  p <- plot_ly(pca,
               x=~PC1,
               y=~PC2, 
               type = "scattergl",
               mode = "markers",
               color=~pca$group,
               text = rownames(pca),
               hoverinfo = 'text',
               marker = list(size=15,
                             line = list(color = '#999999',
                                         width = 1)),
               key = ~rownames(pca),
               source="samples_pca_2d") %>%
    plotly::layout(title = 'PCA 2D',
           clickmode = "event+select",
           dragmode = "select") %>%
    config(
      toImageButtonOptions = list(
        format = "svg",
        filename = "pca2d",
        width = 750,
        height = 500
      )
    )
  p
}

samplePca3dPlot <- function(dge, color){
  tdge <- t(dge$counts)
  tdge[!is.finite(tdge)] <- 0
  pca <- prcomp(tdge, center=TRUE)
  
  pca <- data.frame(scale(tdge, center=T, scale=F)%*%pca$rotation)
  pca$group <- dge$samples[,color]
  
  color <- "phenotype"
  
  p <- plot_ly(pca,
               x=~PC1,
               y=~PC2,
               z=~PC3,
               color=~pca$group,
               text = rownames(pca),
               hoverinfo = 'text') %>%
    add_markers(marker = list(size=5, opacity = 0.75)) %>%
    plotly::layout(title = 'PCA 3D') %>%
    config(
      toImageButtonOptions = list(
        format = "svg",
        filename = "pca3d",
        width = 750,
        height = 500
      )
    )
  p
}

#NOT IN AT THE MOMENT
genesPca2dPlot <- function(dge, color){
  pca <- princomp(dge, cor = TRUE)
  pca <- data.frame(pca$scores)
  pca <- head(pca, 50)
  k <- kmeans(pca, 5, nstart=25, iter.max=1000)
  
  p <- plot_ly(pca,
               x=~Comp.1,
               y=~Comp.2,
               type = "scattergl",
               mode = "markers",
               color=~as.character(k$cluster),
               text = rownames(pca),
               hoverinfo = 'text',
               marker = list(size=15)) %>%
    plotly::layout(title = 'PCA')
  p
}

#NOT IN AT THE MOMENT
genesPca3dPlot <- function(dge, color){
  pca <- princomp(dge, cor = TRUE)
  pca <- data.frame(pca$scores)
  pca <- head(pca, 50)
  k <- kmeans(pca, 5, nstart=25, iter.max=1000)
  
  p <- plot_ly(pca,
               x=~Comp.1,
               y=~Comp.2,
               z=~Comp.3,
               color=~as.character(k$cluster),
               text = rownames(pca),
               hoverinfo = 'text') %>%
    add_markers(marker = list(size=3, opacity = 0.75)) %>%
    plotly::layout(title = 'PCA')
  p
}

#NOT IN AT THE MOMENT
samplePcaCpmPlot <- function(dge, color){
  lcpm <- cpm(dge, log=T)
  pca <- prcomp(lcpm, scale = TRUE)
  pca <- data.frame(pca$rotation)
  
  p <- plot_ly(pca,
               x=~PC1,
               y=~PC2,
               z=~PC3,
               color=~color,
               text = rownames(pca),
               hoverinfo = 'text') %>%
    add_markers(marker = list(size=5, opacity = 0.75)) %>%
    plotly::layout(title = 'PCA')
  p
}

#NOT IN AT THE MOMENT
genesPcaCpmPlot <- function(dge, color){
  lcpm <- cpm(dge, log=T)
  pca <- princomp(lcpm, cor = TRUE)
  pca <- data.frame(pca$scores)
  pca <- head(pca, 50)
  k <- kmeans(pca, 5, nstart=25, iter.max=1000)
  
  p <- plot_ly(pca,
               x=~Comp.1,
               y=~Comp.2,
               z=~Comp.3,
               color=~as.character(k$cluster),
               text = rownames(pca),
               hoverinfo = 'text') %>%
    add_markers(marker = list(size=3, opacity = 0.75)) %>%
    plotly::layout(title = 'PCA')
  p
}

## --------------------------------------------------------------------------

## ----- HEATMAPS PLOTS -----

variableHeatmapPlot <- function(dge){
  lcpm <- dge$counts
  var_genes <- apply(lcpm, 1, var)
  head(var_genes)
  select_var <- names(sort(var_genes, decreasing=TRUE))[1:100]
  high_var_cpm <- lcpm[select_var,]
  
  p <- plot_ly(
          x=~colnames(high_var_cpm),
          y=~rownames(high_var_cpm),
          z=~high_var_cpm,
          colorbar = list(title = "Log2CPM", len=1),
          type = "heatmap") %>%
    plotly::layout(xaxis = list(title = ''),
                   title = "Top 100 most variable genes",
           yaxis = list(title = '')) %>%
    config(
      toImageButtonOptions = list(
        format = "svg",
        filename = "heatmapvar",
        width = 750,
        height = 500
      )
    )
  p
}

topDgeHeatmapPlot <- function(deTab, dge){
  sortdeTab <- deTab[order(rank(deTab$adj.P.Val)),]
  sortdeTab <- head(sortdeTab, 100)
  getnorm <- dge[rownames(sortdeTab),]
  getnorm <- getnorm$counts
  
  p <- plot_ly(
    x=~colnames(getnorm),
    y=~rownames(getnorm),
    z=~getnorm,
    colorbar = list(title = "Log2CPM", len=1),
    type = "heatmap") %>%
    plotly::layout(xaxis = list(title = ''),
                   title = "Top 100 most expressed genes",
           yaxis = list(title = '',
                        categoryorder = "array",
                        autorange = "reversed")) %>%
    config(
      toImageButtonOptions = list(
        format = "svg",
        filename = "heatmapdge",
        width = 750,
        height = 500
      )
    )
  p
}

## --------------------------------------------------------------------------

## ----- ANALYSIS PLOTS -----

#analysis en view plots verschillen op een of andere manier?!
voomPlot <- function(dge, deTab, ps){
  v <- voom(2^(dge$counts), save.plot = TRUE)
  
  p <- plot_ly(x = ~v$voom.xy$x,
               y = ~v$voom.xy$y,
               type = "scattergl",
               mode = "markers",
               color = "Voom",
               alpha = 0.75,
               text = names(v$voom.xy$x),
               hoverinfo = 'text',
               key = ~names(v$voom.xy$x),
               source="analysis_plots") %>%
    add_trace(data = deTab[rownames(deTab) %in% ps,],
              x=~v$voom.xy$x[names(v$voom.xy$x) %in% ps],
              y=~v$voom.xy$y[names(v$voom.xy$y) %in% ps],
              color="Selected",
              marker = list(
                color = '#d62728'
              ),
              alpha = 0.75,
              text = names(v$voom.xy$x[names(v$voom.xy$x) %in% ps]),
              hoverinfo = 'text',
              key = ~names(v$voom.xy$x[names(v$voom.xy$x) %in% ps])) %>%
    add_trace(mode = "lines",
              x = v$voom.line$x,
              y = v$voom.line$y,
              line = list(color = 'rgb(255, 127, 14, 0.75)'),
              name = "Voom average") %>%
    plotly::layout(xaxis = list(title = 'Average Log2 Count'),
                   title = "Voom",
           yaxis = list(title = 'SQRT(Standard Deviation)'),
           clickmode = "event+select",
           dragmode = "select") %>%
    config(
      toImageButtonOptions = list(
        format = "svg",
        filename = "voom",
        width = 750,
        height = 500
      )
    )
  p
}

#NOT IN AT THE MOMENT
residualVariancePlot <- function(deTab){
  p <- plot_ly(
            x = ~deTab$AveExpr,
            y = ~log2(deTab$sigma),
            type = "scattergl",
            mode = "markers",
            alpha = 0.75,
            text = rownames(deTab$genes),
            hoverinfo = 'text') %>%
    plotly::layout(xaxis = list(title = 'Average Expression'),
           yaxis = list(title = 'Log2 Sigma'))
  p <- add_trace(p,
                 y = mean(log2(deTab$sigma)),
                 mode = "line"
  )
  p
}

deRatioPlot <- function(deTab){
  defeatures <- aggregate(deTab$DE, by=list(category=deTab$DE), FUN=length)
  defeatures$perc <- defeatures[,2]/sum(defeatures[,2])
  
  defeatures$category[which(defeatures$category==0)] <- "Not sign"
  defeatures$category[which(defeatures$category==-1)] <- "Down"
  defeatures$category[which(defeatures$category==1)] <- "Up"
  
  p <- plot_ly(defeatures,
               x = "",
               y = ~perc,
               color = ~category,
               type = "bar",
               text = defeatures[,2],
               textposition = "auto",
               textfont=list(color="black"),
               hoverinfo = 'text') %>%
    plotly::layout(barmode = 'stack',
           xaxis = list(title = ""),
           title = "Differential expression ratio",
           yaxis = list(tickformat = "%", title = "Ratio")) %>%
    config(
      toImageButtonOptions = list(
        format = "svg",
        filename = "dgeratio",
        width = 750,
        height = 500
      )
    )
  p
}

ma_plot <- function(deTab, ps){
  prediction <- gamConfidenceFit(deTab, "avgLog2CPM")
  
  p <- plot_ly(deTab[deTab$DE == 0,],
               x=~avgLog2CPM,
               y=~avgLog2FC,
               type = "scattergl",
               mode = "markers",
               color=~as.character(DE),
               alpha = 0.75,
               text = rownames(deTab[deTab$DE == 0,]),
               hoverinfo = 'text',
               key = ~rownames(deTab[deTab$DE == 0,]),
               source="analysis_plots") %>%
    add_trace(x=~deTab[deTab$DE != 0,]$avgLog2CPM,
              y=~deTab[deTab$DE != 0,]$avgLog2FC,
              color=as.character(deTab[deTab$DE != 0,]$DE),
              alpha = 0.75,
              text = rownames(deTab[deTab$DE != 0,]),
              hoverinfo = 'text',
              key = ~rownames(deTab[deTab$DE != 0,])) %>%
    add_trace(data = deTab[rownames(deTab) %in% ps,],
              x=~avgLog2CPM,
              y=~avgLog2FC,
              color="Selected",
              marker = list(
                color = '#d62728'
              ),
              alpha = 0.75,
              text = rownames(deTab[rownames(deTab) %in% ps,]),
              hoverinfo = 'text',
              key = ~rownames(deTab[rownames(deTab) %in% ps,])) %>%
    add_trace(data = prediction,
              mode="lines",
              x = ~avgLog2CPM,
              y = ~.fitted,
              text = NA,
              key = NA,
              color = "Fitted",
              line = list(color = 'rgba(7, 164, 181, 1)'),
              name = "Fitted") %>%
    add_ribbons(data = prediction,
                ymin = ~.fitted - 1.96 * .se.fit,
                ymax = ~.fitted + 1.96 * .se.fit,
                fillcolor = "rgba(7, 164, 181, 0.25)",
                text = NA,
                key = NA,
                color = "Standard Error",
                line = list(color = 'rgba(0, 0, 0, 0)'),
                name = "Standard Error") %>%
    plotly::layout(xaxis = list(title = 'Average Log2 CPM'),
                   title = "MA",
           yaxis = list(title = 'Average Log2 FC'),
           clickmode = "event+select",
           dragmode = "select") %>%
    config(
      toImageButtonOptions = list(
        format = "svg",
        filename = "ma",
        width = 750,
        height = 500
      )
    )
  p
}

volcanoPlot <- function(deTab, LogCut, PCut, ps){
  p <- plot_ly(deTab[deTab$DE == 0,],
               x=~avgLog2FC,
               y=~-log10(adj.P.Val),
               type = "scattergl",
               mode = "markers",
               color=~as.character(DE),
               alpha = 0.75,
               text = rownames(deTab[deTab$DE == 0,]),
               hoverinfo = 'text',
               key = ~rownames(deTab[deTab$DE == 0,]),
               source="analysis_plots") %>%
    add_trace(x=~deTab[deTab$DE != 0,]$avgLog2FC,
              y=~-log10(deTab[deTab$DE != 0,]$adj.P.Val),
              color=as.character(deTab[deTab$DE != 0,]$DE),
              alpha = 0.75,
              text = rownames(deTab[deTab$DE != 0,]),
              hoverinfo = 'text',
              key = ~rownames(deTab[deTab$DE != 0,])) %>%
    add_trace(data = deTab[rownames(deTab) %in% ps,],
              x=~avgLog2FC,
              y=~-log10(adj.P.Val),
              color="Selected",
              marker = list(
                color = '#d62728'
              ),
              alpha = 0.75,
              text = rownames(deTab[rownames(deTab) %in% ps,]),
              hoverinfo = 'text',
              key = ~rownames(deTab[rownames(deTab) %in% ps,])) %>%
    plotly::layout(xaxis = list(title = 'Average Log2 FC'),
                   title = "Volcano",
           yaxis = list(title = '- Log10 P-Value'),
           shapes=list(
             list(type = "line",  line = list(color = "red"),
                  x0 = 0,  x1 = 1, xref="paper",
                  y0 = PCut, y1 = PCut),
             
             list(type = "line",  line = list(color = "red"),
                  x0 = LogCut,  x1 = LogCut,
                  y0 = 0, y1 = 1, yref="paper"),
             list(type = "line",  line = list(color = "red"),
                  x0 = -LogCut,  x1 = -LogCut,
                  y0 = 0, y1 = 1, yref="paper")
           ),
           clickmode = "event+select",
           dragmode = "select"
    ) %>%
    config(
      toImageButtonOptions = list(
        format = "svg",
        filename = "volcano",
        width = 750,
        height = 500
      )
    )
  p
}

barcodePlot <- function(deTab, dge, color, ps) {
  if (is.null(color)) {
    return(NULL)
  }
  
  sortdeTab <- deTab[order(rank(deTab$adj.P.Val)),]
  sortdeTab <- head(sortdeTab, 25)
  sortdeTab <- unique(rbind(sortdeTab, deTab[rownames(deTab) %in% ps,]))
  getnorm <- dge[rownames(sortdeTab),]
  getnorm$counts <- getnorm$counts
  stack1 <- as.data.frame(stack(getnorm$counts))
  stack1$group <- getnorm$samples[[color]][stack1$col]
  
  p <- plot_ly(type = "scattergl",
               mode = "markers",
               marker = list(symbol = "line-ns-open",
                             size = 12,
                             line = list(width=2)))
  p <- add_trace(p,
                 x = ~stack1$value,
                 y = ~stack1$row,
                 color = ~stack1$group,
                 text = stack1$col,
                 hoverinfo = 'text') %>%
    plotly::layout(xaxis = list(title = 'Log2 CPM'),
                   title = "Barcode",
           yaxis = list(title = '',
                        categoryorder = "array",
                        autorange = "reversed")) %>%
    config(
      toImageButtonOptions = list(
        format = "svg",
        filename = "barcode",
        width = 750,
        height = 500
      )
    )
  p
}

pValuePlot <- function(deTab){
  pvalue <- round(deTab$P.Value, digits = 2)
  pvalue <- aggregate(pvalue, by=list(p=pvalue), FUN=length)
  
  p <- plot_ly(pvalue,
               x = ~p,
               y = ~x,
               type = "bar") %>%
    plotly::layout(xaxis = list(title = 'P-Value'),
                   title = "P-Value",
           yaxis = list(title = 'Count')) %>%
    config(
      toImageButtonOptions = list(
        format = "svg",
        filename = "pvalue",
        width = 750,
        height = 500
      )
    )
  p
}

## --------------------------------------------------------------------------

## ----- BIAS PLOT -----

biasPlot <- function(deTab, biasColumn, log) {
  if (is.null(biasColumn)) {
    return(NULL)
  }
  prediction <- gamConfidenceFit(deTab, biasColumn)
  
  p <- plot_ly(deTab,
               x = ~get(biasColumn),
               y = ~avgLog2FC,
               type = 'scattergl',
               mode = "markers",
               color = ~adj.P.Val,
               alpha = 0.75,
               showlegend=FALSE,
               text = rownames(deTab),
               hoverinfo = 'text') %>%
    add_trace(data = prediction,
              mode="lines",
              x = ~get(biasColumn),
              y = ~.fitted,
              text = NA,
              key = NA,
              color = "green",
              line = list(color = 'rgba(7, 164, 181, 1)'),
              name = "Fitted") %>%
    add_ribbons(data = prediction,
                ymin = ~.fitted - 1.96 * .se.fit,
                ymax = ~.fitted + 1.96 * .se.fit,
                fillcolor = "rgba(7, 164, 181, 0.25)",
                text = NA,
                key = NA,
                color = "green",
                line = list(color = 'rgba(0, 0, 0, 0)'),
                name = "Standard Error") %>%
    plotly::layout(xaxis = list(title = biasColumn, type = log), #, type = "log"),
                   title = paste("Bias based on", biasColumn),
           yaxis = list(title = 'Average Log2 FC')) %>%
    config(
      toImageButtonOptions = list(
        format = "svg",
        filename = "bias",
        width = 750,
        height = 500
      )
    )
  p
}

## --------------------------------------------------------------------------

## ----- GENE SET ENRICHMENT PLOTS -----


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
        format = "svg",
        filename = "enrichbar",
        width = 750,
        height = 500
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
        format = "svg",
        filename = "enrichnet",
        width = 750,
        height = 500
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

## ----- MARKDOWN FUNCTIONS -----

reOrderDesign <- function(matrix_value, design_value, data_samples) {
  check_matrix <- strsplit(matrix_value, " - ", fixed = TRUE)[[1]]
  check_design <- strsplit(design_value, " + ", fixed = TRUE)[[1]]
  
  get_column <- which(check_matrix[1] == data_samples, arr.ind=TRUE)[,2][[1]]
  column <- colnames(data_samples[get_column])
  remove_design_items <- c("~0", column)
  new_design <- setdiff(check_design, remove_design_items)
  
  if (length(new_design) != 0) {
    new_design <- paste("~0 +", column, "+", gsub(",", " +", toString(c(new_design))))
  } else {
    new_design <- paste("~0 +", column)
  }
  new_design
}

highExpressedFeatures <- function(method, dge, design_value, cpm_value) {
  if (method == "edger") {
    edger <- calcNormFactors( dge, method = "TMM")
    counts <- cpm(edger, log = TRUE)
    selectedFeatures <- rownames( edger )[ apply( counts, 1, function( v ) sum( v >= cpm_value ) ) >= 1/4 * ncol( counts ) ]
    
  } else {
    selectedFeatures <- filterByExpr(dge, model.matrix(eval(parse(text=design_value)), dge$samples ))
    
  }
  selectedFeatures
}

## NOT IN USE ANYMORE
filterDge <- function(normDge, excluded_samples, data_samples, se) {
  normDge$counts <- normDge$counts[,!colnames(normDge$counts) %in% excluded_samples]
  data_samples <- data_samples[!rownames(data_samples) %in% excluded_samples, ]
  data_samples <- droplevels(data_samples)
  
  se <- addSamplesFromTableToSE(se, data_samples)
  
  tempDge <- DGEList(counts = normDge$counts, samples = colData(se), genes = normDge$genes)
  tempDge <- calcNormFactors( tempDge, method = "TMM")
  tempDge
}

reNameDesign <- function(design, normDge) {
  for (sample_column in rev(colnames(normDge$samples))) {
    if (startsWith(colnames(design)[1], sample_column)) {
      colnames(design) <- sub(sample_column, "", colnames(design))
    }
  }
  design
}

## --------------------------------------------------------------------------

## ----- POSSIBLE LATER USE -----

# Gene id vs symbols
#if (setGeneName == "symbol") {
#  tempCol <- rownames(deTab)
#  rownames(deTab,make.names) <- deTab$geneName
#  deTab$geneName <- tempCol
#  colnames(deTab)[1] <- "geneId"
#  rownames(normDge$counts) <- normDge$genes$geneName
#}

vennDiagram <- function(){
  p <- plot_ly(x = c(0.2, 0.75, 1.3),
               y = c(0.75, 0.75, 0.75),
               type='scattergl',
               text = c('A', 'A+B', 'B'),
               mode = 'text',
               textfont = list(size = 18)
  ) %>%
    plotly::layout(xaxis = list(title = '',
                        zeroline = FALSE,
                        showline = FALSE,
                        showticklabels = FALSE,
                        showgrid = FALSE),
           yaxis = list(title = '',
                        zeroline = FALSE,
                        showline = FALSE,
                        showticklabels = FALSE,
                        showgrid = FALSE),
           shapes = list(
             list(
               x0 = 0,
               x1 = 1.1,
               y0 = 0,
               y1 = 1.5,
               line = list(color = "#1f77b4"),
               type = "circle",
               xref = "x",
               yref = "y",
               opacity = 0.5,
               layer="below",
               fill = 'tonextt',
               fillcolor = "#1f77b4"
             ), 
             list(
               x0 = 0.4,
               x1 = 1.5,
               y0 = 0,
               y1 = 1.5,
               line = list(color = "#2ca02c"),
               type = "circle",
               xref = "x",
               yref = "y",
               opacity = 0.5,
               layer="below",
               fillcolor = "#2ca02c"
             )
           ))
  p
}

## --------------------------------------------------------------------------
