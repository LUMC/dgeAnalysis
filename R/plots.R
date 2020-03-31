
## ----- ALIGNMENT PLOTS -----


## alignmentSummaryPlot()
##  Calculates Log values in the function alignmentSummary()
##  If percentages == TRUE percentages are calculated
##  A stacked bar plot is created based on sample read counts
## Parameters:
##  se = SummerizedExperiment object, containing samples and counts
##  perc = Boolean, Data in percentages (default=TRUE)
## Returns:
##  p = Plotly object

alignmentSummaryPlot <- function(se, perc=T){
  lse <- alignmentSummary(se)
  lse$feature <- gsub("_", " ", gsub("__", "", lse$feature))
  
  if (perc) {
    for (var in unique(lse$sample)) {
      temp <- lse[lse$sample == var, ]
      lse$count[lse$sample == var] <- temp$count/(sum(temp$count))
    }
    
    p <- plot_ly(
      data = lse,
      x = ~count,
      y = ~sample,
      orientation = 'h',
      color = ~feature,
      type = "bar",
      text = ~paste(feature, round(count*100, 2), '%\n'),
      hoverinfo = 'text') %>% 
      plotly::layout(
        title = "Count assignments %",
        xaxis = list(title = 'Counts', tickformat = "%"),
        yaxis = list(title = ''),
        barmode = 'stack') %>%
      config(
        toImageButtonOptions = list(
          format = "png",
          filename = "alignment_perc",
          width = 1500,
          height = 1000
        )
      )
  } else {
    p <- plot_ly(
      data = lse,
      x = ~count,
      y = ~sample,
      orientation = 'h',
      color = ~feature,
      type = "bar",
      text = ~paste(feature, formatC(count, format="f", big.mark=".", digits=0), 'Reads\n'),
      hoverinfo = 'text') %>% 
      plotly::layout(
        title = "Count assignments",
        xaxis = list(title = 'Counts'),
        yaxis = list(title = ''),
        barmode = 'stack') %>%
      config(
        toImageButtonOptions = list(
          format = "png",
          filename = "alignment",
          width = 1500,
          height = 1000
        )
      )
  }
  p
}


## complexityPlot()
##  Calculates count data per rank in the function complexityData()
##  If percentages == TRUE percentages are calculated
##  A dot line plot is created based on the number of reads at rank
## Parameters:
##  se = SummerizedExperiment object, containing samples and counts
##  perc = Boolean, Data in percentages (default=TRUE)
##  rank = The number of genes/rank (min=10)
## Returns:
##  p = Plotly object

complexityPlot <- function(se, perc, rank) {
  compData <- complexityData(se, rank)
  
  if (perc){
    p <- plot_ly(
      data = compData,
      x = ~rank,
      y = ~fraction,
      color = ~sample,
      type = "scattergl",
      mode = "lines+markers",
      text = ~paste(rank, "Genes\n", round(fraction*100, 2), '% Reads\n'),
      hoverinfo = 'text') %>%
      plotly::layout(
        title = "Gene complexity %",
        xaxis = list(title = 'Rank', type="log"),
        yaxis = list(tickformat = "%", title = 'Cumulative fraction of total reads till rank')) %>%
      config(
        toImageButtonOptions = list(
          format = "png",
          filename = "complexity_perc",
          width = 1500,
          height = 1000
        )
      )
  } else {
    p <- plot_ly(
      data = compData,
      x = ~rank,
      y = ~value,
      color = ~sample,
      type = "scattergl",
      mode = "lines+markers",
      text = ~paste(rank, "Genes\n", formatC(value, format="f", big.mark=".", digits=0), 'Reads\n'),
      hoverinfo = 'text') %>%
      plotly::layout(
        title = "Gene complexity",
        xaxis = list(title = 'Rank', type="log"),
        yaxis = list(title = 'Cumulative reads till rank')) %>%
      config(
        toImageButtonOptions = list(
          format = "png",
          filename = "complexity",
          width = 1500,
          height = 1000
        )
      )
  }
  p
}

## --------------------------------------------------------------------------

## ----- RAW DATA PLOTS / NORMALIZATION PLOTS -----


## countDistributionLinePlot()
##  Stacks count data from dge list in the function stackDge()
##  A line plot is created based on Log2CPM and the density of the counts
## Parameters:
##  dge = DGE list object, containing samples and counts
## Returns:
##  p = Plotly object

countDistributionLinePlot <- function(dge){
  stackCounts <- data.frame(stackDge(dge))
  
  p <- plot_ly(
    type = 'scattergl',
    mode = 'lines',
    source = "dist_line") %>%
    plotly::layout(
      title = "Gene count distribution",
      xaxis = list(title = 'Log2CPM'),
      yaxis = list(title = 'Density')) %>%
    config(
      toImageButtonOptions = list(
        format = "png",
        filename = "countdistline",
        width = 1500,
        height = 1000
      )
    )
  for (var in unique(stackCounts$sample)) {
    temp <- stackCounts[stackCounts$sample == var, ]
    density <- density(temp$logCPM)
    p <- add_trace(
      p,
      x = density$x,
      y = density$y,
      name = var,
      text = var,
      hoverinfo = 'text',
      fill = 'tozeroy',
      alpha = 0.05)
  }
  p
}


## countDistributionBoxPlot()
##  Stacks count data from dge list in the function stackDge()
##  A box plot is created based on Log2CPM counts of the samples
## Parameters:
##  dge = DGE list object, containing samples and counts
## Returns:
##  p = Plotly object

countDistributionBoxPlot <- function(dge){
  stackCounts <- data.frame(stackDge(dge))
  
  p <- plot_ly(
    type = 'box',
    boxpoints = FALSE) %>%
    config(
      toImageButtonOptions = list(
        format = "png",
        filename = "countdistbox",
        width = 1500,
        height = 1000
      )
    )
  for (var in unique(stackCounts$sample)) {
    temp <- stackCounts[stackCounts$sample == var, ]
    p <- add_trace(
      p,
      y = temp$logCPM,
      name = var,
      alpha = 0.5) %>%
      plotly::layout(
        title = "Gene count distribution",
        xaxis = list(title = ''),
        yaxis = list(title = 'Log2CPM'))
  }
  p
}


## voomPlot()
##  Calculates required values with 'voom' method
##  The plot is created with plotly with values retrieved from the voom object
## Parameters:
##  dge = DGE list object, containing samples and counts
##  sourceId = plot ID, depends on raw/normalized counts
## Returns:
##  p = Plotly object

voomPlot <- function(dge, sourceId){
  v <- voom(2^(dge$counts), save.plot = TRUE)
  
  p <- plot_ly(
    x = ~v$voom.xy$x,
    y = ~v$voom.xy$y,
    type = "scattergl",
    mode = "markers",
    color = "Voom",
    alpha = 0.75,
    text = names(v$voom.xy$x),
    hoverinfo = 'text',
    key = ~names(v$voom.xy$x),
    source=sourceId) %>%
    add_trace(
      mode = "lines",
      x = v$voom.line$x,
      y = v$voom.line$y,
      hoverinfo = 'none',
      line = list(color = "rgba(7, 164, 181, 1)"),
      name = "Voom average") %>%
    plotly::layout(
      title = "Voom",
      xaxis = list(title = 'Average Log2 Count'),
      yaxis = list(title = 'SQRT(Standard Deviation)'),
      clickmode = "event+select",
      dragmode = "select") %>%
    config(
      toImageButtonOptions = list(
        format = "png",
        filename = sourceId,
        width = 1500,
        height = 1000
      )
    )
  p
}


## multidimensionalScaling2dPlot()
##  Calculates required values with 'plotMDS' method
##  The plot is created with plotly with values retrieved from the mds object
##  Plot is colored based on the selected column
## Parameters:
##  dge = DGE list object, containing samples and counts
##  color = String, Column on wich colors should be based
##  sourceId = plot ID, depends on raw/normalized counts
## Returns:
##  p = Plotly object

multidimensionalScaling2dPlot <- function(dge, color, sourceId){
  logFC <- plotMDS(dge$counts, ndim = ncol(dge)-1)
  for_plots <- data.frame(logFC$cmdscale.out)
  for_plots$group <- dge$samples[,color]
  
  p <- plot_ly(
    data = for_plots,
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
    source=sourceId) %>%
    plotly::layout(
      title = paste("MDS Plot 2D"),
      xaxis = list(title = 'MDS1'),
      yaxis = list(title = 'MDS2'),
      clickmode = "event+select",
      dragmode = "select") %>%
    config(
      toImageButtonOptions = list(
        format = "png",
        filename = sourceId,
        width = 150,
        height = 1000
      )
    )
  p
}


## multidimensionalScaling3dPlot()
##  Calculates required values with 'plotMDS' method
##  The plot is created with plotly with values retrieved from the mds object
##  Plot is colored based on the selected column
## Parameters:
##  dge = DGE list object, containing samples and counts
##  color = String, Column on wich colors should be based
## Returns:
##  p = Plotly object

multidimensionalScaling3dPlot <- function(dge, color){
  logFC <- plotMDS(dge$counts, ndim = ncol(dge)-1)
  for_plots <- data.frame(logFC$cmdscale.out)
  for_plots$group <- dge$samples[,color]
  
  p <- plot_ly(
    data = for_plots,
    x = ~X1,
    y = ~X2,
    z = ~X3,
    color = ~for_plots$group,
    text = rownames(for_plots),
    hoverinfo = 'text') %>%
    add_markers(marker = list(size=5, opacity = 0.75)) %>%
    plotly::layout(
      title = paste("MDS Plot 3D"),
      scene = list(
        xaxis = list(title = 'MDS1'),
        yaxis = list(title = 'MDS2'),
        zaxis = list(title = 'MDS3'))
    ) %>%
    config(
      toImageButtonOptions = list(
        format = "png",
        filename = "mds3d",
        width = 1500,
        height = 1000
      )
    )
  p
}

## --------------------------------------------------------------------------

## ----- PCA PLOTS -----


## variancePcaPlot()
##  Columns and rows from DGE list are turned
##  PCA is calculated with prcomp
##  PC percentages are calulated
##  Barplot is created with PC percentages
## Parameters:
##  dge = DGE list object, containing samples and counts
## Returns:
##  p = Plotly object

variancePcaPlot <- function(dge){
  tdge <- t(dge$counts)
  tdge[!is.finite(tdge)] <- 0
  pca <- prcomp(tdge, center=TRUE)
  percent <- data.frame(summary( pca )$importance[2,])
  colnames(percent) <- "percent"
  
  p <- plot_ly(
    data = percent,
    x = rownames(percent),
    y = ~percent,
    type = "bar") %>% 
    plotly::layout(
      title = "PCA Scree",
      xaxis = list(title = 'Component', categoryorder='trace'),
      yaxis = list(title = 'Percentage', tickformat = ".2%")) %>%
    config(
      toImageButtonOptions = list(
        format = "png",
        filename = "pcascree",
        width = 1500,
        height = 1000
      )
    )
  p
}


## samplePca2dPlot()
##  Columns and rows from DGE list are turned
##  PCA is calculated with prcomp
##  PC percentages are calulated
##  Scatter plot is created based on selected PCs
## Parameters:
##  dge = DGE list object, containing samples and counts
##  color = String, Column on wich colors should be based
##  getPC1 = String, Selected PC to be plotted on x-axis
##  getPC2 = String, Selected PC to be plotted on y-axis
## Returns:
##  p = Plotly object

samplePca2dPlot <- function(dge, color, getPC1, getPC2){
  tdge <- t(dge$counts)
  tdge[!is.finite(tdge)] <- 0
  pca <- prcomp(tdge, center=TRUE)
  percent <- data.frame(summary( pca )$importance[2,])
  colnames(percent) <- "percent"
  
  pca <- data.frame(scale(tdge, center=T, scale=F)%*%pca$rotation)
  pca$group <- dge$samples[,color]
  
  p <- plot_ly(
    data = pca,
    x = pca[[getPC1]],
    y = pca[[getPC2]], 
    type = "scattergl",
    mode = "markers",
    color=~pca$group,
    text = rownames(pca),
    hoverinfo = 'text',
    marker = list(size=15,
                  line = list(color = '#999999',
                              width = 1)),
    key = ~rownames(pca),
    source = "pca_pca2d") %>%
    plotly::layout(
      title = 'PCA 2D',
      xaxis = list(title = paste0(getPC1, " (", round(percent[getPC1,]*100, 2), "%)")),
      yaxis = list(title = paste0(getPC2, " (", round(percent[getPC2,]*100, 2), "%)")),
      clickmode = "event+select",
      dragmode = "select") %>%
    config(
      toImageButtonOptions = list(
        format = "png",
        filename = "pca2d",
        width = 1500,
        height = 1000
      )
    )
  p
}


## samplePca3dPlot()
##  Columns and rows from DGE list are turned
##  PCA is calculated with prcomp
##  PC percentages are calulated
##  Scatter plot is created based on selected PCs
## Parameters:
##  dge = DGE list object, containing samples and counts
##  color = String, Column on wich colors should be based
##  getPC1 = String, Selected PC to be plotted on x-axis
##  getPC2 = String, Selected PC to be plotted on y-axis
##  getPC3 = String, Selected PC to be plotted on z-axis
## Returns:
##  p = Plotly object

samplePca3dPlot <- function(dge, color, getPC1, getPC2, getPC3){
  tdge <- t(dge$counts)
  tdge[!is.finite(tdge)] <- 0
  pca <- prcomp(tdge, center=TRUE)
  percent <- data.frame(summary( pca )$importance[2,])
  colnames(percent) <- "percent"
  
  pca <- data.frame(scale(tdge, center=T, scale=F)%*%pca$rotation)
  pca$group <- dge$samples[,color]
  
  p <- plot_ly(
    data = pca,
    x = pca[[getPC1]],
    y = pca[[getPC2]],
    z = pca[[getPC3]],
    color = ~pca$group,
    text = rownames(pca),
    hoverinfo = 'text') %>%
    add_markers(marker = list(size=5, opacity = 0.75)) %>%
    plotly::layout(
      title = 'PCA 3D',
      scene = list(
        xaxis = list(title = paste0(getPC1, " (", round(percent[getPC1,]*100, 2), "%)")),
        yaxis = list(title = paste0(getPC2, " (", round(percent[getPC2,]*100, 2), "%)")),
        zaxis = list(title = paste0(getPC3, " (", round(percent[getPC3,]*100, 2), "%)")))
    ) %>%
    config(
      toImageButtonOptions = list(
        format = "png",
        filename = "pca3d",
        width = 1500,
        height = 1000
      )
    )
  p
}

## --------------------------------------------------------------------------

## ----- HEATMAPS PLOTS -----


## variableHeatmapPlot()
##  LogCPM values of counts are calculated
##  Variance is calculated and the first x genes are kept
##  Heatmap with the values left in high_var_cpm
## Parameters:
##  dge = DGE list object, containing samples and counts
##  amount = Integer, The number of genes shown in plot
## Returns:
##  p = Plotly object

variableHeatmapPlot <- function(dge, amount){
  lcpm <- dge$counts
  var_genes <- apply(lcpm, 1, var)
  select_var <- names(sort(var_genes, decreasing=TRUE))[1:amount]
  high_var_cpm <- lcpm[select_var,]
  
  p <- plot_ly(
    x = ~colnames(high_var_cpm),
    y = ~rownames(high_var_cpm),
    z = ~high_var_cpm,
    colorbar = list(title = "Log2CPM", len=1),
    type = "heatmap") %>%
    plotly::layout(
      title = "Most variable genes",
      xaxis = list(title = ''),
      yaxis = list(
        title = '',
        categoryorder = "array",
        autorange = "reversed")) %>%
    config(
      toImageButtonOptions = list(
        format = "png",
        filename = "heatmapvar",
        width = 1500,
        height = 1000
      )
    )
  p
}


## topDgeHeatmapPlot()
##  The DE table is sorted on FDR/adjPvalue
##  The first x genes are kept
##  Normalized values are extracted based on the DE genes still present
##  Heatmap with the normalized values of genes is created
## Parameters:
##  deTab = Dataframe, with all analysis results
##  dge = DGE list object, containing samples and counts
##  amount = Integer, The number of genes shown in plot
## Returns:
##  p = Plotly object

topDgeHeatmapPlot <- function(deTab, dge, amount){
  sortdeTab <- deTab[order(rank(deTab$adj.P.Val)),]
  sortdeTab <- head(sortdeTab, amount)
  getnorm <- dge[rownames(sortdeTab),]
  getnorm <- getnorm$counts
  
  p <- plot_ly(
    x = ~colnames(getnorm),
    y = ~rownames(getnorm),
    z = ~getnorm,
    colorbar = list(title = "Log2CPM", len=1),
    type = "heatmap") %>%
    plotly::layout(
      title = "Most expressed genes",
      xaxis = list(title = ''),
      yaxis = list(
        title = '',
        categoryorder = "array",
        autorange = "reversed")) %>%
    config(
      toImageButtonOptions = list(
        format = "png",
        filename = "heatmapdge",
        width = 1500,
        height = 1000
      )
    )
  p
}

## --------------------------------------------------------------------------

## ----- ANALYSIS PLOTS -----


## deRatioPlot()
##  The number of unique values in column 'DE' are extracted
##  Percentages of these values are calculated and renamed
##  Barplot is created with DE results based on percentages
## Parameters:
##  deTab = Dataframe, with all analysis results
## Returns:
##  p = Plotly object

deRatioPlot <- function(deTab){
  defeatures <- aggregate(deTab$DE, by=list(category=deTab$DE), FUN=length)
  defeatures$perc <- defeatures[,2]/sum(defeatures[,2])
  
  defeatures$category[which(defeatures$category == 0)] <- "Not sign"
  defeatures$category[which(defeatures$category == -1)] <- "Down"
  defeatures$category[which(defeatures$category == 1)] <- "Up"
  
  p <- plot_ly(
    data = defeatures,
    x = "",
    y = ~perc,
    color = ~category,
    type = "bar",
    text = paste(defeatures[,2], "Genes\n", round(defeatures$perc*100, 2), "%"),
    textposition = "auto",
    textfont= list(color="black"),
    hovertext = ~paste(category, "expressed"),
    hoverinfo = 'text') %>%
    plotly::layout(
      title = "Differential expression ratio",
      xaxis = list(title = ""),
      yaxis = list(tickformat = "%", title = "Ratio")) %>%
    config(
      toImageButtonOptions = list(
        format = "png",
        filename = "dgeratio",
        width = 1500,
        height = 1000
      )
    )
  p
}


## ma_plot()
##  The confidence prediction calculated in the function gamConfidenceFit()
##  Confidence is calculated based on avgLog2CPM
##  Scatterplot is created with DE results and a line showing confidence
## Parameters:
##  deTab = Dataframe, with all analysis results
## Returns:
##  p = Plotly object

ma_plot <- function(deTab){
  prediction <- gamConfidenceFit(deTab, "avgLog2CPM")
  
  p <- plot_ly(
    data = deTab[deTab$DE == 0,],
    x = ~avgLog2CPM,
    y = ~avgLog2FC,
    type = "scattergl",
    mode = "markers",
    color = ~as.character(DE),
    alpha = 0.75,
    text = rownames(deTab[deTab$DE == 0,]),
    hoverinfo = 'text',
    key = ~rownames(deTab[deTab$DE == 0,]),
    source = "analysis_ma") %>%
    add_trace(
      x = ~deTab[deTab$DE != 0,]$avgLog2CPM,
      y = ~deTab[deTab$DE != 0,]$avgLog2FC,
      color = as.character(deTab[deTab$DE != 0,]$DE),
      alpha = 0.75,
      text = rownames(deTab[deTab$DE != 0,]),
      hoverinfo = 'text',
      key = ~rownames(deTab[deTab$DE != 0,])) %>%
    add_trace(
      data = prediction,
      mode = "lines",
      x = ~avgLog2CPM,
      y = ~.fitted,
      text = NA,
      key = NA,
      color = "Fitted",
      line = list(color = 'rgba(7, 164, 181, 1)'),
      name = "Fitted") %>%
    add_ribbons(
      data = prediction,
      ymin = ~.fitted - 1.96 * .se.fit,
      ymax = ~.fitted + 1.96 * .se.fit,
      fillcolor = "rgba(7, 164, 181, 0.25)",
      text = NA,
      key = NA,
      color = "Standard Error",
      line = list(color = 'rgba(0, 0, 0, 0)'),
      name = "Standard Error") %>%
    plotly::layout(
      title = "MA",
      xaxis = list(title = 'Average Log2 CPM'),
      yaxis = list(title = 'Average Log2 FC'),
      clickmode = "event+select",
      dragmode = "select") %>%
    config(
      toImageButtonOptions = list(
        format = "png",
        filename = "ma",
        width = 1500,
        height = 1000
      )
    )
  p
}


## volcanoPlot()
##  Creates scatter plot with avgLog2FC vs -log10(adj.P.Val)
##  Two lines are generated indicating LogFC cutoff and p value cutoff
## Parameters:
##  deTab = Dataframe, with all analysis results
##  LogCut = Integer, LogFC cutoff for line
##  PCut = Intege, Pvalue cutoff for line
## Returns:
##  p = Plotly object

volcanoPlot <- function(deTab, LogCut, PCut){
  p <- plot_ly(
    data = deTab[deTab$DE == 0,],
    x = ~avgLog2FC,
    y = ~-log10(adj.P.Val),
    type = "scattergl",
    mode = "markers",
    color = ~as.character(DE),
    alpha = 0.75,
    text = rownames(deTab[deTab$DE == 0,]),
    hoverinfo = 'text',
    key = ~rownames(deTab[deTab$DE == 0,]),
    source = "analysis_volcano") %>%
    add_trace(
      x = ~deTab[deTab$DE != 0,]$avgLog2FC,
      y = ~-log10(deTab[deTab$DE != 0,]$adj.P.Val),
      color = as.character(deTab[deTab$DE != 0,]$DE),
      alpha = 0.75,
      text = rownames(deTab[deTab$DE != 0,]),
      hoverinfo = 'text',
      key = ~rownames(deTab[deTab$DE != 0,])) %>%
    plotly::layout(
      title = "Volcano",
      xaxis = list(title = 'Average Log2 FC'),
      yaxis = list(title = '- Log10 P-Value'),
      shapes = list(
        list(type = "line",  line = list(color = "red"),
             x0 = 0,  x1 = 1, xref = "paper",
             y0 = PCut, y1 = PCut),
        
        list(type = "line",  line = list(color = "red"),
             x0 = LogCut,  x1 = LogCut,
             y0 = 0, y1 = 1, yref = "paper"),
        list(type = "line",  line = list(color = "red"),
             x0 = -LogCut,  x1 = -LogCut,
             y0 = 0, y1 = 1, yref = "paper")
      ),
      clickmode = "event+select",
      dragmode = "select"
    ) %>%
    config(
      toImageButtonOptions = list(
        format = "png",
        filename = "volcano",
        width = 1500,
        height = 1000
      )
    )
  p
}


## barcodePlot()
##  The DE table is sorted on FDR/adjPvalue
##  The first x genes are kept
##  Normalized values are extracted based on the DE genes still present
##  normalized values are stacked
##  Scatter plot is created with LogCPM values per gene per sample
## Parameters:
##  deTab = Dataframe, with all analysis results
##  dge = DGE list object, containing samples and counts
##  color = String, Column on wich colors should be based
##  amount = Integer, The number of genes shown in plot
##  selected = Vector, Extra selected rownames
## Returns:
##  p = Plotly object

barcodePlot <- function(deTab, dge, color, amount, selected) {
  if (is.null(color)) {
    return(NULL)
  }
  
  sortdeTab <- deTab[order(rank(deTab$adj.P.Val)),]
  sortdeTab <- head(sortdeTab, amount)
  getnorm <- dge[c(rownames(sortdeTab), selected),]
  getnorm$counts <- getnorm$counts
  stack1 <- as.data.frame(stack(getnorm$counts))
  stack1$group <- getnorm$samples[[color]][stack1$col]
  
  p <- plot_ly(
    type = "scattergl",
    mode = "markers",
    marker = list(symbol = "line-ns-open",
                  size = 250/(amount+length(selected)),
                  line = list(width=2)))
  p <- add_trace(
    p,
    x = ~stack1$value,
    y = ~stack1$row,
    color = ~stack1$group,
    text = stack1$col,
    hoverinfo = 'text') %>%
    plotly::layout(
      title = "Barcode",
      xaxis = list(title = 'Log2 CPM'),
      yaxis = list(title = '',
                   categoryorder = "array",
                   autorange = "reversed"),
      autosize = T) %>%
    config(
      toImageButtonOptions = list(
        format = "png",
        filename = "barcode",
        width = 1500,
        height = 1000
      )
    )
  p
}


## pValuePlot()
##  The Pvalues are rounded on two decimals
##  All occurences of pvalues are counted
##  Bar plot is created with the p value vs occurence
## Parameters:
##  deTab = Dataframe, with all analysis results
## Returns:
##  p = Plotly object

pValuePlot <- function(deTab){
  pvalue <- round(deTab$P.Value, digits=2)
  pvalue <- aggregate(pvalue, by=list(p=pvalue), FUN=length)
  
  p <- plot_ly(
    data = pvalue,
    x = ~p,
    y = ~x,
    type = "bar") %>%
    plotly::layout(
      title = "P-Value",
      xaxis = list(title = 'P-Value'),
      yaxis = list(title = 'Count')) %>%
    config(
      toImageButtonOptions = list(
        format = "png",
        filename = "pvalue",
        width = 1500,
        height = 1000
      )
    )
  p
}

## --------------------------------------------------------------------------

## ----- BIAS PLOT -----


## biasPlot()
##  The confidence prediction calculated in the function gamConfidenceFit()
##  Confidence is calculated based on the GC or length
##  Scatterplot is created with avgLog2FC and corresponding bias value
## Parameters:
##  deTab = Dataframe, with all analysis results
##  biasColumn = String, Column indicating bias values (GC or length)
##  log = Boolean, Show plot in Log scale
## Returns:
##  p = Plotly object

biasPlot <- function(deTab, biasColumn, log) {
  if (is.null(biasColumn)) {
    return(NULL)
  }
  prediction <- gamConfidenceFit(deTab, biasColumn)
  
  p <- plot_ly(
    data = deTab,
    x = ~get(biasColumn),
    y = ~avgLog2FC,
    type = 'scattergl',
    mode = "markers",
    color = ~adj.P.Val,
    alpha = 0.75,
    showlegend = FALSE,
    text = rownames(deTab),
    hoverinfo = 'text') %>%
    add_trace(
      data = prediction,
      mode = "lines",
      x = ~get(biasColumn),
      y = ~.fitted,
      text = NA,
      key = NA,
      color = "green",
      line = list(color = 'rgba(7, 164, 181, 1)'),
      name = "Fitted") %>%
    add_ribbons(
      data = prediction,
      ymin = ~.fitted - 1.96 * .se.fit,
      ymax = ~.fitted + 1.96 * .se.fit,
      fillcolor = "rgba(7, 164, 181, 0.25)",
      text = NA,
      key = NA,
      color = "green",
      line = list(color = 'rgba(0, 0, 0, 0)'),
      name = "Standard Error") %>%
    plotly::layout(
      title = paste("Bias based on", biasColumn),
      xaxis = list(title = biasColumn, type = log), #, type = "log"),
      yaxis = list(title = 'Average Log2 FC')) %>%
    config(
      toImageButtonOptions = list(
        format = "png",
        filename = "bias",
        width = 1500,
        height = 1000
      )
    )
  p
}

## --------------------------------------------------------------------------

## ----- INFORMATION BOX -----


## informationBox()
##  Template for plot information
## Parameters:
##  infoText = String, Explanation of a plot
## Returns:
##  Shiny Box object

informationBox <- function(infoText) {
  tryCatch({
    box(
      title = icon("info-circle"),
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      collapsed = FALSE,
      span(
        infoText,
        style = "padding-left: 5px; text-align: justify; display: block;"
      ),
      style = "padding-left: unset;"
    )
  }, error = function(err) {
    return(NULL)
  })
}

## --------------------------------------------------------------------------
