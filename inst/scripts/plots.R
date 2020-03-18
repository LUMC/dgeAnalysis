
## ----- ALIGNMENT PLOTS -----

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
      title = paste("MDS Plot, Grouped By:", color),
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
      title = paste("MDS Plot, Grouped By:", color),
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
        format = "pngg",
        filename = "pca2d",
        width = 1500,
        height = 1000
      )
    )
  p
}

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

volcanoPlot <- function(deTab, LogCut, PCut, ps){
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

barcodePlot <- function(deTab, dge, color) {
  if (is.null(color)) {
    return(NULL)
  }
  
  sortdeTab <- deTab[order(rank(deTab$adj.P.Val)),]
  sortdeTab <- head(sortdeTab, 25)
  getnorm <- dge[rownames(sortdeTab),]
  getnorm$counts <- getnorm$counts
  stack1 <- as.data.frame(stack(getnorm$counts))
  stack1$group <- getnorm$samples[[color]][stack1$col]
  
  p <- plot_ly(
    type = "scattergl",
    mode = "markers",
    marker = list(symbol = "line-ns-open",
                  size = 12,
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
                   autorange = "reversed")) %>%
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

informationBox <- function(infoText) {
  tryCatch({
    box(
      title = "Information",
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
