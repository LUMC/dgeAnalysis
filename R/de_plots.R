
## ----- ALIGNMENT PLOTS -----


#' Calculates Log values in the function alignmentSummary().
#' If percentages == TRUE percentages are calculated.
#' A stacked bar plot is created based on sample read counts.
#'
#' @param se SummerizedExperiment object, containing samples and counts
#' @param sort_value String, Sort samples based on a group
#' @param perc Boolean, Data in percentages
#'
#' @return p, (Plotly object) plot
#'
#' @export

profvis({
alignmentSummaryPlot <- function(se, sort_value = "None", perc = T) {
  lse <- alignmentSummary(se)
  lse$feature <- gsub("_", " ", gsub("__", "", lse$feature))
  if (sort_value != "None") {
    for (sample in lse$sample) {
      lse$order[lse$sample == sample] <- se[[sort_value]][colnames(se) == sample]
    }
    lse <- lse[order(lse$order, lse$sample, method = "radix"), ]
    lse$sample <- factor(lse$sample, levels = unique(lse$sample))
  } else {
    lse$order <- "Sample"
  }
  
  if (perc) {
    for (var in unique(lse$sample)) {
      temp <- lse[lse$sample == var,]
      lse$count[lse$sample == var] <- temp$count / (sum(temp$count))
    }
  }
  
  plot_list <- c()
  for (order in unique(lse$order)) {
    temp <- lse[lse$order == order, ]
    p <- plot_ly(
      data = temp,
      x = ~ count,
      y = ~ sample,
      color = ~ feature,
      orientation = 'h',
      alignmentgroup = ~ order,
      legendgroup = ~ feature,
      type = "bar",
      showlegend = if (order == unique(lse$order)[1]) {
        TRUE
      } else{
        FALSE
      },
      text = if (perc) {
        ~ paste(sample,
                '\n', round(count * 100, 2),
                '%', feature)
      } else {
        ~ paste(
          sample,
          '\n',
          formatC(
            count,
            format = "f",
            big.mark = ".",
            digits = 0
          ),
          'Reads',
          feature
        )
      },
      hoverinfo = 'text'
    ) %>%
      group_by(order) %>%
      plotly::layout(
        barmode = 'stack',
        title = "Count assignments",
        xaxis = if (perc) {
          list(title = 'Counts',
               tickformat = "%")
        } else {
          list(title = 'Counts')
        },
        yaxis = if (sort_value != "None") {
          list(
            title = "",
            tickmode = "array",
            tickvals = length(unique(temp$sample)) / 2 - 0.5,
            ticktext = ~ paste(order, " ")
          )
        } else {
          list(title = "")
        },
        legend = list(tracegroupgap = 0)
      ) %>%
      config(
        toImageButtonOptions = list(
          format = "png",
          filename = "countdistline",
          width = 1500,
          height = 1000
        )
      )
    plot_list[[paste0("plot", order)]] <- p
  }
  plotly::subplot(
    plot_list,
    nrows = length(unique(lse$order)),
    shareY = TRUE,
    shareX = TRUE,
    margin = 0.005
  )
}


#' Calculates count data per rank in the function complexityData().
#' If percentages == TRUE percentages are calculated.
#' A dot line plot is created based on the number of reads at rank.
#'
#' @param se SummerizedExperiment object, containing samples and counts
#' @param color String, Sort samples based on a group
#' @param perc Boolean, Data in percentages
#' @param rank Integer, The number of genes/rank (min=10)
#'
#' @return p, (Plotly object) plot
#'
#' @export

complexityPlot <- function(se, color = "None", perc = T, rank = 1000) {
  if (color == "None") {color <- NA}
  
  compData <- complexityData(se, rank)
  compData <- merge(
    x = compData,
    y = as.data.frame(colData(se)),
    by.x = "sample",
    by.y = "row.names",
    all.x = TRUE
  )
  
  p <- plot_ly(type = 'scattergl',
               mode = "lines+markers") %>%
    plotly::layout(
      title = "Gene complexity %",
      xaxis = list(title = 'Rank', type = "log"),
      yaxis = if (perc) {
        list(tickformat = "%", title = 'Cumulative fraction of total reads till rank')
      }
      else {
        list(title = 'Cumulative reads till rank')
      },
      legend = list(tracegroupgap = 0)
    ) %>%
    config(
      toImageButtonOptions = list(
        format = "png",
        filename = "countdistline",
        width = 1500,
        height = 1000
      )
    )
  
  header_col <- c()
  group_color <- NA
  for (var in colnames(se)) {
    temp <- compData[compData$sample == var, ]
    if (!is.na(color)) {
      group_color <- temp[[color]][1]
    }
    p <- add_trace(
      p,
      x = temp$rank,
      y = if (perc) {
        temp$fraction
      } else {
        temp$value
      },
      showlegend = if (!is.na(color) & group_color %in% header_col) {FALSE} else{TRUE},
      legendgroup = if (is.na(color)) {var} else {group_color},
      color = if (is.na(color)) {var} else {group_color},
      text = if (perc) {
        paste(
          temp$sample,
          "\n",
          temp$rank,
          "Genes\n",
          round(temp$fraction * 100, 2),
          '% Reads\n'
        )
      } else {
        paste(
          temp$sample,
          "\n",
          temp$rank,
          "Genes\n",
          formatC(
            temp$value,
            format = "f",
            big.mark = ".",
            digits = 0
          ),
          'Reads\n'
        )
      },
      hoverinfo = 'text'
    )
    if (!is.na(color)) {
      header_col <- c(header_col, as.character(group_color))
    }
  }
  p
}

## --------------------------------------------------------------------------

## ----- RAW DATA PLOTS / NORMALIZATION PLOTS -----


#' Stacks count data from dge list in the function stackDge().
#' A line plot is created based on Log2CPM and the density of the counts.
#' A dot line plot is created based on the number of reads at rank.
#'
#' @param dge DGE list object, containing samples and counts
#' @param color String, Sort samples based on a group
#'
#' @return p, (Plotly object) plot
#'
#' @export

countDistributionLinePlot <- function(dge, color = "None") {
  if (color == "None") {color <- NA}
  
  stackCounts <- data.frame(stackDge(dge))
  stackCounts <- merge(
    x = stackCounts,
    y = dge$samples,
    by.x = "sample",
    by.y = "row.names",
    all.x = TRUE
  )
  
  p <- plot_ly(type = 'scattergl',
               mode = 'lines',
               source = "dist_line") %>%
    plotly::layout(
      title = "Gene count distribution",
      xaxis = list(title = 'Log2CPM'),
      yaxis = list(title = 'Density'),
      legend = list(tracegroupgap = 0)
    ) %>%
    config(
      toImageButtonOptions = list(
        format = "png",
        filename = "countdistline",
        width = 1500,
        height = 1000
      )
    )
  
  header_col <- c()
  group_color <- NA
  for (var in unique(stackCounts$sample)) {
    temp <- stackCounts[stackCounts$sample == var,]
    density <- density(temp$logCPM)
    if (!is.na(color)) {
      group_color <- temp[[color]][1]
    }
    p <- add_trace(
      p,
      x = density$x,
      y = density$y,
      name = if (is.na(color)) {var} else {group_color},
      text = var,
      legendgroup = if (is.na(color)) {NULL} else {group_color},
      showlegend = if (!is.na(color) & group_color %in% header_col) {FALSE} else{TRUE},
      color = if (is.na(color)) {NULL} else {group_color},
      hoverinfo = 'text',
      fill = 'tozeroy',
      alpha = 0.05
    )
    if (!is.na(color)) {
      header_col <- c(header_col, as.character(group_color))
    }
  }
  p
}


#' Stacks count data from dge list in the function stackDge().
#' A box plot is created based on Log2CPM counts of the samples.
#'
#' @param dge DGE list object, containing samples and counts
#'
#' @return p, (Plotly object) plot
#'
#' @export

countDistributionBoxPlot <- function(dge) {
  stackCounts <- data.frame(stackDge(dge))
  
  p <- plot_ly(type = 'box',
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
    temp <- stackCounts[stackCounts$sample == var,]
    p <- add_trace(p,
                   y = temp$logCPM,
                   name = var,
                   alpha = 0.5) %>%
      plotly::layout(
        title = "Gene count distribution",
        xaxis = list(title = ''),
        yaxis = list(title = 'Log2CPM')
      )
  }
  p
}


#' Calculates required values with 'voom' method.
#' The plot is created with plotly with values retrieved from the voom object.
#'
#' @param dge DGE list object, containing samples and counts
#' @param sourceId plot ID, depends on raw/normalized counts
#'
#' @return p, (Plotly object) plot
#'
#' @export

voomPlot <- function(dge, sourceId) {
  v <- voom(2 ^ (dge$counts), save.plot = TRUE)
  
  p <- plot_ly(
    x = ~ v$voom.xy$x,
    y = ~ v$voom.xy$y,
    type = "scattergl",
    mode = "markers",
    color = "Voom",
    alpha = 0.75,
    text = names(v$voom.xy$x),
    hoverinfo = 'text',
    key = ~ names(v$voom.xy$x),
    source = sourceId
  ) %>%
    add_trace(
      mode = "lines",
      x = v$voom.line$x,
      y = v$voom.line$y,
      hoverinfo = 'none',
      line = list(color = "rgba(7, 164, 181, 1)"),
      name = "Voom average"
    ) %>%
    plotly::layout(
      title = "Voom",
      xaxis = list(title = 'Average Log2 Count'),
      yaxis = list(title = 'SQRT(Standard Deviation)'),
      clickmode = "event+select",
      dragmode = "select"
    ) %>%
    config(toImageButtonOptions = list(
      format = "png",
      filename = sourceId,
      width = 1500,
      height = 1000
    ))
  p
}


#' Calculates required values with 'plotMDS' method.
#' The plot is created with plotly with values retrieved from the mds object.
#' Plot is colored based on the selected column.
#'
#' @param dge DGE list object, containing samples and counts
#' @param color String, Column on wich colors should be based
#' @param sourceId plot ID, depends on raw/normalized counts
#'
#' @return p, (Plotly object) plot
#'
#' @export

multidimensionalScalingPlot <- function(dge, color, sourceId) {
  logFC <- plotMDS(dge$counts, ndim = ncol(dge) - 1)
  for_plots <- data.frame(logFC$cmdscale.out)
  for_plots$group <- dge$samples[, color]
  
  p <- plot_ly(
    data = for_plots,
    x = ~ X1,
    y = ~ X2,
    type = "scattergl",
    mode = "markers",
    color = ~ for_plots$group,
    text = rownames(for_plots),
    hoverinfo = 'text',
    marker = list(size = 15,
                  line = list(color = '#999999',
                              width = 1)),
    key = ~ rownames(for_plots),
    source = sourceId
  ) %>%
    plotly::layout(
      title = paste("MDS Plot"),
      xaxis = list(title = 'MDS1'),
      yaxis = list(title = 'MDS2'),
      clickmode = "event+select",
      dragmode = "select"
    ) %>%
    config(toImageButtonOptions = list(
      format = "png",
      filename = sourceId,
      width = 1500,
      height = 1000
    ))
  p
}

## --------------------------------------------------------------------------

## ----- DIMENSION REDUCTION PLOTS -----


#' Columns and rows from DGE list are turned.
#' PCA is calculated with prcomp.
#' PC percentages are calculated.
#' Barplot is created with PC percentages.
#'
#' @param dge DGE list object, containing samples and counts
#'
#' @return p, (Plotly object) plot
#'
#' @export

variancePcaPlot <- function(dge) {
  tdge <- t(dge$counts)
  tdge[!is.finite(tdge)] <- 0
  pca <- prcomp(tdge, center = TRUE)
  percent <- data.frame(summary(pca)$importance[2, ])
  colnames(percent) <- "percent"
  
  p <- plot_ly(
    data = percent,
    x = rownames(percent),
    y = ~ percent,
    type = "bar"
  ) %>%
    plotly::layout(
      title = "PCA Scree",
      xaxis = list(title = 'Component', categoryorder = 'trace'),
      yaxis = list(title = 'Percentage', tickformat = ".2%")
    ) %>%
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


#' Columns and rows from DGE list are turned.
#' PCA is calculated with prcomp.
#' PC percentages are calculated.
#' Scatter plot is created based on selected PCs.
#'
#' @param dge DGE list object, containing samples and counts
#' @param color String, Column on which colors should be based
#' @param getPC1 String, Selected PC to be plotted on x-axis
#' @param getPC2 String, Selected PC to be plotted on y-axis
#'
#' @return p, (Plotly object) plot
#'
#' @export

pcaPlot <- function(dge, color, getPC1, getPC2) {
  tdge <- t(dge$counts)
  tdge[!is.finite(tdge)] <- 0
  pca <- prcomp(tdge, center = TRUE)
  percent <- data.frame(summary(pca)$importance[2, ])
  colnames(percent) <- "percent"
  
  pca <- data.frame(scale(tdge, center = T, scale = F) %*% pca$rotation)
  pca$group <- dge$samples[, color]
  
  p <- plot_ly(
    data = pca,
    x = pca[[getPC1]],
    y = pca[[getPC2]],
    type = "scattergl",
    mode = "markers",
    color =  ~ pca$group,
    text = rownames(pca),
    hoverinfo = 'text',
    marker = list(size = 15,
                  line = list(color = '#999999',
                              width = 1)),
    key = ~ rownames(pca),
    source = "pca"
  ) %>%
    plotly::layout(
      title = 'PCA',
      xaxis = list(title = paste0(
        getPC1, " (", round(percent[getPC1, ] * 100, 2), "%)"
      )),
      yaxis = list(title = paste0(
        getPC2, " (", round(percent[getPC2, ] * 100, 2), "%)"
      )),
      clickmode = "event+select",
      dragmode = "select"
    ) %>%
    config(toImageButtonOptions = list(
      format = "png",
      filename = "pca",
      width = 1500,
      height = 1000
    ))
  p
}


#' Perplexity is set (max 30 & min 1), this depends on the number of samples
#' tsne model is calculated.
#' Scatter plot is created based on selected PCs.
#'
#' @param dge DGE list object, containing samples and counts
#' @param color String, Column on which colors should be based
#'
#' @return p, (Plotly object) plot
#'
#' @export

tsnePlot <- function(dge, color) {
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
    }, silent = TRUE
    )
    perplexity <- perplexity - 1
  }
  
  tsne_data <- as.data.frame(tsne_model$Y)
  rownames(tsne_data) <- colnames(dge$counts)
  tsne_data$color <- dge$samples[[color]]
  
  p <- plot_ly(
    data = tsne_data,
    x = ~V1,
    y = ~V2,
    type = "scattergl",
    mode = "markers",
    source = "tsne",
    key = ~ rownames(tsne_data),
    marker = list(size=15,
                  line = list(color = '#999999',
                              width = 1)),
    color = ~color,
    text = rownames(tsne_data),
    hoverinfo = 'text') %>%
    plotly::layout(
      title = "t-SNE",
      xaxis = list(title = "tSNE 1"),
      yaxis = list(title = "tSNE 2"),
      clickmode = "event+select",
      dragmode = "select") %>%
    config(
      toImageButtonOptions = list(
        format = "png",
        filename = "t-SNE",
        width = 1500,
        height = 1000
      )
    )
  p
}

#' Get dendrogram data dendro_data().
#' Plot a dendrogram.
#'
#' @param d Hclust object, tree object
#' @param color String, color samples
#' @param color_list Vector, Color genes
#'
#' @return p, (Plotly object) plot
#'
#' @export

plotly_dendrogram <- function(d, color, color_list) {
  dendro_data <- get_dendrogram_data(d)
  
  p <- plot_ly(
    data = dendro_data,
    x = ~ x,
    y = ~ y,
    color = I("black"),
    hoverinfo = "none"
  )  %>%
    add_segments(xend = ~ xend,
                 yend = ~ yend,
                 showlegend = FALSE) %>%
    add_markers(
      data = dendro_data[dendro_data$label != "", ],
      x = ~ x,
      y = ~ y,
      color = color,
      marker = list(size = 10,
                    color = color_list),
      text = ~ label,
      hoverinfo = 'text'
    ) %>%
    plotly::layout(
      dragmode = "zoom",
      title = "Dendrogram",
      xaxis = list(
        title = "",
        showticklabels = FALSE,
        zeroline = FALSE
      ),
      yaxis = list(title = "Height")
    ) %>%
    config(toImageButtonOptions = list(
      format = "png",
      filename = "dendro",
      width = 1500,
      height = 1000
    ))
  p
}

## --------------------------------------------------------------------------

## ----- HEATMAPS PLOTS -----


#' LogCPM values of counts are calculated.
#' Variance is calculated and the first x genes are kept.
#' Heatmap with the values left in high_var_cpm.
#'
#' @param dge DGE list object, containing samples and counts
#' @param group_col String, Sort samples based on a group
#' @param amount Integer, The number of genes shown in plot
#'
#' @return p, (Plotly object) plot
#'
#' @export

variableHeatmapPlot <- function(dge, group_col, amount) {
  lcpm <- dge$counts
  var_genes <- apply(lcpm, 1, var)
  select_var <- names(sort(var_genes, decreasing = TRUE))[1:amount]
  high_var_cpm <- lcpm[select_var, ]
  high_var_cpm <- as.data.frame(stack(high_var_cpm))
  
  if (group_col != "None") {
    order <- as.data.frame(as.character(dge$samples[[group_col]]), rownames(dge$samples))
    colnames(order) <- "group"
    order <- order[order(order$group), , drop = F]
    high_var_cpm <- high_var_cpm[order(match(high_var_cpm$col, rownames(order))), ]
  }
  
  p <- plot_ly(
    data = high_var_cpm,
    x = ~ col,
    y = ~ row,
    z = ~ value,
    colorbar = list(title = "Log2CPM", len = 1),
    type = "heatmap",
    hoverinfo = 'text',
    text = paste(
      "Sample:",
      high_var_cpm$col,
      "<br>Gene:",
      high_var_cpm$row,
      "<br>Log2CPM:",
      high_var_cpm$value
    )
  ) %>%
    plotly::layout(
      title = "Most variable genes",
      xaxis = list(
        title = '',
        categoryorder = "array",
        categoryarray = ~ col
      ),
      yaxis = list(
        title = '',
        categoryorder = "array",
        autorange = "reversed"
      )
    ) %>%
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

#' The DE table is sorted on FDR/adjPvalue.
#' The first x genes are kept.
#' Normalized values are extracted based on the DE genes still present.
#' Heatmap with the normalized values of genes is created.
#'
#' @param deTab Dataframe, with all analysis results
#' @param dge DGE list object, containing samples and counts
#' @param group_col String, Sort samples based on a group
#' @param amount Integer, The number of genes shown in plot
#'
#' @return p, (Plotly object) plot
#'
#' @export

topDgeHeatmapPlot <- function(deTab, dge, group_col, amount) {
  sortdeTab <- deTab[order(rank(deTab$FDR)), ]
  sortdeTab <- head(sortdeTab, amount)
  getnorm <- dge[rownames(sortdeTab), ]
  getnorm <- getnorm$counts
  getnorm <- as.data.frame(stack(getnorm))
  
  if (group_col != "None") {
    order <- as.data.frame(as.character(dge$samples[[group_col]]), rownames(dge$samples))
    colnames(order) <- "group"
    order <- order[order(order$group), , drop = F]
    getnorm <- getnorm[order(match(getnorm$col, rownames(order))), ]
  }
  
  p <- plot_ly(
    data = getnorm,
    x = ~ col,
    y = ~ row,
    z = ~ value,
    colorbar = list(title = "Log2CPM", len = 1),
    type = "heatmap",
    hoverinfo = 'text',
    text = paste(
      "Sample:",
      getnorm$col,
      "<br>Gene:",
      getnorm$row,
      "<br>Log2CPM:",
      getnorm$value
    )
  ) %>%
    plotly::layout(
      title = "Most expressed genes",
      xaxis = list(
        title = '',
        categoryorder = "array",
        categoryarray = ~ col
      ),
      yaxis = list(
        title = '',
        categoryorder = "array",
        autorange = "reversed"
      )
    ) %>%
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


#' The number of unique values in column 'DE' are extracted.
#' Percentages of these values are calculated and renamed.
#' Barplot is created with DE results based on percentages.
#'
#' @param deTab Dataframe, with all analysis results
#'
#' @return p, (Plotly object) plot
#'
#' @export

deRatioPlot <- function(deTab) {
  defeatures <- aggregate(deTab$DE, by = list(category = deTab$DE), FUN = length)
  defeatures$perc <- defeatures[, 2] / sum(defeatures[, 2])
  defeatures$category <- c("Down regulated", "Not sign.", "Up regulated")[match(defeatures$category, c(-1, 0, 1))]
  
  p <- plot_ly(
    data = defeatures,
    x = "",
    y = ~ perc,
    color = ~ category,
    type = "bar",
    text = paste(defeatures[, 2], "Genes\n", round(defeatures$perc * 100, 2), "%"),
    textposition = "auto",
    textfont = list(color = "black"),
    hovertext = ~ category,
    hoverinfo = 'text'
  ) %>%
    plotly::layout(
      title = "Differential expression ratio",
      xaxis = list(title = ""),
      yaxis = list(tickformat = "%", title = "Ratio")
    ) %>%
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


#' The confidence prediction calculated in the function gamConfidenceFit().
#' Confidence is calculated based on avgLog2CPM.
#' Scatterplot is created with DE results and a line showing confidence.
#'
#' @param deTab Dataframe, with all analysis results
#'
#' @return p, (Plotly object) plot
#'
#' @export

ma_plot <- function(deTab) {
  prediction <- gamConfidenceFit(deTab, "avgLog2CPM")
  
  p <- plot_ly(
    data = deTab[deTab$DE == 0, ],
    x = ~ avgLog2CPM,
    y = ~ avgLog2FC,
    type = "scattergl",
    mode = "markers",
    color = ~ as.character(DE),
    alpha = 0.75,
    text = rownames(deTab[deTab$DE == 0, ]),
    hoverinfo = 'text',
    key = ~ rownames(deTab[deTab$DE == 0, ]),
    source = "analysis_ma"
  ) %>%
    add_trace(
      x = ~ deTab[deTab$DE != 0, ]$avgLog2CPM,
      y = ~ deTab[deTab$DE != 0, ]$avgLog2FC,
      color = as.character(deTab[deTab$DE != 0, ]$DE),
      alpha = 0.75,
      text = rownames(deTab[deTab$DE != 0, ]),
      hoverinfo = 'text',
      key = ~ rownames(deTab[deTab$DE != 0, ])
    ) %>%
    add_trace(
      data = prediction,
      mode = "lines",
      x = ~ avgLog2CPM,
      y = ~ fit,
      text = NA,
      key = NA,
      color = "Fitted",
      line = list(color = 'rgba(7, 164, 181, 1)'),
      name = "Fitted"
    ) %>%
    add_ribbons(
      data = prediction,
      ymin = ~ fit - 1.96 * se.fit,
      ymax = ~ fit + 1.96 * se.fit,
      fillcolor = "rgba(7, 164, 181, 0.25)",
      text = NA,
      key = NA,
      color = "Standard Error",
      line = list(color = 'rgba(0, 0, 0, 0)'),
      name = "Standard Error"
    ) %>%
    plotly::layout(
      title = "MA",
      xaxis = list(title = 'Average Log2 CPM'),
      yaxis = list(title = 'Average Log2 FC'),
      clickmode = "event+select",
      dragmode = "select"
    ) %>%
    config(toImageButtonOptions = list(
      format = "png",
      filename = "ma",
      width = 1500,
      height = 1000
    ))
  p
}


#' Creates scatter plot with avgLog2FC vs -log10(FDR).
#' Two lines are generated indicating LogFC cutoff and p value cutoff.
#'
#' @param deTab Dataframe, with all analysis results
#' @param LogCut Integer, LogFC cutoff for line
#' @param PCut Integer, Pvalue cutoff for line
#'
#' @return p, (Plotly object) plot
#'
#' @export

volcanoPlot <- function(deTab, LogCut, PCut) {
  p <- plot_ly(
    data = deTab[deTab$DE == 0, ],
    x = ~ avgLog2FC,
    y = ~ -log10(FDR),
    type = "scattergl",
    mode = "markers",
    color = ~ as.character(DE),
    alpha = 0.75,
    text = rownames(deTab[deTab$DE == 0, ]),
    hoverinfo = 'text',
    key = ~ rownames(deTab[deTab$DE == 0, ]),
    source = "analysis_volcano"
  ) %>%
    add_trace(
      x = ~ deTab[deTab$DE != 0, ]$avgLog2FC,
      y = ~ -log10(deTab[deTab$DE != 0, ]$FDR),
      color = as.character(deTab[deTab$DE != 0, ]$DE),
      alpha = 0.75,
      text = rownames(deTab[deTab$DE != 0, ]),
      hoverinfo = 'text',
      key = ~ rownames(deTab[deTab$DE != 0, ])
    ) %>%
    plotly::layout(
      title = "Volcano",
      xaxis = list(title = 'Average Log2 FC'),
      yaxis = list(title = '- Log10 P-Value'),
      shapes = list(
        list(
          type = "line",
          line = list(color = "red"),
          x0 = 0,
          x1 = 1,
          xref = "paper",
          y0 = PCut,
          y1 = PCut
        ),
        
        list(
          type = "line",
          line = list(color = "red"),
          x0 = LogCut,
          x1 = LogCut,
          y0 = 0,
          y1 = 1,
          yref = "paper"
        ),
        list(
          type = "line",
          line = list(color = "red"),
          x0 = -LogCut,
          x1 = -LogCut,
          y0 = 0,
          y1 = 1,
          yref = "paper"
        )
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


#' The DE table is sorted on FDR/adjPvalue.
#' The first x genes are kept.
#' Normalized values are extracted based on the DE genes still present.
#' Normalized values are stacked.
#' Scatter plot is created with LogCPM values per gene per sample.
#'
#' @param deTab Dataframe, with all analysis results
#' @param dge DGE list object, containing samples and counts
#' @param color String, Column on wich colors should be based
#' @param amount Integer, The number of genes shown in plot
#' @param selected Vector, Extra selected rownames
#'
#' @return p, (Plotly object) plot
#'
#' @export

barcodePlot <- function(deTab, dge, color, amount, selected) {
  sortdeTab <- deTab[order(rank(deTab$FDR)), ]
  sortdeTab <- head(sortdeTab, amount)
  getnorm <- dge[c(rownames(sortdeTab), selected), ]
  getnorm$counts <- getnorm$counts
  stack1 <- as.data.frame(stack(getnorm$counts))
  stack1$group <- getnorm$samples[[color]][stack1$col]
  
  if (is.null(stack1$group)) {
    return(NULL)
  }
  
  p <- plot_ly(
    type = "scattergl",
    mode = "markers",
    marker = list(
      symbol = "line-ns-open",
      size = 250 / (amount + length(selected)),
      line = list(width = 2)
    )
  )
  p <- add_trace(
    p,
    x = ~ stack1$value,
    y = ~ stack1$row,
    color = ~ stack1$group,
    text = stack1$col,
    hoverinfo = 'text'
  ) %>%
    plotly::layout(
      title = "Barcode",
      xaxis = list(title = 'Log2 CPM'),
      yaxis = list(
        title = '',
        categoryorder = "array",
        autorange = "reversed"
      ),
      autosize = T
    ) %>%
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


#' The Pvalues are rounded on two decimals.
#' All occurences of pvalues are counted.
#' Bar plot is created with the p value vs occurence.
#'
#' @param deTab Dataframe, with all analysis results
#'
#' @return p, (Plotly object) plot
#'
#' @export

pValuePlot <- function(deTab) {
  pvalue <- round(deTab$P.Value, digits = 2)
  pvalue <- aggregate(pvalue, by = list(p = pvalue), FUN = length)
  
  p <- plot_ly(
    data = pvalue,
    x = ~ p,
    y = ~ x,
    type = "bar"
  ) %>%
    plotly::layout(
      title = "P-Value",
      xaxis = list(title = 'P-Value'),
      yaxis = list(title = 'Count')
    ) %>%
    config(toImageButtonOptions = list(
      format = "png",
      filename = "pvalue",
      width = 1500,
      height = 1000
    ))
  p
}

## --------------------------------------------------------------------------

## ----- BIAS PLOTS -----


#' The confidence prediction calculated in the function gamConfidenceFit().
#' Confidence is calculated based on the GC or length.
#' Scatterplot is created with avgLog2FC and corresponding bias value.
#'
#' @param deTab Dataframe, with all analysis results
#' @param biasColumn String, Column indicating bias values (GC or length)
#' @param log Boolean, Show plot in Log scale
#'
#' @return p, (Plotly object) plot
#'
#' @export

biasPlot <- function(deTab, biasColumn, log, tick, sourceId) {
  if (is.null(biasColumn)) {
    return(NULL)
  }
  prediction <- gamConfidenceFit(deTab, biasColumn)
  
  p <- plot_ly(
    data = deTab,
    x = ~ get(biasColumn),
    y = ~ avgLog2FC,
    type = 'scattergl',
    mode = "markers",
    color = ~ FDR,
    alpha = 0.75,
    showlegend = FALSE,
    text = rownames(deTab),
    hoverinfo = 'text',
    key = rownames(deTab),
    source = sourceId
  ) %>%
    add_trace(
      data = prediction,
      mode = "lines",
      x = ~ get(biasColumn),
      y = ~ fit,
      text = NA,
      key = NA,
      color = "green",
      line = list(color = 'rgba(7, 164, 181, 1)'),
      name = "Fitted"
    ) %>%
    add_ribbons(
      data = prediction,
      ymin = ~ fit - 1.96 * se.fit,
      ymax = ~ fit + 1.96 * se.fit,
      fillcolor = "rgba(7, 164, 181, 0.25)",
      text = NA,
      key = NA,
      color = "green",
      line = list(color = 'rgba(0, 0, 0, 0)'),
      name = "Standard Error"
    ) %>%
    plotly::layout(
      title = paste("Bias based on", biasColumn),
      xaxis = list(
        title = biasColumn,
        type = log,
        tickformat = tick
      ),
      #, type = "log"),
      yaxis = list(title = 'Average Log2 FC'),
      clickmode = "event+select",
      dragmode = "select"
    ) %>%
    config(toImageButtonOptions = list(
      format = "png",
      filename = "bias",
      width = 1500,
      height = 1000
    ))
  p
}


#' The number of genes are grouped by strand and DE.
#' A barplot is created with the # genes divided between + and - strand.
#'
#' @param deTab Dataframe, with all analysis results
#'
#' @return p, (Plotly object) plot
#'
#' @export

geneStrandBar <- function(deTab) {
  geneStrand <- as.data.frame(table(deTab$geneStrand, deTab$DE, dnn = c("strand", "DE")))
  geneStrand$DE <- c("Down regulated", "Not sign.", "Up regulated")[match(geneStrand$DE, c(-1, 0, 1))]
  geneStrand$perc <- geneStrand$Freq / sum(geneStrand$Freq)
  
  p <- plot_ly(
    data = geneStrand,
    x = ~ strand,
    y = ~ Freq,
    color = ~ DE,
    orientation = 'v',
    type = "bar",
    text = ~ paste(Freq, "Genes\n", round(perc * 100, 2), "%"),
    textposition = "auto",
    textfont = list(color = "black"),
    hovertext = ~ DE,
    hoverinfo = 'text'
  ) %>%
    plotly::layout(
      title = "Gene strand",
      xaxis = list(title = 'Gene strand'),
      yaxis = list(title = 'Number of genes')
    ) %>%
    config(
      toImageButtonOptions = list(
        format = "png",
        filename = "geneStrand",
        width = 1500,
        height = 1000
      )
    )
  p
}
})
## --------------------------------------------------------------------------

## ----- INFORMATION BOX -----


#' Template for plot information.
#'
#' @param infoText String, Explanation of a plot
#'
#' @return Shiny Box object
#'
#' @export

informationBox <- function(infoText) {
  tryCatch({
    box(
      title = icon("info-circle"),
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      collapsed = FALSE,
      span(infoText,
           style = "padding-left: 5px; text-align: justify; display: block;"),
      style = "padding-left: unset;"
    )
  }, error = function(err) {
    return(NULL)
  })
}

## --------------------------------------------------------------------------
