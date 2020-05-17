
## --------------------------------------------------------------------------

## ----- TREE PLOTS -----

get_dendrogram_data <- function(tree, labels = NULL, 
                               horiz = FALSE, reverseDirection = FALSE,
                               hang = 0.1, xlab = "", ylab = "", axes = TRUE,
                               cex.labels = 1, ..., adjustRange = FALSE) {
  hang.gr = hang;
  if (hang < 0) hang.gr = 0.1;
  n = length(tree$order);
  heights = tree$height;
  range = range(heights);
  hang.scaled = hang.gr * (max(heights) - min(heights));
  range[1] = range[1] - hang.scaled;
  
  indexLim = c(0.5, n+0.5);
  if (adjustRange)
  {
    ctr = mean(indexLim);
    indexLim = ctr + (indexLim - ctr)/1.08;
  }
  
  nMerge = n-1;
  if (is.null(labels)) labels = tree$labels;
  if (is.null(labels)) labels = rep("", n);
  if (is.na(labels[1])) labels = rep("", n);
  if (is.logical(labels) && labels[1]=="FALSE") labels = rep("", n);
  
  singleton.x = rep(NA, n);
  singleton.x[tree$order] = c(1:n);
  cluster.x = rep(NA, n);
  
  new <- data.frame(matrix(ncol=5, nrow=0, dimnames=list(NULL, c("label", "x", "y", "xend", "yend"))))
  for (m in 1:nMerge) {
    o1 = tree$merge[m, 1]
    o2 = tree$merge[m, 2]
    h = heights[m];
    hh = if (hang>0) h-hang.scaled else range[1];
    h1 = if (o1 < 0) hh else heights[o1];
    h2 = if (o2 < 0) hh else heights[o2];
    
    x1 = if (o1 < 0) singleton.x[-o1] else cluster.x[o1]
    x2 = if (o2 < 0) singleton.x[-o2] else cluster.x[o2]
    
    cluster.x[m] = mean(c(x1, x2));
    
    label1 <- labels[-o1]
    label2 <- labels[-o2]
    if (length(label1) > 1) {
      label1 <- ""
    } 
    if (length(label2) > 1) {
      label2 <- ""
    }
    
    if (!is.na(x1)) {
      new[nrow(new) + 1,] <- c(label1, x1, h1, x1, h)
      if (!is.na(x2)) {
        new[nrow(new) + 1,] <- c("", x1, h, x2, h)
        new[nrow(new) + 1,] <- c(label2, x2, h2, x2, h)
      }
    }
  }
  new[2:5] <- sapply(new[2:5], as.double)
  new <- new[order(new$x),]
  return(new)
}


plotly_dendrogram <- function(d, color, color_list) {
  dendro_data <- get_dendrogram_data(d)
  
  p <- plot_ly(
    data = dendro_data,
    x = ~x,
    y = ~y,
    color = I("black"), 
    hoverinfo = "none"
  )  %>%
    add_segments(
      xend = ~xend,
      yend = ~yend,
      showlegend = FALSE
    ) %>%
    add_markers(
      data = dendro_data[dendro_data$label != "",],
      x = ~x,
      y = ~y,
      color = color,
      marker = list(
        size = 10,
        color = color_list
      ),
      text = ~label,
      hoverinfo = 'text'
    ) %>%
    layout(
      dragmode = "zoom",
      title = "Dendrogram with normalized log2CPM values",
      xaxis = list(title = "", showticklabels = FALSE, zeroline = FALSE),
      yaxis = list(title = "Height")
    )
  p
}

## --------------------------------------------------------------------------

## ----- HEATMAP PLOTS -----

plot_dendro_heat <- function(trait) {
  heatmap_list <- list()
  for(column in colnames(trait)) {
    p <- plot_ly(
      x = rownames(trait),
      y = column,
      z = t(trait[[column]]),
      type = "heatmap",
      showscale = FALSE,
      hoverinfo = 'text',
      text = matrix(paste("Sample:", rownames(trait),
                          "<br>Column:", rep(c(column), nrow(trait)),
                          "<br>Value:", t(trait[[column]])), ncol = nrow(trait))
    )
    heatmap_list[[column]] <- p
  }
  subplot(
    heatmap_list,
    nrows = ncol(trait),
    shareX = TRUE,
    margin = 0.00025
  )
}


## --------------------------------------------------------------------------

## ----- POWER PLOTS -----

plot_power <- function(soft, cutoff) {
  soft_combined <- data.frame(
    pwr = soft$fitIndices[,1],
    value = -sign(soft$fitIndices[,3])*soft$fitIndices[,2])
  
  p <- plot_ly(
    data = soft_combined,
    x = ~pwr,
    y = ~value,
    type = "scattergl",
    mode = "markers",
    text = ~paste("Power:", pwr),
    hoverinfo = 'text') %>%
    plotly::layout(
      title = "Scale independence",
      xaxis = list(title = "Soft Threshold (power)"),
      yaxis = list(title = "Scale Free Topology Model Fit, signed R^2"),
      shapes = list(
        list(type = "line", line = list(color = "red"),
             x0 = 0,  x1 = 1, xref = "paper",
             y0 = cutoff, y1 = cutoff)
      )
    )
  p
}


plot_soft <- function(soft) {
  soft_combined <- data.frame(
    pwr = soft$fitIndices[,1],
    value = soft$fitIndices[,5])
  
  p <- plot_ly(
    data = soft_combined,
    x = ~pwr,
    y = ~value,
    type = "scattergl",
    mode = "markers",
    text = ~paste("Power: ", pwr),
    hoverinfo = 'text') %>%
    plotly::layout(
      title = "Mean connectivity",
      xaxis = list(title = "Soft Threshold (power)"),
      yaxis = list(title = "Mean Connectivity")
    )
  p
}


## --------------------------------------------------------------------------

## ----- DENDRO MODULES -----

plot_modules <- function(color) {
  colorTable <- data.frame(table(color))
  colorTable <- colorTable[order(match(colorTable$color, unique(color))),]
  colorTable$perc <- colorTable$Freq/sum(colorTable$Freq)
  colorTable$color <- factor(colorTable$color, levels = colorTable$color)
  
  p <- plot_ly(
    data = colorTable,
    x = ~perc,
    y = NULL,
    type = "bar",
    color = ~color,
    marker = list(color = colorTable$color),
    text = ~paste(Freq, "Genes"),
    hoverinfo = "text"
  ) %>%
    layout(
      barmode = "stack",
      showlegend = FALSE,
      xaxis = list(title = "",
                   zeroline = FALSE,
                   showline = FALSE,
                   showticklabels = FALSE,
                   showgrid = FALSE),
      yaxis = list(zeroline = FALSE,
                   showline = FALSE,
                   showticklabels = FALSE,
                   showgrid = FALSE)
    )
  p
}

## --------------------------------------------------------------------------
