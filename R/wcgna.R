
## --------------------------------------------------------------------------

## ----- TREE PLOTS -----


plot_dendro_new <- function(d, color) {
  tidy_segments <- dendro_data(d)
  
  branch_height <- mean(tidy_segments$segments$y[tidy_segments$segments$yend == 0]/10)
  tidy_segments$labels$y <- tidy_segments$segments$y[tidy_segments$segments$yend == 0]-branch_height
  tidy_segments$segments$yend[tidy_segments$segments$yend == 0] <- tidy_segments$segments$y[tidy_segments$segments$yend == 0]-branch_height
  
  p <- plot_ly(
    data = tidy_segments$segments,
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
      data = tidy_segments$labels,
      x = ~x,
      y = ~y,
      color = color,
      marker = list(
        size = 10
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

#Markers add branch split:

#get_xy <- function(node) {
#  m <- dendextend::get_nodes_xy(node)
#  colnames(m) <- c("x", "y")
#  tibble::as_tibble(m)
#}

#xy_nodes <- get_xy(as.dendrogram(d))

#add_markers(
#  data = xy_nodes[xy_nodes$y > 0,],
#  x = ~x,
#  y = ~y,
#  name = "nodes"
#) %>%


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

## ----- HEATMAP PLOTS -----

#d <- datTraits
#d <- normDge$samples


plot_dendro_heat <- function(d) {
  heatmap_list <- list()
  for(column in colnames(d)) {
    p <- plot_ly(
      x = rownames(d),
      y = column,
      z = t(d[[column]]),
      type = "heatmap",
      showscale = FALSE,
      hoverinfo = 'text',
      text = matrix(paste("Sample:", rownames(d),
                   "<br>Column:", rep(c(column), nrow(d)),
                   "<br>Value:", t(d[[column]])), ncol = nrow(d))
    )
    heatmap_list[[column]] <- p
  }
  subplot(
    heatmap_list,
    nrows = ncol(d),
    shareX = TRUE,
    margin = 0.00025
  )
}


## --------------------------------------------------------------------------
