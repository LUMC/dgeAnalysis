
#' Create line plot with ggplot2
#'
#' @param df Dataframe, Dataframe to create plot with
#' @param x String, Value to plot on X axis
#' @param y String, Value to plot on Y axis
#' @param text String, Hover info text
#' @param group String, Value to color/group data
#' @param plot String, Extra condition for complexity plot
#' @param title String, Title for plot
#' @param xlab String, Label for X axis
#' @param ylab String, Label for Y axis
#'
#' @return gg, Plot object (ggplot2)
#'
#' @export

line_plot <- function(df, x, y, text = NA, group, plot = "", title = "", xlab = "", ylab = "") {
  gg <- ggplot(data = df, aes_string(
    x = x,
    y = y,
    color = group,
    text = text
  )) +
    geom_line(aes(group = sample), size = 1) +
    labs(title = title,
         x = xlab,
         y = ylab) +
    theme_bw()
  
  if (plot == "complexity") {
    gg <- gg + scale_x_continuous(trans = "log10")
  }
  
  gg
}


#' Create violin plot with ggplot2
#'
#' @param df Dataframe, Dataframe to create plot with
#' @param text String, Hover info text
#' @param group String, Value to color/group data
#' @param title String, Title for plot
#' @param xlab String, Label for X axis
#' @param ylab String, Label for Y axis
#'
#' @return gg, Plot object (ggplot2)
#'
#' @export

violin_plot <- function(df, text = NA, group, title = "", xlab = "", ylab = "") {
  gg <- ggplot(data = df, aes_string(
    x = "sample",
    y = "logCPM",
    color = group,
    fill = group,
    text = text
  )) +
    geom_violin(aes(group = sample), alpha = 0.75, size = 1) +
    coord_flip() +
    labs(title = title,
         x = xlab,
         y = ylab) +
    theme_bw()
  
  gg
}


#' Create scatter plot with ggplot2
#'
#' @param df Dataframe, Dataframe to create plot with
#' @param x String, Value to plot on X axis
#' @param y String, Value to plot on Y axis
#' @param text String, Hover info text
#' @param group String, Value to color/group data
#' @param size Numeric, size of dots (default 1.5)
#' @param scale String, Should X-axis be scaled in log10
#' @param index Vector, vector of items to select from dataframe
#' @param key String, Value for point selection by plotly
#' @param title String, Title for plot
#' @param xlab String, Label for X axis
#' @param ylab String, Label for Y axis
#'
#' @return gg, Plot object (ggplot2)
#'
#' @export

scatter_plot <- function(df, x, y, text = NA, group, size = 1.5, scale = NA, index = NA, key = NA, title = "", xlab = "", ylab = "") {
  gg <- ggplot(data = df, aes_string(
    x = x,
    y = y,
    key = key,
    text = text,
    color = group
  )) +
    geom_point(size = size, alpha = 0.5) +
    labs(title = title,
         x = xlab,
         y = ylab) +
    theme_bw()
  
  ## Add loess trend
  if (!is.na(index[1])) {
    gg <- gg + geom_smooth(
      data = df[index, ],
      aes_string(x = x, y = y),
      inherit.aes = FALSE,
      method = "loess",
      fill = "#3366ff",
      alpha = 0.2
    )
  }
  
  if (!is.na(scale)) {
    gg <- gg + scale_x_continuous(trans = "log10")
  }
  
  gg
}


#' Create bar plot with ggplot2
#'
#' @param df Dataframe, Dataframe to create plot with
#' @param x String, Value to plot on X axis
#' @param y String, Value to plot on Y axis
#' @param text String, Hover info text
#' @param group String, Value to color/group data
#' @param fill String, value to fill bars by color
#' @param colorbar String, Should default colors be used
#' @param rev Boolean, Plot bars in reverse order (alignment sum) for correct colors
#' @param facet String, Should data be ordered by group
#' @param title String, Title for plot
#' @param xlab String, Label for X axis
#' @param ylab String, Label for Y axis
#'
#' @return gg, Plot object (ggplot2)
#'
#' @export

bar_plot <- function(df, x, y, text = NA, group, fill = NULL, colorbar = NA, rev = FALSE, facet = "none", title = "", xlab = "", ylab = "") {
  gg <- ggplot(data = df, aes_string(
    x = x,
    y = y,
    fill = fill,
    text = text
  )) +
    geom_bar(stat = "identity", position = position_stack(reverse = rev)) +
    labs(title = title,
         x = xlab,
         y = ylab) +
    theme_bw()
  
  if (!is.na(colorbar)) {
    gg <- gg + scale_fill_manual(values = c("#619CFF", "#F8766D", "#00BA38"))
  }
  
  if (facet != "none") {
    gg <- gg + facet_grid(as.formula(paste("~", group)), scales = "free")
  }
  
  gg
}


#' Create barcode plot with ggplot2
#'
#' @param df Dataframe, Dataframe to create plot with
#' @param x String, Value to plot on X axis
#' @param y String, Value to plot on Y axis
#' @param text String, Hover info text
#' @param group String, Value to color/group data
#' @param title String, Title for plot
#' @param xlab String, Label for X axis
#' @param ylab String, Label for Y axis
#'
#' @return gg, Plot object (ggplot2)
#'
#' @export

barcode_plot <- function(df, x, y, text = NA, group, title = "", xlab = "", ylab = "") {
  gg <- ggplot(data = df, aes_string(
    x = x,
    y = y,
    color = group,
    text = text
  )) +
    geom_point(size = 4, alpha = 0.5) +
    labs(title = title,
         x = xlab,
         y = ylab) +
    theme_bw()
  
  gg
}


#' Create dendrogram plot with ggplot2
#'
#' @param df Dataframe, Dataframe to create plot with
#' @param text String, Hover info text
#' @param group String, Value to color/group data
#' @param title String, Title for plot
#' @param xlab String, Label for X axis
#' @param ylab String, Label for Y axis
#'
#' @return gg, Plot object (ggplot2)
#'
#' @export

dendro_plot <- function(df, text = NA, group, title = "", xlab = "", ylab = "") {
  gg <- ggplot(data = df) +
    geom_segment(aes(
      x = x,
      y = y,
      xend = xend,
      yend = yend
    )) +
    geom_point(data = df[df$label != "", ], aes_string(x = "x", y = "y", color = group, text = text), size = 4) +
    labs(title = title,
         x = xlab,
         y = ylab) +
    theme_bw() +
    theme(axis.text.x = element_blank())
  
  gg
}


#' Create heatmap plot with ggplot2
#'
#' @param df Dataframe, Dataframe to create plot with
#' @param x String, Value to plot on X axis
#' @param y String, Value to plot on Y axis
#' @param text String, Hover info text
#' @param group String, Value to color/group data
#' @param fill String, value to fill bars by color
#' @param title String, Title for plot
#' @param xlab String, Label for X axis
#' @param ylab String, Label for Y axis
#'
#' @return gg, Plot object (ggplot2)
#'
#' @export

heatmap_plot <- function(df, x, y, text = NA, group, fill, title = "", xlab = "", ylab = "") {
  gg <- ggplot(data = df, aes_string(
    x = x,
    y = y,
    fill = fill,
    text = text
  )) +
    geom_tile() +
    labs(title = title,
         x = xlab,
         y = ylab) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45))
  
  if (table(df[[x]])[1] > 50) {
    gg <- gg + theme(axis.text.y = element_blank())
  }
  
  if (group != "none") {
    gg <- gg + facet_grid(as.formula(paste("~", group)), scales = "free")
  }
  
  gg
}


#' Create network plot with ggplot2
#'
#' @param df Dataframe, Dataframe to create plot with
#' @param text String, Hover info text
#' @param label1 Boolean, Should terms be labeled in plot
#' @param label2 Boolean, Should genes be labeled in plot
#' @param title String, Title for plot
#' @param xlab String, Label for X axis
#' @param ylab String, Label for Y axis
#'
#' @return gg, Plot object (ggplot2)
#'
#' @export

network_plot <- function(df, text = NA, label1 = TRUE, label2 = FALSE, title = "", xlab = "", ylab = "") {
  gg <- ggplot() +
    geom_segment(
      data = df[[1]],
      aes(
        x = from.x,
        xend = to.x,
        y = from.y,
        yend = to.y,
      ),
      size = 1,
      colour = "grey"
    ) +
    geom_point(
      data = df[[2]],
      aes(x = V1, y = V2),
      size = 8,
      colour = "red"
    ) +
    geom_point(
      data = df[[3]],
      aes_string(x = "V1", y = "V2", colour = "fc", text = text),
      size = 4
    ) +
    theme_void()
  
  ## Add label to terms
  if (label1) {
    gg <- gg + geom_text(
      data = df[[2]],
      aes(x = V1,
          y = V2,
          label = stringr::str_wrap(genes, 25)),
      inherit.aes = FALSE,
      check_overlap = TRUE,
      size = 3
    )
  }
  
  ## Add label to genes
  if (label2) {
    gg <- gg + geom_text(
      data = df[[3]],
      aes(x = V1,
          y = V2,
          label = stringr::str_wrap(genes, 25)),
      inherit.aes = FALSE,
      check_overlap = TRUE,
      size = 3
    )
  }
  
  gg
}
