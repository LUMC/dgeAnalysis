
line_plot <- function(df, group, title = "", xlab = "", ylab = "", plot = NA) {
  gg <- ggplot(data = df, aes_string(
    x = "rank",
    y = "fraction",
    color = group
  )) +
    geom_line(aes(group = sample), size = 1) +
    labs(title = title,
         x = xlab,
         y = ylab) +
    theme_bw()
  
  if (plot == "complexity") {
    gg <- gg + scale_x_continuous(trans = "log10")
  }
  
  ggplotly(gg)
}

density_plot <- function(df, group, title = "", xlab = "", ylab = "") {
  gg <- ggplot(data = df, aes_string(
    x = "logCPM",
    color = group,
    fill = group
  )) +
    geom_line(aes(group = sample),
              stat = "density",
              size = 1,
              show.legend = TRUE) +
    geom_density(aes(group = sample),
                 alpha = 0.05,
                 size = 0,
                 show.legend = FALSE) +
    labs(title = title,
         x = xlab,
         y = ylab) +
    theme_bw()
  
  ggplotly(gg)
}


violin_plot <- function(df, group, title = "", xlab = "", ylab = "") {
  gg <- ggplot(data = df, aes_string(
    x = group,
    y = "logCPM",
    color = group,
    fill = group
  )) +
    geom_violin(aes(group = sample), alpha = 0.75, size = 1) +
    coord_flip() +
    labs(title = title,
         x = xlab,
         y = ylab) +
    theme_bw()
  
  ggplotly(gg)
}


scatter_plot <- function(df, size = 1.5, source = NA, key = NA, index = NA, x, y, group, title = "", xlab = "", ylab = "", scale = NA) {
  gg <- ggplot(data = df, aes_string(
    x = x,
    y = y,
    key = key,
    color = group
  )) +
    geom_point(size = size, alpha = 0.5) +
    labs(title = title,
         x = xlab,
         y = ylab) +
    theme_bw()
  
  ## Add loess trend
  if (!is.na(index)) {
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
  
  ggplotly(gg, source = source)
}

## x = "count",
## y = group,
## fill = "feature"
bar_plot <- function(df, group, x, y, fill = NULL, title = "", xlab = "", ylab = "", facet = NA) {
  gg <- ggplot(data = df, aes_string(
    x = x,
    y = y,
    fill = fill
  )) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("#619CFF", "#F8766D", "#00BA38")) +
    labs(title = title,
         x = xlab,
         y = ylab) +
    theme_bw()
  
  if (!is.na(facet)) {
    gg <- gg + facet_grid(as.formula(paste("~", group)), scales = "free")
  }
  
  ggplotly(gg)
}


barcode_plot <- function(df, x, y, group, title = "", xlab = "", ylab = "") {
  gg <- ggplot(data = df, aes_string(
    x = x,
    y = y,
    color = group
  )) +
    geom_point() +
    labs(title = title,
         x = xlab,
         y = ylab) +
    theme_bw()
  
  ggplotly(gg)
}


dendro_plot <- function(df, group = NULL, title = "", xlab = "", ylab = "") {
  gg <- ggplot(data = df) +
    geom_segment(aes(
      x = x,
      y = y,
      xend = xend,
      yend = yend
    )) +
    geom_point(data = df[df$label != "", ], aes_string(x = "x", y = "y", color = group)) +
    labs(title = title,
         x = xlab,
         y = ylab) +
    theme_bw()
  
  ggplotly(gg)
}


heatmap_plot <- function(df, group, title = "", xlab = "", ylab = "") {
  gg <- ggplot(data = df, aes(x = col,
                              y = row,
                              fill = value)) +
    geom_tile() +
    labs(title = title,
         x = xlab,
         y = ylab) +
    theme_bw()
  
  if (table(df$col)[1] > 50) {
    gg <- gg + theme(axis.text.y = element_blank())
  }
  
  if (group != "none") {
    gg <- gg + facet_grid(as.formula(paste("~", group)), scales = "free")
  }
  
  ggplotly(gg)
}
