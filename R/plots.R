
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
    labs(title = title,
         x = xlab,
         y = ylab) +
    theme_bw()
}

temp1 <- function() {
  index <- round(seq(1, nrow(deTab), length.out = 1000))
  deTab <- deTab[order(deTab$avgLog2CPM), ]
  deTab$DE <- factor(
    x = deTab$DE,
    levels = c(0, 1, -1),
    labels = c(
      "Not significant",
      "Up-regulated",
      "Down-regulated"
    )
  )
}

scatter_plot <- function(df, size = 1.5, source = NA, index = NA, x, y, group, title = "", xlab = "", ylab = "") {
  gg <- ggplot(data = df, aes_string(
    x = x,
    y = y,
    color = group
  )) +
    geom_point(size = size, alpha = 0.5) +
    labs(title = title,
         x = xlab,
         y = ylab) +
    theme_bw()
  
  ## Add loess trend for MA
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
  
  ggplotly(gg, source = source)
}


bar_plot <- function(df, group, title = "", xlab = "", ylab = "") {
  gg <- ggplot(data = df, aes_string(
    x = "count",
    y = group,
    fill = "feature"
  )) +
    geom_bar(stat = "identity") +
    labs(title = title,
         x = xlab,
         y = ylab) +
    theme_bw()
  ggplotly(gg)
}


barcode_plot <- function(dge) {
  temp <- dge$counts[1:10, ]
  temp <- reshape2::melt(temp)
  
  gg <- ggplot(data = temp, aes(
    x = value,
    y = Var1,
    color = Var2,
    text = paste('Sample:', Var2,
                 '<br>Log2CPM: ', value)
  )) +
    geom_point() +
    labs(title = "Plot",
         x = "Label X",
         y = "Label Y") +
    theme_bw()
  ggplotly(gg, tooltip = "text")
}
