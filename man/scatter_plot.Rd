% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{scatter_plot}
\alias{scatter_plot}
\title{Create scatter plot with ggplot2}
\usage{
scatter_plot(
  df,
  x,
  y,
  text = NA,
  group,
  size = 1.5,
  scale = NA,
  index = NA,
  key = NA,
  title = "",
  xlab = "",
  ylab = ""
)
}
\arguments{
\item{df}{Dataframe, Dataframe to create plot with}

\item{x}{String, Value to plot on X axis}

\item{y}{String, Value to plot on Y axis}

\item{text}{String, Hover info text}

\item{group}{String, Value to color/group data}

\item{size}{Numeric, size of dots (default 1.5)}

\item{scale}{String, Should X-axis be scaled in log10}

\item{index}{Vector, vector of items to select from dataframe}

\item{key}{String, Value for point selection by plotly}

\item{title}{String, Title for plot}

\item{xlab}{String, Label for X axis}

\item{ylab}{String, Label for Y axis}
}
\value{
gg, Plot object (ggplot2)
}
\description{
Create scatter plot with ggplot2
}
