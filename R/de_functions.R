

#' Add all counts that are in samples in SummerizedExperiment.
#'
#' @param data_counts Dataframe, Containing all raw count data
#' @param data_samples Dataframe, Containing all sample data
#'
#' @return se, (SummerizedExperiment) With counts
#'
#' @export

readCountsFromTable <- function(data_counts, data_samples) {
  out <- as.matrix(data_counts[, colnames(data_counts) %in% rownames(data_samples)])
  se <- SummarizedExperiment(assays = list(counts = out))
  se
}


#' Add samples to the SummerizedExperiment.
#'
#' @param se SummerizedExperiment, With counts
#' @param data_samples Dataframe, Containing all sample data
#'
#' @return se, (SummerizedExperiment) With samples and counts
#'
#' @export

addSamplesFromTableToSE <- function(se, data_samples) {
  data_samples <- droplevels(data_samples)
  samples <- intersect(colnames(se), rownames(data_samples))
  se <- se[, samples]
  colData(se) <- DataFrame(data_samples[samples, , drop = FALSE])
  se
}


#' Add annotation to the SummerizedExperiment.
#'
#' @param se SummerizedExperiment, With counts
#' @param data_annotation Dataframe, Containing all annotation data
#'
#' @return se, (SummerizedExperiment) With samples, counts and annotation
#'
#' @export

addAnnotationsFromTableToSE <- function(se, data_annotation) {
  features <- intersect(rownames(se), rownames(data_annotation))
  se <- se[features, ]
  rowData(se) <- DataFrame(data_annotation[features, ])
  se
}


#' Count the read counts per feature to find number of reads that are aligned, not aligned, etc.
#'
#' @param x Containing the value/label on which to count reads
#' @param raw Dataframe, Containing count data
#'
#' @return Integer, With total counted reads
#'
#' @export

getCount <- function(x, raw) {
  if (x["feature"] %in% rownames(raw)) {
    return(raw[x["feature"], x["sample"]])
  } else {
    features <- rownames(raw)[!grepl("^__", rownames(raw))]
    return(sum(raw[features, x["sample"]]))
  }
}


#' Creates dataframe with number of reads per gene, ordered on genes with the most reads assigned.
#'
#' @param se SummerizedExperiment, With samples, counts (and annotation)
#' @param max Integer, The maximum rank that is used
#'
#' @return out, (Dataframe) With total read counts per gene
#'
#' @export

complexityData <- function(se, max) {
  features <- rownames(se)[!grepl("^__", rownames(se))]
  ranks <- c(1:max)
  out <- expand.grid(rank = ranks, sample = colnames(se))
  for (x in colnames(se)) {
    values <- as.vector(assay(se)[features, x])
    sorted <- sort(values, T)
    total <- sum(sorted)
    out[out$sample == x, "value"] <- cumsum(sorted)[1:max]
    out[out$sample == x, "fraction"] <- out[out$sample == x, "value"] / total * 100
  }
  out
}


#' Stacks total DGE counts based on: mapping feature (aligned, not aligned, etc.), sample and LogCPM.
#'
#' @param dge DGE list object, containing samples and counts
#' @param max Integer, The maximum rank that is used
#'
#' @return count, (Dataframe) With mapping feature, sample and LogCPM
#'
#' @export

stackDge <- function(dge) {
  count <- stack(dge$counts)
  names(count) <- c("feature", "sample", "logCPM")
  count
}


#' Fix hover info for violin plot, as it cant find CPM values
#'
#' @param plot List object, List with plot object from ggplotly
#'
#' @return plot, List object with fixed hover
#'
#' @export

fix_violin_hover <- function(plot) {
  for (x in 1:length(plot[["x"]][["data"]])) {
    layer <- plot[["x"]][["data"]][[x]]
    logcpm <- round(layer[["x"]], 2)
    temp <- sub("(<br />).*", "", layer[["text"]])
    plot[["x"]][["data"]][[x]][["text"]] <- paste(temp, "\nLog2CPM:", logcpm)
  }
  
  plot
}


#' Get heigth information from tree object.
#' Get labels/names from tree ends.
#' Calculate start and end positions of tree branches
#' Save line coordinates in dataframe
#'
#' @param tree Hclust object, tree object
#'
#' @return new, (Dataframe) dataframe with all dendrogram line coordinates
#'
#' @export

get_dendrogram_data <- function(tree,
                                labels = NULL,
                                horiz = FALSE,
                                reverseDirection = FALSE,
                                hang = 0.1,
                                xlab = "",
                                ylab = "",
                                axes = TRUE,
                                cex.labels = 1,
                                ...,
                                adjustRange = FALSE) {
  hang.gr = hang
  if (hang < 0)
    hang.gr = 0.1
  n = length(tree$order)
  heights = tree$height
  range = range(heights)
  hang.scaled = hang.gr * (max(heights) - min(heights))
  range[1] = range[1] - hang.scaled
  
  indexLim = c(0.5, n + 0.5)
  if (adjustRange)
  {
    ctr = mean(indexLim)
    indexLim = ctr + (indexLim - ctr) / 1.08
  }
  
  nMerge = n - 1
  if (is.null(labels))
    labels = tree$labels
  if (is.null(labels))
    labels = rep("", n)
  if (is.na(labels[1]))
    labels = rep("", n)
  if (is.logical(labels) && labels[1] == "FALSE")
    labels = rep("", n)
  
  singleton.x = rep(NA, n)
  singleton.x[tree$order] = c(1:n)
  cluster.x = rep(NA, n)
  
  new <- data.frame(matrix(
    ncol = 5,
    nrow = 0,
    dimnames = list(NULL, c("label", "x", "y", "xend", "yend"))
  ))
  for (m in 1:nMerge) {
    o1 = tree$merge[m, 1]
    o2 = tree$merge[m, 2]
    h = heights[m]
    hh = if (hang > 0)
      h - hang.scaled
    else
      range[1]
    h1 = if (o1 < 0)
      hh
    else
      heights[o1]
    h2 = if (o2 < 0)
      hh
    else
      heights[o2]
    
    x1 = if (o1 < 0)
      singleton.x[-o1]
    else
      cluster.x[o1]
    x2 = if (o2 < 0)
      singleton.x[-o2]
    else
      cluster.x[o2]
    
    cluster.x[m] = mean(c(x1, x2))
    
    label1 <- labels[-o1]
    label2 <- labels[-o2]
    if (length(label1) > 1) {
      label1 <- ""
    }
    if (length(label2) > 1) {
      label2 <- ""
    }
    
    if (!is.na(x1)) {
      new[nrow(new) + 1, ] <- c(label1, x1, h1, x1, h)
      if (!is.na(x2)) {
        new[nrow(new) + 1, ] <- c("", x1, h, x2, h)
        new[nrow(new) + 1, ] <- c(label2, x2, h2, x2, h)
      }
    }
  }
  new[2:5] <- sapply(new[2:5], as.double)
  new <- new[order(new$x), ]
  return(new)
}

## --------------------------------------------------------------------------
