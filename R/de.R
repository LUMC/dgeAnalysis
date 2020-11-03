
## ----- Read files -----


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

## --------------------------------------------------------------------------

## ----- Analysis utility -----


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


#' Creates dataframe with counts per mapping feature (aligned, not aligned, etc.).
#'
#' @param se SummerizedExperiment, With samples, counts (and annotation)
#'
#' @return out, (Dataframe) With total counts per available mapping feature
#'
#' @export

alignmentSummary <- function(se) {
  specialFeatures <- rownames(se)[grepl("^__", rownames(se))]
  out <-  expand.grid(feature = c("aligned", specialFeatures),
                sample = colnames(se))
  out$count <- apply(out, 1, getCount, assays(se)$counts)
  out
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
    out[out$sample == x, "fraction"] <-
      out[out$sample == x, "value"] / total
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


#' Calculates a confidence fit for various plots.
#' Model used is GAM to calculate predictions.
#'
#' @param deTab Dataframe, with all analysis results
#' @param biasColumn String, Value on which to calculate confidence
#'
#' @return prediction, (Vector) With coordinates of prediction locations (line plot)
#'
#' @export

gamConfidenceFit <- function(deTab, biasColumn) {
  method.args = list()
  method.args$method <- "REML"
  
  formula <- avgLog2FC ~ s(columnHere, bs = "cs")
  formula <- paste(gsub("columnHere", parse(text = biasColumn), formula))
  formula <- eval(parse(text = gsub(
    "\\", "", paste(formula[2], formula[3], sep = " ~ "), fixed = TRUE
  )))
  
  base.args <- list(quote(formula), data = quote(deTab))
  gamModel <- do.call(mgcv::gam, c(base.args, method.args))
  
  prediction <- deTab[, c(biasColumn, "avgLog2FC")]
  prediction <- cbind(prediction, predict(gamModel, se.fit = TRUE))
  prediction <- prediction[order(prediction[[biasColumn]]),]
  
  setRange <- range(1, nrow(prediction))
  result <- round(seq(setRange[1], setRange[2], length.out = 500))
  prediction <- prediction[c(result), ]
}

## --------------------------------------------------------------------------
