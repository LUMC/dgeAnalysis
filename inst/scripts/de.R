
## ----- Read files -----

## Add counts to se
readCountsFromTable <- function(data_counts, data_samples) {
  out <- as.matrix(data_counts[, colnames(data_counts) %in% rownames(data_samples)])
  se <- SummarizedExperiment(assays=list(counts=out))
  se
}

## Add samples to se
addSamplesFromTableToSE <- function(se, data_samples){
  data_samples <- droplevels(data_samples)
  samples <- intersect(colnames(se), rownames(data_samples))
  se <- se[,samples]
  colData(se) <- DataFrame(data_samples[samples,])
  names(colData(se)) <- colnames(data_samples)
  se
}

## Add annotation to se
addAnnotationsFromTableToSE <- function(se, data_annotation){
  features <- intersect(rownames(se), rownames(data_annotation))
  se <- se[features,]
  rowData(se) <- DataFrame(data_annotation[features,])
  se
}

## --------------------------------------------------------------------------

## ----- Analysis utility -----

getCount <- function(x, raw){
  if (x["feature"] %in% rownames(raw)) {
    return(raw[x["feature"], x["sample"]])
  } else {
    features <- rownames(raw)[! grepl("^__", rownames(raw))]
    return(sum(raw[features, x["sample"]]))
  }
}

alignmentSummary <- function(se){
  specialFeatures <- rownames(se)[ grepl( "^__", rownames(se) ) ]
  out <- expand.grid(feature=c("aligned", specialFeatures), sample=colnames(se))
  out$count <- apply(out, 1, getCount, assays(se)$counts)
  out
}

complexityData <- function(se, max=1000){
  features <- rownames(se)[ ! grepl( "^__", rownames(se) ) ]
  ranks <- c(1:max)
  out <- expand.grid(rank=ranks, sample=colnames(se))
  for (x in colnames(se)){
    values <- as.vector(assay(se)[features,x])
    sorted <- sort(values, T)
    total <- sum(sorted)
    out[out$sample == x, "value"] <- cumsum(sorted)[1:max]
    out[out$sample == x, "fraction"] <- out[out$sample == x, "value"] /total
  }
  out
}

stackDge <- function(dge){
  count <- stack(dge$counts)
  names(count) <- c("feature", "sample", "logCPM")
  count
}

gamConfidenceFit <- function(deTab, biasColumn) {
  method.args = list()
  method.args$method <- "REML"
  
  formula <- avgLog2FC ~ s(columnHere, bs = "cs")
  formula <- paste(gsub("columnHere", parse(text=biasColumn), formula))
  formula <- eval(parse(text = gsub("\\", "", paste(formula[2], formula[3], sep=" ~ "), fixed=TRUE)))
  
  base.args <- list(quote(formula), data = quote(deTab))
  gamModel <- do.call(mgcv::gam, c(base.args, method.args))
  prediction <- augment(gamModel)
  prediction <- prediction[order(prediction[[biasColumn]]), ]
  
  setRange <- range(1, nrow(prediction))
  result <- round(seq(setRange[1], setRange[2], length.out = 500))
  prediction <- prediction[c(result),]
}

## --------------------------------------------------------------------------


