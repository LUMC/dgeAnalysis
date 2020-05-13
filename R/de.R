
## ----- Read files -----


## readCountsFromTable()
##  Add all counts that are in samples in SummerizedExperiment
## Parameters:
##  data_counts = Dataframe, Containing all raw count data
##  data_samples = Dataframe, Containing all sample data
## Returns:
##  se = SummerizedExperiment, With counts

readCountsFromTable <- function(data_counts, data_samples) {
  out <- as.matrix(data_counts[, colnames(data_counts) %in% rownames(data_samples)])
  se <- SummarizedExperiment(assays=list(counts=out))
  se
}


## addSamplesFromTableToSE()
##  Add samples to the SummerizedExperiment
## Parameters:
##  se = SummerizedExperiment, With counts
##  data_samples = Dataframe, Containing all sample data
## Returns:
##  se = SummerizedExperiment, With samples and counts

addSamplesFromTableToSE <- function(se, data_samples){
  data_samples <- droplevels(data_samples)
  samples <- intersect(colnames(se), rownames(data_samples))
  se <- se[,samples]
  colData(se) <- DataFrame(data_samples[samples,, drop = FALSE])
  se
}


## addAnnotationsFromTableToSE()
##  Add annotation to the SummerizedExperiment
## Parameters:
##  se = SummerizedExperiment, With samples and counts
##  data_annotation = Dataframe, Containing all annotation data
## Returns:
##  se = SummerizedExperiment, With samples, counts and annotation

addAnnotationsFromTableToSE <- function(se, data_annotation){
  features <- intersect(rownames(se), rownames(data_annotation))
  se <- se[features,]
  rowData(se) <- DataFrame(data_annotation[features,])
  se
}

## --------------------------------------------------------------------------

## ----- Analysis utility -----


## getCount()
##  Count the read counts per feature to find number of reads that are aligned, not aligned, etc.
## Parameters:
##  x = String, Containing the value on which to count reads
##  raw = Dataframe, Containing count data
## Returns:
##  Integer, With total counted reads

getCount <- function(x, raw){
  if (x["feature"] %in% rownames(raw)) {
    return(raw[x["feature"], x["sample"]])
  } else {
    features <- rownames(raw)[! grepl("^__", rownames(raw))]
    return(sum(raw[features, x["sample"]]))
  }
}


## alignmentSummary()
##  Creates dataframe with counts per mapping feature (aligned, not aligned, etc.)
## Parameters:
##  se = SummerizedExperiment, With samples, counts (and annotation)
## Returns:
##  out = Dataframe, With total counts per available mapping feature

alignmentSummary <- function(se){
  specialFeatures <- rownames(se)[ grepl( "^__", rownames(se) ) ]
  out <- expand.grid(feature=c("aligned", specialFeatures), sample=colnames(se))
  out$count <- apply(out, 1, getCount, assays(se)$counts)
  out
}


## complexityData()
##  Creates dataframe with number of reads per gene, ordered on genes with the most reads assigned.
## Parameters:
##  se = SummerizedExperiment, With samples, counts (and annotation)
##  max = Integer, The maximum rank that is used
## Returns:
##  out = Dataframe, With total read counts per gene

complexityData <- function(se, max){
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


## stackDge()
##  Stacks total DGE counts based on: mapping feature (aligned, not aligned, etc.), sample and LogCPM
## Parameters:
##  dge = DGE list object, containing samples and counts
## Returns:
##  count = Dataframe, With mapping feature, sample and LogCPM

stackDge <- function(dge){
  count <- stack(dge$counts)
  names(count) <- c("feature", "sample", "logCPM")
  count
}


## gamConfidenceFit()
##  Calculates a confidence fit for various plots.
##  Model used is GAM to calculate predictions
## Parameters:
##  deTab = Dataframe, with all analysis results
##  biasColumn = String, Value on which to calculate confidence
## Returns:
##  prediction = Vector, With coordinates of prediction locations (line plot)

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
  prediction$.se.fit <- as.vector(prediction$.se.fit)
  
  setRange <- range(1, nrow(prediction))
  result <- round(seq(setRange[1], setRange[2], length.out = 500))
  prediction <- prediction[c(result),]
}

## --------------------------------------------------------------------------
