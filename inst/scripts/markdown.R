
## ----- MARKDOWN FUNCTIONS -----

reOrderDesign <- function(matrix_value, design_value, data_samples) {
  check_matrix <- strsplit(matrix_value, " - ", fixed = TRUE)[[1]]
  check_design <- strsplit(design_value, " + ", fixed = TRUE)[[1]]
  
  get_column <- which(check_matrix[1] == data_samples, arr.ind=TRUE)[,2][[1]]
  column <- colnames(data_samples[get_column])
  remove_design_items <- c("~0", column)
  new_design <- setdiff(check_design, remove_design_items)
  
  if (length(new_design) != 0) {
    new_design <- paste("~0 +", column, "+", gsub(",", " +", toString(c(new_design))))
  } else {
    new_design <- paste("~0 +", column)
  }
  new_design
}

highExpressedFeatures <- function(method, dge, design_value, cpm_value) {
  if (method == "edger") {
    edger <- calcNormFactors( dge, method = "TMM")
    counts <- cpm(edger, log = TRUE)
    selectedFeatures <- rownames( edger )[ apply( counts, 1, function( v ) sum( v >= cpm_value ) ) >= 1/4 * ncol( counts ) ]
    
  } else {
    selectedFeatures <- filterByExpr(dge, model.matrix(eval(parse(text=design_value)), dge$samples ))
    
  }
  selectedFeatures
}

## NOT IN USE ANYMORE
filterDge <- function(normDge, excluded_samples, data_samples, se) {
  normDge$counts <- normDge$counts[,!colnames(normDge$counts) %in% excluded_samples]
  data_samples <- data_samples[!rownames(data_samples) %in% excluded_samples, ]
  data_samples <- droplevels(data_samples)
  
  se <- addSamplesFromTableToSE(se, data_samples)
  
  tempDge <- DGEList(counts = normDge$counts, samples = colData(se), genes = normDge$genes)
  tempDge <- calcNormFactors( tempDge, method = "TMM")
  tempDge
}

reNameDesign <- function(design, normDge) {
  for (sample_column in rev(colnames(normDge$samples))) {
    if (startsWith(colnames(design)[1], sample_column)) {
      colnames(design) <- sub(sample_column, "", colnames(design))
    }
  }
  design
}

## --------------------------------------------------------------------------

