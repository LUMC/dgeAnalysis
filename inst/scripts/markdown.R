
## ----- MARKDOWN FUNCTIONS -----

createDesign <- function(dge, dbase, dvalue, matrix_v1, matrix_v2) {
  columns <- c(dbase, dvalue)
  
  matrix <- NULL
  for (value in c(matrix_v1, matrix_v2)) {
    matrix <- c(matrix, paste0(na.omit(columns[which(dge == value, arr.ind=T)[, "col"]]), value))
  }
  
  getdesign <- "~"
  
  tryCatch({
    if (isTRUE(all.equal(sort(unique(matrix)), sort(as.vector(paste0(dbase, unique(dge[[dbase]]))))))) {
      getdesign <- "~0+"
    }
  }, error = function(err) {
    return(NULL)
  })
  
  for (value in columns) {
    if (all(value == columns[1])) {
      getdesign <- c(getdesign, value)
    } else {
      isPresent <- FALSE
      for (mat in matrix) {
        if (mat %in% dge[[value]]) {
          isPresent <- TRUE
        }
      }
      if (isPresent) {
        getdesign <- c(getdesign, paste0("+", columns[1], ":", value))
      } else {
        getdesign <- c(getdesign, paste0("+", value))
      }
    }
  }
  getdesign <- paste(getdesign, collapse = '')
  getdesign
}

relevelSamples <- function(dge, dbase, dvalue, matrix_v1, matrix_v2) {
  columns <- c(dbase, dvalue)
  for (value in columns) {
    newRef <- as.character(dge$samples[[value]][!dge$samples[[value]] %in% c(matrix_v1, matrix_v2)][1])
    if (is.na(newRef)) {
      newRef <- as.character(dge$samples[[value]][1])
    }
    dge$samples[[value]] <- relevel(as.factor(dge$samples[[value]]), ref = newRef)
  }
  dge
}

createMatrix <- function(dge, dbase, dvalue, matrix) {
  getbase <- NULL
  for (value in matrix) {
    if (value %in% dge$samples[[dbase]]) {
      getbase <- c(getbase, paste0(dbase, value))
    }
  }
  
  getmatrix <- NULL
  for (column1 in getbase) {
    for (column2 in dvalue) {
      for (value in matrix) {
        if (value %in% dge$samples[[column2]]) {
          getmatrix <- c(getmatrix, paste0(column1, ":", column2, value))
        }
      }
    }
  }
  
  if (is.null(getmatrix)) {
    getmatrix <- getbase
  }
  
  getmatrix
}

createContrast <- function(design, matrix_v1, matrix_v2) {
  contrast = integer(length(colnames(design)))
  contrast[match(matrix_v1, colnames(design))] <- -1
  contrast[match(matrix_v2, colnames(design))] <- 1
  contrast
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

## --------------------------------------------------------------------------

