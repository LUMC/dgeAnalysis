
#' Create vector from two design values and two matrix values.
#' Check if all values are the same then design will start with ~0.
#' Get column names based on corresponding values in matrix.
#' Paste column names together with +.
#' Create String from vector seprated by ''.
#'
#' @param dge DGE list object, containing samples and counts
#' @param dbase Vector, base design column
#' @param dvalue Vector, other design columns
#' @param matrix_v1 Vector, containing the left matrix values
#' @param matrix_v2 Vector, containing the right matrix values
#'
#' @return getdesign, (String) String with the design formula
#'
#' @export

createDesign <- function(dge, dbase, dvalue, matrix_v1, matrix_v2) {
  columns <- c(dbase, dvalue)
  matrix <- c(matrix_v1, matrix_v2)
  getdesign <- "~"
  
  tryCatch({
    if (isTRUE(all.equal(sort(unique(matrix)), sort(as.vector(unique(
      dge[[dbase]]
    )))))) {
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


#' Create vector from two matrix values.
#' Try to relevel samples on values that are not present in matrix.
#'
#' @param dge DGE list object, containing samples and counts
#' @param dbase Vector, base design column
#' @param dvalue Vector, other design columns
#' @param matrix_v1 Vector, containing the left matrix values
#' @param matrix_v2 Vector, containing the right matrix values
#'
#' @return dge, (DGE list object) containing samples and counts
#'
#' @export

relevelSamples <- function(dge, dbase, dvalue, matrix_v1, matrix_v2) {
    columns <- c(dbase, dvalue)
    for (value in columns) {
      newRef <-
        as.character(dge$samples[[value]][!dge$samples[[value]] %in% c(matrix_v1, matrix_v2)][1])
      if (is.na(newRef)) {
        newRef <- as.character(dge$samples[[value]][1])
      }
      dge$samples[[value]] <-
        relevel(as.factor(dge$samples[[value]]), ref = newRef)
    }
    dge
  }


#' Getbase column and use this to create right matrix.
#' Loop through base, nested columns and matrix to create full matrix.
#'
#' @param dge DGE list object, containing samples and counts
#' @param dbase Vector, base design column
#' @param dvalue Vector, other design columns
#' @param matrix Vector, containing the matrix values
#'
#' @return dge, (DGE list object) containing samples and counts
#'
#' @export

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


#' Create contrast by finding matrix values in the design matrix.
#' Values are assigned based if present in matrix (v1 or v2) else 0.
#'
#' @param design String, in use design matrix
#' @param matrix_v1 Vector, containing the left matrix values
#' @param matrix_v2 Vector, containing the right matrix values
#'
#' @return contrast, (vector) containing the contrasts (0, 1, -1)
#'
#' @export

createContrast <- function(design, matrix_v1, matrix_v2) {
  contrast = integer(length(colnames(design)))
  contrast[match(matrix_v1, colnames(design))] <- 1
  contrast[match(matrix_v2, colnames(design))] <- -1
  contrast
}
