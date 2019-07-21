label<-function (mat, nbmask = matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 
                               ncol = 3), wrap = FALSE) 
{
  if (!is.logical(mat)) {
    stop("Labelling of patches requirese a logical matrix", 
         "(TRUE/FALSE values): please convert your data first.")
  }
  if (!is.matrix(mat)) {
    stop("The input object must be a matrix")
  }
  if (all(mat)) {
    result <- matrix(1, nrow = nrow(mat), ncol = ncol(mat))
    attr(result, "psd") <- prod(dim(mat))
    attr(result, "percolation") <- TRUE
    return(result)
  }
  if (!any(mat)) {
    result <- matrix(NA, nrow = nrow(mat), ncol = ncol(mat))
    attr(result, "psd") <- integer(0)
    attr(result, "percolation") <- FALSE
    return(result)
  }
  if (ncol(mat) == 1 || nrow(mat) == 1) {
    vec <- as.vector(mat)
    result <- cumsum(c(vec[1] > 0, diff(vec)) == 1) * vec
    result <- ifelse(result > 0, result, NA)
    if (wrap && !is.na(head(result, 1)) && !is.na(tail(result, 
                                                       1))) {
      result[result == tail(result, 1)] <- head(result, 
                                                1)
    }
    attr(result, "psd") <- tabulate(result)
    dim(result) <- dim(mat)
    attr(result, "percolation") <- any(mat)
    return(result)
  }
  label_cpp(mat, nbmask, wrap)
}