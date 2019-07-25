#' @title Labelling of unique patches and detection of percolation.
#'
#' @description Label each patch with a number in a binary matrix
#'
#' @param mat A binary matrix
#'
#' @param nbmask a "neighboring mask": a matrix with odd dimensions describing
#'   which cells are to be considered as neighbors around a cell
#'   (see examples).
#'
#' @param wrap Whether to wrap around lattice boundaries (`TRUE`/`FALSE`),
#'   effectively using periodic boundaries.
#'
#' @return A matrix containing ID numbers for each connected patch. Default
#'   parameters assume 4-cell neighborhood and periodic boundaries. The
#'   distribution of patch sizes is returned as the attribute "psd" and the
#'   percolation status as "percolation" (whether a TRUE patch has a width
#'   or height equal to the size of the matrix).
#'
#' @details The \code{label} function "labels" the patches of a binary (TRUE/FALSE)
#'   matrix. It returns a matrix of similar height and width, with integer
#'   values representing the ID of each unique patch (contiguous cells).
#'   Empty cells are labeled as \code{NA}.
#'
#' @seealso \code{\link{patchsizes}}
#'
#' @examples
#'
#' data(forestgap)
#'
#' par(mfrow=c(1, 2))
#' rmat <- matrix(rnorm(100) > .1, ncol = 10)
#' image(rmat)
#' image(label(rmat))
#'
#' # With 8-way neighborhood mask and no wrapping around borders
#' nbmask8 <- matrix(c(1,1,1,
#'                     1,0,1,
#'                     1,1,1), ncol=3)
#' image(label(rmat, nbmask8, wrap = FALSE))
#'
#' @export
label <- function(mat,
                  nbmask = matrix(c(0,1,0,
                                    1,0,1,
                                    0,1,0), ncol=3), # 4way NB
                  wrap = FALSE) {

  if ( ! is.logical(mat) ) {
    stop('Labelling of patches requirese a logical matrix',
         '(TRUE/FALSE values): please convert your data first.')
  }

  if ( ! is.matrix(mat) ) {
    stop('The input object must be a matrix')
  }

  # The matrix is full
  if ( all(mat) ) {
    result <- matrix(1, nrow = nrow(mat), ncol = ncol(mat))
    attr(result, "psd") <- prod(dim(mat))
    attr(result, "percolation") <- TRUE
    return(result)
  }

  # The matrix is empty
  if ( !any(mat) ) {
    result <- matrix(NA, nrow = nrow(mat), ncol = ncol(mat))
    attr(result, "psd") <- integer(0)
    attr(result, "percolation") <- FALSE
    return(result)
  }

  # The matrix is a row/column vector
  if ( ncol(mat) == 1 || nrow(mat) == 1 ) {
    vec <- as.vector(mat)
    result <- cumsum( c(vec[1] > 0, diff(vec)) == 1 ) * vec
    result <- ifelse(result > 0, result, NA)
    # If we wrap, then we need to merge the two patches at the end of the vector
    if ( wrap && !is.na(head(result, 1)) && !is.na(tail(result, 1)) ) {
      result[ result == tail(result, 1) ] <- head(result, 1)
    }

    # PSD is the just the number of times each unique values appears in the
    # result vector.
    attr(result, "psd") <- tabulate(result)
    # Adjust dimensions
    dim(result) <- dim(mat)

    # If there is a patch, then it necessarily has the width or height
    # of the matrix, so percolation is present.
    attr(result, "percolation") <- any(mat)
    return(result)
  }

  # Otherwise we scan for patches
  label_cpp(mat, nbmask, wrap)
}
