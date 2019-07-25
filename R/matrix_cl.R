#' matrix_cl
#'
#' @param ncell
#' @param prob_vector
#'
#' @return
<<<<<<< HEAD
#' @export
#'
#' @examples
matrix_cl <- function(prob, ncell) {
  rbinom(n = ncell^2, size = 1, prob = prob) %>%
    as.logical(.) %>%
    matrix(data = ., nrow = ncell, ncol = ncell) %>%
    RandomClustering::label(.)
}
=======
#' @export matrix_cl
#'
#' @examples
matrix_cl <- function(prob, ncell) {
    rbinom(n = ncell^2, size = 1, prob = prob) %>%
      as.logical(.) %>%
      matrix(data = ., nrow = ncell, ncol = ncell) %>%
      spatialwarnings::label(.)
  }


>>>>>>> 683122258085aad9c9064332efbd4d935dc5f425
