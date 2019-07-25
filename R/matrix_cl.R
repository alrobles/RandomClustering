#' matrix_cl
#'
#' @param ncell
#' @param prob_vector
#'
#' @return
#' @export
#'
#' @examples
matrix_cl <- function(prob, ncell) {
  rbinom(n = ncell^2, size = 1, prob = prob) %>%
    as.logical(.) %>%
    matrix(data = ., nrow = ncell, ncol = ncell) %>%
    RandomClustering::label(.)
}
