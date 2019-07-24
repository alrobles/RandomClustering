#' label_binary_lattice
#'
#' @param ncell
#' @param prob_vector
#'
#' @return
#' @export
#'
#' @examples
label_binary_lattice <- function(ncell = 1000, prob_vector) {
    prob_matrix <- prob_vector %>%
      purrr::map(RandomClustering::matrix_cl, ncell)
    return(prob_matrix)
}

