#' process_rm_paste
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
process_rm_paste <- function(data){
  data <- big_cl(data) %>%
    small_cl()
  data[!is.na(data)] <- 1
  data[is.na(data)] <- 0
  data <- data %>%
    as.logical() %>%
    matrix(ncol = dim(data)[1] ) %>%
    RandomClustering::label()
  return(data)
}
