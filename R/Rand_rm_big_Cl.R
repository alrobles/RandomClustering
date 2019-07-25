#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
Rand_rm_big_Cl <- function(data) {
  clust_casos <- data %>% table() %>% data.frame() %>% select(Freq)
  casos_max <- which(clust_casos == max(clust_casos))

  if (length(unique(clust_casos$Freq)) == 1) {
    data <- data
  } else {
    if (length(casos_max) == 1) {
      coordenadas <- data.frame(which(data == casos_max, arr.ind = TRUE))
      indice <- 1:dim(coordenadas)[1] %>% sample(1)
      muestra <- as.vector(coordenadas[indice, ])
      data[muestra$row, muestra$col] <- NA
    } else {
      caso_max <- sample(casos_max, 1)
      coordenadas <- data.frame(which(data == caso_max, arr.ind = T))
      indice <- 1:dim(coordenadas)[1] %>% sample(1)
      muestra <- data.frame(coordenadas[indice, ])
      data[muestra$row, muestra$col] <- NA
    }
  }
  return(data)
}
