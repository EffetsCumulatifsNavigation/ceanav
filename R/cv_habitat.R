#' Habitats: Integrated habitats dataset
#'
#' Intégration des données utilisées pour caractériser la composante valorisée d'habitats
#'
#' @keywords zostère
#' @keywords milieu humide
#' @keywords habitat
#' @keywords composante valorisée
#'
#' @export
#'
#' @details Cette fonction télécharge et formatte les données
#'

cv_habitats <- function() {
  # Load grid
  data(aoi_grid1000poly)

  # Identify grid cells with zostera
  zostereID <- st_intersects(zostere_inv, aoi) %>%
               unlist() %>%
               unique()

  # Add info to grid
  zostere <- aoi %>% mutate(zostere = 0)
  zostere$zostere[zostereID] <- 1
  
}
