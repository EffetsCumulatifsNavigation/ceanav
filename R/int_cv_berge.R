#' Berges: Données sur l'intégrité des berges
#'
#' Intégration des données utilisées pour caractériser la composante valorisée d'intégrité des berges
#'
#' @keywords berge
#' @keywords composante valorisée
#'
#' @export
#'
#' @details Cette fonction télécharge et formatte les données
#'

cv_berge <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Include to study grid
  # ------------------------------------
  #
  # All we will do for now for this dataset is include it as presence-absence
  # in the study grid
  #
  # So I intersect the zostere db with the grid to identify which grid cell
  # intersect with the db
  #
  # WARNING: there is still work to do here, at this phase this is exploratory
  # TODO: see if if makes sense to divide by erosion processes identified in dataset
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load grid
  data(grid1p)

  # Identify grid cells with zostera
  bergeID <- st_intersects(berge, grid1p) %>%
             unlist() %>%
             unique()

  # Add info to grid
  berge <- grid1p %>% mutate(berge = 0)
  berge$berge[bergeID] <- 1
  # ------------------------------------------------------------------------- #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  save(berge, file = './data/cv_berge.RData')
  # ------------------------------------------------------------------------- #}
}
