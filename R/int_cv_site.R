#' Sites d’intérêt culturels, patrimoniaux et archéologiques
#'
#' Intégration des données utilisées pour caractériser la composante valorisée des sites d’intérêt culturels, patrimoniaux et archéologiques
#'
#' @keywords sites d’intérêt culturels, patrimoniaux et archéologiques
#' @keywords composante valorisée
#'
#' @export
#'
#' @details Cette fonction télécharge et formatte les données
#'

cv_site <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Notes
  # ------------------------------------
  #
  # Plusieurs sites ont été identifiés à travers ce projet grâce aux données
  # ouvertement disponibles ainsi qu'aux collaborations développées avec les
  # Premières Nations. Il est toutefois important de mentionner que ces sites
  # sont représentatifs des objectifs de notre étude, des collaborations
  # développées et des personnes impliquées dans le projet. Ces sites ne
  # devraient donc pas être perçus comme une liste exhaustive de l'ensemble des
  # sites d'importance dans la région d'étude. De plus, bien que les représentants
  # des Premières Nations ont identifiés des sites ayant une importance
  # particulière pour leurs communautés, ça ne veut pas dire que les milieux
  # qui ne sont pas identifiés n'ont aucune importance et qu'ils renoncent à
  # ces territoires.
  #
  # Il est clair que d'autres sites pourraient être ajoutés à la liste actuelle.
  # La liste des sites considérés actuellement est la suivante:
  #
  #   - AGHAMM - pêche commerciale : 0022
  #   - AGHAMM - pêche au saumon Atlantique : 0023
  #   - Essipit - Culture et patrimoine : 0024
  #   - Essipit - Pêche traditionnelle : 0024
  #   - Essipit - Chasse oiseaux migrateurs : 0024
  #   - Essipit - Chasse phoque : 0024
  #   - Essipit - Pêche commerciale : 0024
  #   - Essipit - Activités touristiques : 0024
  #   - Essipit - Accès au plan d'eau : 0024
  #   - Wolastoqiyik Wahsipekuk - Pêche commerciale : 0025, 0026
  #   - Kahnawake - Cultural Sites : 0032
  #   - Kahnawake - Fishing Commercial : 0032
  #   - Kahnawake - Hunting : 0032
  #   - Kahnawake - Near Shore Fishing : 0032
  #   - Kahnawake - Open Water Fishing : 0032
  #   - Kahnawake - SSSM : 0032
  #   - Kahnawake - Traffic : 0032
  #   - Kahnawake - Vegetation : 0032
  #   - Milieux protégés : 0038, 0039, 0040
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # ------------------------------------------------------
  uid <- function(...) {
    # -----
    uid <- bind_rows(...) %>%
           st_intersects(grid1p) %>%
           unlist() %>%
           unique()

    # -----
    dat <- numeric(nrow(grid1p))
    dat[uid] <- 1

    # -----
    dat
  }



  # ------------------------------------------------------
  meta <- load_metadata("int_cv_site")
  meta <- metadata_int_cv_habitat
  meta$rawData <- NULL

  # ------------------------------------------------------
  data(grid1p)
  habitat <- grid1p

  # ------------------------------------------------------
  # Zostères : 0001, 0002, 0003
  meta$rawData <- c(meta$rawData, "0001", "0002", "0003")

  # -----
  load_format("data0001")
  load_format("data0002")
  load_format("data0003")
}
