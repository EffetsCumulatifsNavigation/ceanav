#' Hyperlien à partir de deux vecteurs
#'
#' Création d'un hyperlien en format markdown à partir de deux vecteurs
#'
#' @param texte `character`, texte à utiliser pour créer l'hyperlien
#' @param url `character`, hyperlien vers la ressource externe
#'
#' @keywords hyperlien
#'
#' @export
#'
#' @details Cette fonction permet de générer le tableau récapitulatif des données utilisées pour le rapport
#'
#' @examples
#' texte <- "Rapport"
#' url <- "https://github.com/EffetsCumulatifsNavigation/ceanav"
#' rep_hyperlien(texte, url)

rep_hyperlien <- function(texte, url) {
  nl <- length(texte)
  hyperlien <- character(nl)

  for(i in 1:nl) {
    if(!is.na(url[i])) {
      hyperlien[i] <- paste0("[",texte[i],"](",url[i],")")
    } else {
      hyperlien[i] <- texte[i]
    }
  }

  # Return
  hyperlien
}
