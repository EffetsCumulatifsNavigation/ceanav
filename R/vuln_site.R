#' Vulnérabilité des sites d'intérêt culturels, patrimoniaux et archéologiques aux stresseurs environnementaux
#'
#' Création de la matrice de vulnérabilité pour les sites d'intérêt culturels, patrimoniaux et archéologiques considérés pour l'évaluation des effets cumulatifs
#'
#' @keywords vulnérabilité
#' @keywords sites d'intérêt culturels, patrimoniaux et archéologiques
#' @keywords composante valorisée
#'
#' @export
#'
#' @details Cette fonction génère la matrice de vulnérabilité pour les habitats
#'

vuln_site <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Matrix
  # ------------------------------------
  #
  # NOTE:
  #   L'identification des sites d'intérêt culturels, patrimoniaux et archéologiques
  #   a déjà fait appel à une priorisation des sites qui sont jugés importants pour
  #   les communautés locales et pour les Premières Nations. À cet égard, nous
  #   avons ainsi jugé, après discussion avec plusieurs représentants des communautés,
  #   qu'il serait inadéquat d'attribuer un score de vulnérabilité aux sites identifiés.
  #   En effet, attribuer une valeur de vulnérabilité serait l'équivalent d'attribuer
  #   une cote de priorisation des sites entre eux, un processus qui s'avèrerait
  #   nécessairement subjectif, surtout que plusieurs communautés différentes ont
  #   fourni des données de sites d'importance.
  #
  #   Nous attribuons ainsi une valeur de vulnérabilité unique égale à 1 pour
  #   l'ensemble des sites identifiés. Cette décision signifie également que les
  #   résultats pour les sites d'intérêt correspondent aux résultats de l'exposition
  #   cumulée (voir section @ref). Ce n'est toutefois pas le cas pour l'évaluation
  #   globale, puisque l'ensemble des sites reçoivent un score de vulnérabilité
  #   maximal, ce qui n'est pas le cas pour les autres composantes valorisées
  #   considérées au sein de cette évaluation.
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Stressors for this project
  st <- read.csv("./data/data-metadata/metadata_stresseurs.csv")

  # Habitats for this project
  sites <- read.csv("./data/data-metadata/metadata_composantes_valorisees.csv") %>%
           filter(comp_val == "site")
  # ------------------------------------------------------------------------- #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Vulnerability matrix
  # ----------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  vulnerability_site <- matrix(data = 1,
                               nrow = nrow(st),
                               ncol = nrow(sites),
                               dimnames = list(st$accronyme,
                                               sites$accronyme))
  # _____________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # -----
  write.csv(vulnerability_site,
            "./data/data-vulnerability/vulnerability_site.csv",
            row.names = TRUE)
  # ------------------------------------------------------------------------- #}

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Clean global environment
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  clean()
  # ------------------------------------------------------------------------- #}
}
