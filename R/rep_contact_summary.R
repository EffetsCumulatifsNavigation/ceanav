#' Tableau récapitulatif des personnes et organisations ressources
#'
#' Fonction utilisée pour générer un tableau récapitulatif des personnes et organisations ressources pour le projet.
#'
#' @keywords tableau récapitulatif
#'
#' @export
#'
#' @details Cette fonction permet de générer le tableau récapitulatif des personnes et organisations ressources pour le projet
#'
#' @examples
#' rep_contact_summary()

rep_contact_summary <- function() {
  # Data and libraries
  data(data_contact)
  library(knitr)
  library(kableExtra)

  # Select only relevant fields
  dat <- contact %>%
         # Sélectionner et renommer les colonnes à intérer au tableau
         select(Contact_id = id,
                Prénom = prenom,
                Nom = nom,
                Organisation = organisation,
                Courriel = courriel)

  # Export table
  options(knitr.kable.NA = '')
  kable(dat, row.names = FALSE)
}
