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
#' rep_annexe_contact()

rep_annexe_contact <- function(lang = "fr") {
  # Data and libraries
  data(data_contact)
  library(knitr)
  library(kableExtra)

    # Select only relevant fields
    dat <- contact %>%
           select(id, prenom, nom, organisation, courriel)    

  # Export table
  options(knitr.kable.NA = '')
  if (lang == "fr") {
    kable(dat, row.names = FALSE, col.names = c("ID","Prénom","Nom","Organisation","Courriel"))
  } else if (lang == "en") {
    kable(dat, row.names = FALSE, col.names = c("ID","First name","Last name","Organization","Mail"))
  }

}
