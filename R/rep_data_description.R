#' Fiches descriptives des données
#'
#' Fonction utilisée pour générer une fiches descriptive pour chaque données incluse au tableau récapitulatif des données à l'Annexe 1
#'
#' @param data_id `character` id of data to import in R session with format `dataXXXX`
#'
#' @keywords fiche descriptive
#'
#' @export
#'
#' @details Cette fonction permet de générer une fiche descriptive pour la base de données sélectionnée
#'
#' @examples
#' rep_data_description("data0001")

rep_data_summary <- function(data_id) {
  # Data and libraries
  data_id <- "data0017"
  ceanav_load_all(data_id)
  library(knitr)
  library(kableExtra)

  # Single objects to work with
  dat <- get(data_id)
  metadata <- get(paste0("metadata_", data_id))
  contact <- get(paste0("contact_", data_id))

  # Print desired information

}
