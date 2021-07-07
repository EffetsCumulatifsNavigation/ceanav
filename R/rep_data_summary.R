#' Tableau récapitulatif des données
#'
#' Fonction utilisée pour générer un tableau récapitulatif des données présentées dans le rapport.
#'
#' @keywords tableau récapitulatif
#'
#' @export
#'
#' @details Cette fonction permet de générer le tableau récapitulatif des données utilisées pour le rapport
#'
#' @examples
#' rep_data_summary()

rep_data_summary <- function() {
  # Data and libraries
  data(data_metadata)
  library(knitr)
  library(kableExtra)

  # Select only relevant fields
  dat <- metadata %>%
         # Ajouter et modifier colonnes
         mutate(Citation = "TO DO",
                Nom = rep_hyperlien(data_description.name,
                                    data_description.url)) %>%

         # Sélectionner et renommer les colonnes à intérer au tableau
         select(Données_id = data_description.id,
                Nom,
                Citation,
                Disponibilité = data_description.availability) %>%

         # Retirer les doublons
         unique()

  # Export table
  kable(dat, row.names = FALSE)
}
