#' Fiches descriptives des données
#'
#' Fonction utilisée pour générer une fiches descriptive pour chaque données incluse au tableau récapitulatif des données à l'Annexe 1
#'
#' @param data_id `character` id of data to import in R session with format `dataXXXX`
#' @param output_folder `character` output folder for exported document
#' @param suffix `character` if provided, add suffix before data name
#'
#' @keywords fiche descriptive
#'
#' @rdname rep_annexe_data_description
#'
#' @export
#'
#' @details Cette fonction permet de générer une fiche descriptive pour la base de données sélectionnée
#'
#' @examples
#' rep_annexe_data_description("data0001","report/contenu/annexes/","annexe2-")

rep_annexe_data_description <- function(data_id, output_folder, suffix = NULL) {
  # Data and libraries
  load_metadata(data_id)
  load_contact(data_id)

  # -------
  out <- list()
  # out$dat <- get(data_id)
  out$metadata <- get(paste0("metadata_", data_id))
  out$contact <- get(paste0("contact_", data_id))

  # -------
  use_template(
    template = "templates/fiche_descriptive.Rmd",
    data = out,
    save_as = glue("{output_folder}{suffix}{data_id}.Rmd")
  )
}

#' @rdname rep_annexe_data_description
#' @aliases rep_annexe_data_description_all
#' @export
rep_annexe_data_description_all <- function(output_folder, suffix) {
  dataname <- dir("./data/data-metadata/", pattern = ".yml") %>%
              gsub(".yml","",.)

  uid <- str_detect(dataname, "data")
  dataname <- dataname[uid]

  for(i in dataname) {
    rep_annexe_data_description(
      data_id = i,
      output_folder = output_folder,
      suffix = suffix
    )
  }
}
