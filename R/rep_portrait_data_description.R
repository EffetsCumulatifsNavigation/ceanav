#' Fiches descriptives des données intégrés
#'
#' Fonction utilisée pour générer une fiches descriptive pour chaque catégorie des données intégrées pour les stresseurs environnementaux et les composantes valorisées
#'
#' @param data_id `character` id of data to import in R session
#' @param output_folder `character` output folder for exported document
#' @param suffix `character` if provided, add suffix before data name
#'
#' @keywords fiche descriptive
#'
#' @rdname rep_portrait_data_description
#'
#' @export
#'
#' @details Cette fonction permet de générer une fiche descriptive pour la base de données sélectionnée
#'
#' @examples
#' rep_portrait_data_description("peche_commerciale","report/contenu/5-portrait/1-stresseurs")

rep_portrait_data_description <- function(data_id, output_folder) {
  # Proper names
  # TODO: eventually simply this... it's not ideal...
  suffix <- data_id
  st <- c("ancrage","deversement","dragage","naufrage","navigation","peche_commerciale","rejet")
  cv <- c("berge","habitat","mammiferes_marins","site")
  if (data_id %in% st) data_id <- glue("int_st_{data_id}")
  if (data_id %in% cv) data_id <- glue("int_cv_{data_id}")

  # Data and libraries
  meta <- load_metadata(data_id)
  # load_contact(data_id)

  # -------
  nm <- meta$dataDescription$categories$accronyme
  for(i in nm) {
    # -------
    uid <- meta$dataDescription$categories$accronyme == i

    # -------
    out <- list()
    out$metadata <- meta
    out$metadata$dataDescription$categories$accronyme <- out$metadata$dataDescription$categories$accronyme[uid]
    out$metadata$dataDescription$categories$francais <- out$metadata$dataDescription$categories$francais[uid]
    out$metadata$dataDescription$categories$english <- out$metadata$dataDescription$categories$english[uid]
    out$metadata$dataDescription$categories$mdref <- out$metadata$dataDescription$categories$mdref[uid]

    # out$contact <- get(paste0("contact_", data_id))

    use_template(
      template = "templates/fiche_integrated.Rmd",
      data = out,
      save_as = glue("{output_folder}{suffix}-{i}.Rmd")
    )
  }
}

#' @rdname rep_portrait_data_description
#' @aliases rep_portrait_data_description_all
#' @export
rep_portrait_data_description_all <- function(output_folder, suffix) {
  dataname <- dir("./data/data-metadata/", pattern = ".yml") %>%
              gsub(".yml","",.)

  uid <- str_detect(dataname, "int")
  dataname <- dataname[uid]

  for(i in dataname) {
    rep_annexe_data_description(
      data_id = i,
      output_folder = output_folder,
      suffix = suffix
    )
  }
}
