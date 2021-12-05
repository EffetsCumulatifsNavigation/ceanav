#' Generate metadata for stressors
#'
#'
#' @keywords metadata
#' @keywords stresseurs
#'
#' @export
#'
#' @details These functions take '.yml' files in specified folder to generate an individual table containing information from all `.yml` files
#'


metadata_st_cv <- function() {
  # Folder
  folder <- './data/data-metadata/'

  # Stresseurs et composantes valorisees
  st <- global_parameters()$stresseurs
  cv <- global_parameters()$composantes_valorisees

  # Metadata stresseurs
  stresseurs <- list()
  for(i in st) {
    meta <- load_metadata(glue("int_st_{i}"))
    stresseurs[[i]] <- meta$dataDescription$categories %>%
                       as.data.frame() %>%
                       mutate(title = meta$title,
                              stresseur = i) %>%
                       select(stresseur, accronyme, title, francais)#, description)
  }
  stresseurs <- bind_rows(stresseurs)

  # Metadata composantes valorisées
  composantes_valorisees <- list()
  for(i in cv) {
    meta <- load_metadata(glue("int_cv_{i}"))
    composantes_valorisees[[i]] <- meta$dataDescription$categories %>%
                       as.data.frame() %>%
                       mutate(title = meta$title,
                              comp_val = i) %>%
                       select(comp_val, accronyme, title, type, francais)#, description)
  }
  composantes_valorisees <- bind_rows(composantes_valorisees)

  # Add column with simplified version of names for sites
  # WARNING: This is manual, is thus not reproducible per say
  cv <- composantes_valorisees
  cv$simple <- cv$francais
  cv$simple <- gsub("Naturelle - Semi-végétalisée (IE = 1)", "Semi-végétalisée", cv$simple)
  cv$simple <- gsub("Naturelle - Vive (IE = 2)", "Vive", cv$simple)
  cv$simple <- gsub("Artificielle - Semi-végétalisée (IE = 1)", "Semi-végétalisée", cv$simple)
  cv$simple <- gsub("Artificielle - Vive (IE = 2)","Vive", cv$simple)
  cv$simple <- gsub("AGHAMM - ", "", cv$simple)
  cv$simple <- gsub("Essipit - ", "", cv$simple)
  cv$simple <- gsub("Wolastoqiyik Wahsipekuk - ", "", cv$simple)
  cv$simple <- gsub("Kahwanake - ", "", cv$simple)
  cv$simple <- gsub("Public - ", "", cv$simple)
  cv$simple <- gsub("Nation Huronne-Wendat - ", "", cv$simple)
  composantes_valorisees <- cv
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  write.csv(stresseurs, file = './data/data-metadata/metadata_stresseurs.csv', row.names = FALSE)
  write.csv(composantes_valorisees, file = './data/data-metadata/metadata_composantes_valorisees.csv', row.names = FALSE)
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
}
