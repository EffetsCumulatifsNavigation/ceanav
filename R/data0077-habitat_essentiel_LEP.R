#' Data 0077 : L'habitat essentiel désigné en vertu de la Loi sur les espèces en péril
#'
#' Le Programme des espèces en péril consiste à réaliser le mandat du MPO en vertu de la Loi sur les espèces en péril (LEP) afin d’assurer la protection, le rétablissement et la conservation de toutes les espèces aquatiques en péril inscrites au Canada.
#'
#' @keywords frayères
#'
#' @source https://ouvert.canada.ca/data/fr/dataset/db177a8c-5d7d-49eb-8290-31e6a45d786c
#'
#' @export
#'
#' @details This function loads and formats the data
#'

get_data0077 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0077-habitat_essentiel_LEP/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'CriticalHabitat_HabitatEssentiel.zip'))) {
    # URL
    dat <- c(
      'https://pacgis01.dfo-mpo.gc.ca/FGPPublic/CriticalHabitat/Metadata_DFO_CriticalHabitat_EN.pdf',
      "https://pacgis01.dfo-mpo.gc.ca/FGPPublic/CriticalHabitat/Metadonnees_MPO_LHabitatEssentiel_FR.pdf",
      "https://dfogis.azureedge.net/CriticalHabitat_HabitatEssentiel.zip"
    )

    # Download
    download.file(dat[1], destfile = paste0(folder, 'Metadata_DFO_CriticalHabitat_EN.pdf'))
    download.file(dat[2], destfile = paste0(folder, 'Metadonnees_MPO_LHabitatEssentiel_FR.pdf'))
    download.file(dat[3], destfile = paste0(folder, 'CriticalHabitat_HabitatEssentiel.zip'))

    # Unzip
    unzip(zipfile = paste0(folder, 'CriticalHabitat_HabitatEssentiel.zip'), exdir = folder)
  }
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  data0077 <- st_read(
    paste0(folder, 'CriticalHabitat_FGP.gdb'), 
    layer = "DFO_SARA_CritHab_2022_FGP_EN", 
    quiet = TRUE
  ) %>%
  st_transform(crs = global_parameters()$crs)
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0077,
           dsn = "./data/data-format/data0077-habitat_essentiel_LEP.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #

}
