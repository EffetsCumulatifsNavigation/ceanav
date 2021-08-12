#' Data 0037 : Habitats floristiques
#'
#' Un habitat floristique est une aire protégée qui abrite au moins une espèce floristique désignée menacée ou vulnérable et qui est identifié à l’article 7 du Règlement sur les espèces floristiques menacées ou vulnérables et leurs habitats.
#'
#' @keywords habitats floristiques
#' @keywords composantes valorisées
#'
#' @source https://www.donneesquebec.ca/recherche/dataset/habitats-floristiques
#'
#' @export
#'
#' @details This function loads and formats the data
#'

get_data0037 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0037-habitat_floristique/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'Habitats_floristiques.zip'))) {
    # URL
    dat <- c('ftp://ftp.mddelcc.gouv.qc.ca/DONNEES_OUVERTES/Habitats_floristiques/Habitats_floristiques.zip',
             'ftp://ftp.mddelcc.gouv.qc.ca/DONNEES_OUVERTES/Habitats_floristiques/habitats_floristiques.pdf')

    # Download
    download.file(dat[1], destfile = paste0(folder, 'Habitats_floristiques.zip'))
    download.file(dat[2], destfile = paste0(folder, 'Habitats_floristiques.pdf'))

    # Unzip
    unzip(zipfile = paste0(folder, 'Habitats_floristiques.zip'), exdir = folder)
  }
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  data0037 <- st_read(paste0(folder, 'Habitats_floristiques/Habitats_floristiques.shp'), quiet = TRUE) %>%
              st_transform(crs = global_parameters()$crs)
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0037,
           dsn = "./data/data-format/data0037-habitat_floristique.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #

}
