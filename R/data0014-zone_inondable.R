#' Data 0014 : Zones inondables BDZI
#'
#' Base de données des zones à risque d'inondation (BDZI)
#'
#' @keywords zone inondable
#' @keywords habitat
#' @keywords composante valorisée
#'
#' @source https://www.donneesquebec.ca/recherche/fr/dataset/3ac8ddff-fe0a-4a7a-8393-d5938e8f35e5
#'
#' @export
#'
#' @details Cette fonction télécharge et formatte les données
#'

get_data0014 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # WARNING: La limite des hautes eaux dans le Lac Saint-Pierre est disponible
  #          dans ce jeu de données
  # WARNING: Les polygones non-simplifiés (`ZOI_s`) sont énormes et la
  #          résolution est assurément supérieure à ce dont on a besoin

  # Output folder
  output <- "data0014-zone_inondable/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'Struc_physique_20BDZI_V3.0.pdf'))) {
    # URL
    zone_inondable_bdzi <- c('ftp://ftp.mddelcc.gouv.qc.ca/DONNEES_OUVERTES/Base_donnees_zones_inondables/Struc_physique_%20BDZI_V3.0.pdf',
                             'ftp://ftp.mddelcc.gouv.qc.ca/DONNEES_OUVERTES/Base_donnees_zones_inondables/BDZI.gdb.zip')

    # Download
    # Default R options limit download time to 60 seconds. Modify for larger files
    oldopt <- options()$timeout
    options(timeout=500)
    download.file(zone_inondable_bdzi[1], destfile = paste0(folder, 'Struc_physique_20BDZI_V3.0.pdf'))
    download.file(zone_inondable_bdzi[2], destfile = paste0(folder, 'BDZI.gdb.zip'))
    options(timeout=oldopt)

    # Unzip
    unzip(zipfile = paste0(folder, 'BDZI.gdb.zip'), exdir = folder)
  }

  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  data0014 <- st_read(paste0(folder, 'BDZI.gdb'), layer = 'ZOI_s_Simplify')
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0014,
           dsn = "./data/data-format/data0014-zone_inondable.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #
}
