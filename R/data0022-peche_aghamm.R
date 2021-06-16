#' Data 0022 : Pêches commerciales AGHAMM
#'
#' Mi'gmaq and Maliseet commercial fisheries of the communities of Gesgapegiag, Gespeg and Viger
#'
#' @keywords pêche commerciale
#' @keywords sites d'importance
#' @keywords composante valorisée
#'
#' @source https://catalogue.ogsl.ca/en/dataset/bbe35312-987a-42d7-ae2f-41c1a0715dc3
#'
#' @export
#'
#' @details Cette fonction télécharge et formatte les données
#'

get_data0022 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  #
  # https://catalogue.ogsl.ca/en/dataset/bbe35312-987a-42d7-ae2f-41c1a0715dc3
  #
  # Racine, M.-J. and Arsenault, L.M. 2017. Commercial Fisheries Data for the
  # communities of Gesgapegiag, Gespeg and Viger. MMAFMA. Data published on St.
  # Lawrence Global Observatory-SLGO. [https://slgo.ca]. Access date:
  # [YYYY-MM-DD].
  #
  # Report:
  # Use report source identification + " Published on St. Lawrence Global
  # Observatory-SLGO. [https://slgo.ca]. Access date: [YYYY-MM-DD]."
  #
  # Atlas:
  # Arsenault, L.M. Racine, M.-J. and Lambert Koizumi, C. (2017) Atlas of Marine
  # St. Lawrence Mi’gmaq and Maliseet Sites and Their Uses by the Gesgapegiag,
  # Gespeg and Viger Communities. Mi’gmaq Maliseet Aboriginal Fisheries Management
  # Association (MMAFMA), 46 p.

  # Output folder
  output <- "data0022-peche_aghamm/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'atlas-halieutique_fr.pdf'))) {
    # URL
    peche_aghamm <- c('https://catalogue.ogsl.ca/data/aghamm/bbe35312-987a-42d7-ae2f-41c1a0715dc3/atlas-halieutique_fr.pdf',
                      'https://catalogue.ogsl.ca/data/aghamm/bbe35312-987a-42d7-ae2f-41c1a0715dc3/atlas-halieutique_en.pdf',
                      'https://catalogue.ogsl.ca/data/aghamm/bbe35312-987a-42d7-ae2f-41c1a0715dc3/peches_commerciales.gdb.zip',
                      'https://catalogue.ogsl.ca/data/aghamm/bbe35312-987a-42d7-ae2f-41c1a0715dc3/heatmap.tif',
                      'https://catalogue.ogsl.ca/data/aghamm/bbe35312-987a-42d7-ae2f-41c1a0715dc3/descriptionchamps-peches_commerciales.xlsx')

    # Download
    download.file(peche_aghamm[1], destfile = paste0(folder, 'atlas-halieutique_fr.pdf'))
    download.file(peche_aghamm[2], destfile = paste0(folder, 'atlas-halieutique_en.pdf'))
    download.file(peche_aghamm[3], destfile = paste0(folder, 'peches_commerciales.gdb.zip'))
    download.file(peche_aghamm[4], destfile = paste0(folder, 'heatmap.tif'))
    download.file(peche_aghamm[5], destfile = paste0(folder, 'descriptionchamps-peches_commerciales.xlsx'))

    # Unzip
    unzip(zipfile = paste0(folder, 'peches_commerciales.gdb.zip'), exdir = folder)
  }
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import and format data
  # ----------------------------------------
  # Pêches commerciales AGHAMM
  ## WARNING:
  ## Sélectionner uniquement le sommaire des pêches commerciales et sous-diviser par la suite.
  ## Les autres données du geodatabase contiennent parfois des informations sur la biomasse,
  ## mais je ne crois pas que nous les utiliserons, à moins qu'il devienne évident que nous devrions
  ## utiliser les biomasses pour discriminer l'importance relative des sites entre eux.
  message("Sélectionner uniquement le sommaire des pêches commerciales et sous-diviser par la suite. Les autres données du geodatabase contiennent parfois des informations sur la biomasse, mais je ne crois pas que nous les utiliserons, à moins qu'il devienne évident que nous devrions utiliser les biomasses pour discriminer l'importance relative des sites entre eux.")

  # # Pour tout importer:
  # uid <- st_layers(paste0(folder, 'Peches_commerciales.gdb'))
  # peche_aghamm <- list()
  # for(i in 1:length(uid$name)) {
  #   peche_aghamm[[i]] <- st_read(paste0(folder, 'Peches_commerciales.gdb'),
  #                                layer = uid$name[i], quiet = TRUE)
  # }
  # names(peche_aghamm) <- uid$name

  # Sommaire des pêches
  data0022 <- st_read(paste0(folder, 'Peches_commerciales.gdb'),
                      layer = "Sommaire_peches_commerciales", quiet = TRUE)
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0022,
           dsn = "./data/data-format/data0022-peche_aghamm.geojson",
           delete_dsn = TRUE)

  # RData
  save(data0022, file = "./data/data0022.RData")
  # _________________________________________________________________________ #
}
