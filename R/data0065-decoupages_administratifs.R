#' Data 0065 : Découpages administratifs
#'
#' Les couches de découpages administratifs offertes aux échelles de 1/20 000 et 1/100 000 permettent la localisation des limites pour les composantes suivantes : 1) Les arrondissements et agglomérations; 2) Les municipalités, territoires non organisés et territoires autochtones; 3)Les régions administratives, communautés métropolitaines, et MRC; 4)La frontière internationale, les frontières interprovinciales ainsi que la frontière Québec–Terre-Neuve-et-Labrador.
#'
#' @keywords découpages administratifs
#'
#' @source https://www.donneesquebec.ca/recherche/dataset/decoupages-administratifs
#'
#' @export
#'
#' @details This function loads and formats the data
#'

get_data0065 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0065-decoupages_administratifs/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'Structure_physique_SDA.pdf'))) {
    # URL
    dat <- c('https://diffusion.mern.gouv.qc.ca/Diffusion/RGQ/Documentation/SDA/Structure_physique_SDA.pdf',
             'https://diffusion.mern.gouv.qc.ca/Diffusion/RGQ/Documentation/SDA/Historique_des_modifications.pdf',
             'https://diffusion.mern.gouv.qc.ca/Diffusion/RGQ/Vectoriel/Theme/Local/SDA_20k/FGDB/SDA.gdb.zip')

    # Download
    download.file(dat[1], destfile = paste0(folder, 'Structure_physique_SDA.pdf'))
    download.file(dat[2], destfile = paste0(folder, 'Historique_des_modifications.pdf'))
    download.file(dat[3], destfile = paste0(folder, 'SDA.gdb.zip'))

    # Unzip
    unzip(zipfile = paste0(folder, 'SDA.gdb.zip'), exdir = folder)
  }
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  data0065 <- st_read(paste0(folder, '/SDA.gdb'),
                      layer = "regio_s", quiet = TRUE) %>%
              st_transform(crs = global_parameters()$crs)
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0065,
           dsn = "./data/data-format/data0065-decoupages_administratifs.geojson",
           delete_dsn = TRUE,
           quiet = TRUE)
  # _________________________________________________________________________ #

}
