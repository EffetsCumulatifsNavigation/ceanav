#' Data 0043 : Computerized Database of Québec Seabirds (CDQS)
#'
#' The Computerized Database of Quebec Seabirds (CDQS) was set and has been updated by the Canadian Wildlife Service (CWS), (Quebec region,) for over 20 years. The CDSQ contains information on seabird colonies part of the following species: Gaviidae (loons), Hydrobatidae (petrels), Sulidae (gannets/boobies), Phalacrocoracidae (cormorants), Laridae (seagulls, gulls, terns), Auks (guillemots, penguins, puffins) aud Anatidae.
#'
#' @keywords colonies d'oiseaux
#' @keywords habitats
#' @keywords composantes valorisées
#'
#' @source
#'
#' @export
#'
#' @details This function loads and formats the data
#'

get_data0043 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0043-colonie_oiseaux_cdqs/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'biomq_2018.csv'))) {
    # URL
    dat <- c('https://catalogue.ogsl.ca/data/ec/b04cc62f-8774-4978-9a6f-6beae584d6b5/biomq_2018.csv',
             'https://catalogue.ogsl.ca/data/ec/b04cc62f-8774-4978-9a6f-6beae584d6b5/dictionnaire_donnees.docx',
             'https://catalogue.ogsl.ca/data/ec/b04cc62f-8774-4978-9a6f-6beae584d6b5/rail_et_chapdelaine_2002.pdf',
             'https://catalogue.ogsl.ca/data/ec/b04cc62f-8774-4978-9a6f-6beae584d6b5/biomq18_polygones_shp.zip')

    # Download
    download.file(dat[1], destfile = paste0(folder, 'biomq_2018.csv'))
    download.file(dat[2], destfile = paste0(folder, 'dictionnaire_donnees.docx'))
    download.file(dat[3], destfile = paste0(folder, 'rail_et_chapdelaine_2002.pdf'))
    download.file(dat[4], destfile = paste0(folder, 'biomq18_polygones_shp.zip'))

    # Unzip
    unzip(zipfile = paste0(folder, 'biomq18_polygones_shp.zip'), exdir = folder)
  }
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  data0043 <- st_read(paste0(folder, 'BIOMQ18_polygones_shp/BIOMQ_Polygones_2018.shp'), quiet = TRUE) %>%
              st_transform(crs = global_parameters()$crs)
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0043,
           dsn = "./data/data-format/data0043-colonie_oiseaux_cdqs.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #

}
