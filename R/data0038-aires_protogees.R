#' Data 0040 : Registre des aires protégées au Québec
#'
#' Ces thématiques présentent la compilation des aires protégées au Québec.
#'
#' @keywords aire protégée
#'
#' @source https://www.donneesquebec.ca/recherche/dataset/aires-protegees-au-quebec
#'
#' @export
#'
#' @details This function loads and formats the data
#'

get_data0038 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0038-aire_protegee/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'registre_aires_prot.zip'))) {
    # URL
    dat <- c('ftp://ftp.mddelcc.gouv.qc.ca/DONNEES_OUVERTES/Registre_aires_protegees/registre_aires_prot.zip',
             'ftp://ftp.mddelcc.gouv.qc.ca/DONNEES_OUVERTES/Registre_aires_protegees/AP_structures_donnees.pdf',
             'ftp://ftp.mddelcc.gouv.qc.ca/DONNEES_OUVERTES/Registre_aires_protegees/LISEZ_MOI.pdf')

    # Download
    download.file(dat[1], destfile = paste0(folder, 'registre_aires_prot.zip'))
    download.file(dat[2], destfile = paste0(folder, 'AP_structures_donnees.pdf'))
    download.file(dat[3], destfile = paste0(folder, 'LISEZ_MOI.pdf'))

    # Unzip
    unzip(zipfile = paste0(folder, 'registre_aires_prot.zip'), exdir = folder)
  }
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  data0038 <- st_read(paste0(folder, 'registre_aires_prot/AP_REG_S.shp'), quiet = TRUE) %>%
              st_transform(crs = global_parameters()$crs)
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0038,
           dsn = "./data/data-format/data0038-aire_protegee.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #

}
