#' Data 0071 : Mouillages et zones de mouillages canadiennes
#'
#' Mouillages et zones de mouillages canadiennes est composés de quatre fichiers qui comprennent des zones de mouillage (ACHARE) et des mouillages à un seul navire (ACHBRT). Les fichiers sont soit un avec des points et un avec des polygones pour chacun des deux types mouillages. Ces fichiers sont un ensemble complet de données sur les mouillages et les zones de mouillage dans les eaux navigables canadiennes. Cet ensemble de données sera mis à jour au besoin.
#'
#' @keywords ancrage
#' @keywords stresseurs
#'
#' @source 
#'
#' @export
#'
#' @details Cette fonction télécharge et formatte les données
#'

get_data0071 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0071-ancrage_shc/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'Chart1-Carte1.pdf'))) {
    # URL
    dat <- c('http://chs-shc.gc.ca/documents/publications/Chart1-Carte1.pdf',
             'https://pacgis01.dfo-mpo.gc.ca/FGPPublic/Canadian_Anchorages_and_Anchorage_Areas/CanadianAnchoragesAndAnchorageAreas.zip')

    # Download
    download.file(dat[1], destfile = paste0(folder, 'Chart1-Carte1.pdf'))
    download.file(dat[2], destfile = paste0(folder, 'CanadianAnchoragesAndAnchorageAreas.zip'))
    
    # Unzip
    unzip(zipfile = paste0(folder, 'CanadianAnchoragesAndAnchorageAreas.zip'), exdir = folder)
  }
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  achare_a <- st_read(paste0(folder, "ACHARE_A.shp"))
  achare_p <- st_read(paste0(folder, "ACHARE_P.shp"))
  achbrt_a <- st_read(paste0(folder, "ACHBRT_A.shp"))
  achbrt_p <- st_read(paste0(folder, "ACHBRT_P.shp"))
  
  # Single object with buffers for points
  data0071 <- bind_rows(achare_a, achbrt_a, 
                        st_buffer(achare_p, 100),
                        st_buffer(achbrt_p, 100)) %>%
              st_transform(crs = global_parameters()$crs)
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0071,
           dsn = "./data/data-format/data0071-ancrage_shc.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #
}
