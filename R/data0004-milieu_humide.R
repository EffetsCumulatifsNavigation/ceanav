#' Data 0004 : Milieux humides Lac St-Pierre
#'
#' Données de terrain pour la cartographie des milieux humides du lac Saint-Pierre (fleuve Saint-Laurent), 2012
#'
#' @keywords milieu humide
#' @keywords lac st-pierre
#' @keywords habitat
#' @keywords composante valorisée
#'
#' @source https://ouvert.canada.ca/data/fr/dataset/9c52df44-7a34-4a73-a92a-5e3b20de6c73
#'
#' @export
#'
#' @details Cette fonction télécharge et formatte les données
#'

get_data0004 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0004-milieu_humide/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'MilieuxHumides-lacSaintPierre-inventaire-2012.csv'))) {
    # URL
    milieu_humide_lsp <- c('http://donnees.ec.gc.ca/data/sites/scientificknowledge/field-data-for-the-mapping-of-the-lake-st.-pierre-wetlands-st.-lawrence-river-2012/MilieuxHumides-lacSaintPierre-inventaire-2012.csv',
                           'http://donnees.ec.gc.ca/data/sites/scientificknowledge/field-data-for-the-mapping-of-the-lake-st.-pierre-wetlands-st.-lawrence-river-2012/MilieuxHumides-lacSaintPierre-sites-2012.csv')

    # Download
    download.file(milieu_humide_lsp[1], destfile = paste0(folder, 'MilieuxHumides-lacSaintPierre-inventaire-2012.csv'))
    download.file(milieu_humide_lsp[2], destfile = paste0(folder, 'MilieuxHumides-lacSaintPierre-sites-2012.csv'))
  }
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  # Milieux humides lac Saint-Pierre
  data0004 <- read.csv(paste0(folder, 'MilieuxHumides-lacSaintPierre-sites-2012.csv')) %>%
              filter(Type.de.milieux %in% c('Bas marais','Haut marais','Marécage arbustif','Marécage arboré','Eau peu profonde')) %>%
              st_as_sf(coords = c("Longitude","Latitude"), crs = 4326)
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0004,
           dsn = "./data/data-format/data0004-milieu_humide.geojson",
           delete_dsn = TRUE)

  # RData
  save(data0004, file = "./data/data0004.RData")
  # _________________________________________________________________________ #
}
