#' Data 0042 : Territoires d’intérêt pour la conservation - milieux humides
#'
#' Atlas des territoires d’intérêt pour la conservation dans les Basses-terres du Saint-Laurent
#'
#' @keywords milieux humides
#' @keywords habitats
#' @keywords composante valorisée
#'
#' @source https://www.donneesquebec.ca/recherche/dataset/atlas-des-territoires-interet-conservation-btsl#
#'
#' @export
#'
#' @details Cette fonction télécharge et formatte les données
#'

get_data0042 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0042-territoire_btsl/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'Description_donnees.pdf'))) {
    # URL
    data0042 <- c('ftp://ftp.mddelcc.gouv.qc.ca/DONNEES_OUVERTES/Atlas_territoires_interet_conservation_BTSL/Description_donnees.pdf',
                  'ftp://ftp.mddelcc.gouv.qc.ca/DONNEES_OUVERTES/Atlas_territoires_interet_conservation_BTSL/Atlas_BTSL_RapportMethodologique_juin2019.pdf',
                  'ftp://ftp.mddelcc.gouv.qc.ca/DONNEES_OUVERTES/Atlas_territoires_interet_conservation_BTSL/Territoires_interet_BTSL_Juin2019.gdb.zip')

    # Download
    # Default R options limit download time to 60 seconds. Modify for larger files
    oldopt <- options()$timeout
    options(timeout=500)
    download.file(data0042[1], destfile = paste0(folder, 'Description_donnees.pdf'))
    download.file(data0042[2], destfile = paste0(folder, 'Atlas_BTSL_RapportMethodologique_juin2019.pdf'))
    download.file(data0042[3], destfile = paste0(folder, 'Territoires_interet_BTSL_Juin2019.gdb.zip'))
    options(timeout=oldopt)

    # Unzip
    unzip(zipfile = paste0(folder, 'Territoires_interet_BTSL_Juin2019.gdb.zip'), exdir = folder)
  }
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import and format data
  # ----------------------------------------
  # x <- st_layers(paste0(folder,"Territoires_interet_BTSL_Juin2019.gdb"))
  data0042 <- st_read(paste0(folder, 'Territoires_interet_BTSL_Juin2019.gdb'),
                      layer = "MH_Complexes") %>%
              st_transform(crs = global_parameters()$crs) %>%
              st_zm() %>%
              st_geometry() %>%
              st_cast("MULTIPOLYGON") %>%
              st_sf()

  # WARNING:
  # "The dataset is very big and covers a much broader area than what we need
  # for this project. I will therefore only select polygons that intersect with
  # our study area. Simply rqemove the code that intersects with our study area
  # to get the whole dataset"
  message("The dataset is very big and covers a much broader area than what we need for this project.
  I will therefore only select polygons that intersect with our study area. Simply remove the code that
  intersects with our study area to get the whole dataset")

  # Study area grid
  data(grid1p)
  aoi <- st_transform(aoi, st_crs(data0042))

  # Identify polygons intersecting study area
  uid <- st_intersects(aoi, data0042) %>%
         unlist() %>%
         unique() %>%
         sort()

  # Select polygons intersecting study area
  data0042 <- data0042[uid, ]
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0042,
           dsn = "./data/data-format/data0042-territoire_btsl.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #
}
