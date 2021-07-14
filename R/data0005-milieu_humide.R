#' Data 0005 : Milieux humides cartographie détaillée
#'
#' Cartographie détaillée des milieux humides pour les basses-terres du Saint-Laurent, la plaine du Lac Saint-Jean et d'autres secteurs au sud du Québec
#'
#' @keywords milieu humide
#' @keywords habitat
#' @keywords composante valorisée
#'
#' @source https://www.donneesquebec.ca/recherche/dataset/eafec419-d67d-449e-a157-d22230314d36
#'
#' @export
#'
#' @details Cette fonction télécharge et formatte les données
#'

get_data0005 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0005-milieu_humide/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'metadonneesmh.pdf'))) {
    # URL
    data0005 <- c('https://www.donneesquebec.ca/recherche/dataset/eafec419-d67d-449e-a157-d22230314d36/resource/089ac31a-4b49-41e0-b334-2525b1dcfb66/download/mhsudduqc2020.zip',
                  'https://www.donneesquebec.ca/recherche/dataset/eafec419-d67d-449e-a157-d22230314d36/resource/a4d4078a-005f-4eda-904a-f98c635ee4f9/download/classesdemilieuxhumides.pdf',
                  'https://www.donneesquebec.ca/recherche/dataset/eafec419-d67d-449e-a157-d22230314d36/resource/36622527-c0f5-4af1-9e3a-aa149d311b7f/download/metadonneesmh.pdf')

    # Download
    download.file(data0005[1], destfile = paste0(folder, 'mhsudduqc2020.zip'))
    download.file(data0005[2], destfile = paste0(folder, 'classesdemilieuxhumides.pdf'))
    download.file(data0005[3], destfile = paste0(folder, 'metadonneesmh.pdf'))


    # Unzip
    unzip(zipfile = paste0(folder, 'mhsudduqc2020.zip'), exdir = folder)
  }
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import and format data
  # ----------------------------------------
  data0005 <- st_read(paste0(folder, 'MH_sudduQC_2020.shp'))

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
  grid1p <- st_transform(grid1p, st_crs(data0005))

  # Identify polygons intersecting study area
  uid <- st_intersects(grid1p, data0005) %>%
         unlist() %>%
         unique() %>%
         sort()

  # Select polygons intersecting study area
  data0005 <- data0005[uid, ]

  # Transform projection
  data0005 <- st_transform(data0005, crs = global_parameters()$crs)
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0005,
           dsn = "./data/data-format/data0005-milieu_humide.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #
}
