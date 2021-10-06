#' Data 0053 : Milieux humides potentiels
#'
#' La cartographie des milieux humides potentiels du Québec (CMHPQ) 2019, diffusée par la Direction de la connaissance écologiques (DCE), fournit une information la plus à jour sur la présence potentielle de milieux humides pour toute la province du Québec. Une série de nouveautés sont maintenant intégrées dans la version 2019 de la CMHPQ. Entre autre, l’ajout de nouvelles sources de données, la classification des entités géographiques potentiellement humide selon une typologie retenue et l’attribution d’un niveau de confiance associé à chaque entité. Cette cartographie constitue une agrégation de différentes bases de données produites à d’autres fins et à des échelles différentes, le résultat de cet assemblage dépend de la précision et de l’exactitude de chacune des sources de données utilisées. Ces données doivent être utilisées en restant vigilant au regard de leurs potentiels et limites.
#'
#' @keywords milieu humide
#' @keywords habitat
#' @keywords composante valorisée
#'
#' @source https://www.donneesquebec.ca/recherche/dataset/milieux-humides-potentiels
#'
#' @export
#'
#' @details Cette fonction télécharge et formatte les données
#'

get_data0053 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0053-milieu_humide_potentiels/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'MH_Potentiel_2019_Guide_utilisateur.pdf'))) {
    # URL
    data0053 <- c('ftp://ftp.mddelcc.gouv.qc.ca/DONNEES_OUVERTES/Milieux_humides_potentiels/MH_POTENTIEL_2019.gdb.zip',
                  'ftp://ftp.mddelcc.gouv.qc.ca/DONNEES_OUVERTES/Milieux_humides_potentiels/MH_Potentiel_2019_Guide_utilisateur.pdf')

    # Download
    oldopt <- options()$timeout
    options(timeout=500)
    download.file(data0053[1], destfile = paste0(folder, 'MH_POTENTIEL_2019.gdb.zip'))
    download.file(data0053[2], destfile = paste0(folder, 'MH_Potentiel_2019_Guide_utilisateur.pdf'))
    options(timeout=oldopt)


    # Unzip
    unzip(zipfile = paste0(folder, 'MH_POTENTIEL_2019.gdb.zip'), exdir = folder)
  }
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import and format data
  # ----------------------------------------
  data0053 <- st_read(paste0(folder, 'MH_POTENTIEL_2019.gdb'),
                      layer = "MH_POTENTIEL_2019",
                      quiet = TRUE)

  # WARNING:
  # "The dataset is very big and covers a much broader area than what we need
  # for this project. I will therefore only select polygons that intersect with
  # our study area. Simply rqemove the code that intersects with our study area
  # to get the whole dataset"
  message("The dataset is very big and covers a much broader area than what we need for this project. I will therefore only select polygons that intersect with our study area. Simply remove the code that intersects with our study area to get the whole dataset")

  # Study area grid
  data(grid1p)
  grid1p <- st_transform(grid1p, st_crs(data0053))

  # -----
  # Problem with certain geometries...
  # Identify which and remove
  # WARNING: This is a very long process!
  #          For the data that I loaded on 2021-10-05, these are the problematic polygons:
  #          uid <- c(74220, 178010)
  #          Use this directly if you use the same data, otherwise run the code to identify them
  #          in case the data available was modified. Just be prepared to wait for a long time.
  uid <- numeric()
  for(i in 1:nrow(data0053)) {
    x <- suppressWarnings(try(st_buffer(data0053[i, ], 1))) %>% class() %>% .[1]
    if (x == "try-error") uid <- c(uid,i)
  }

  # -----
  data0053 <- data0053[-uid, ]

  # Identify polygons intersecting study area
  uid <- st_intersects(grid1p, data0053) %>%
         unlist() %>%
         unique() %>%
         sort()

  # Select polygons intersecting study area
  data0053 <- data0053[uid, ]

  # Transform projection
  data0053 <- st_transform(data0053, crs = global_parameters()$crs)
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0053,
           dsn = "./data/data-format/data0053-milieu_humide_potentiels.geojson",
           delete_dsn = TRUE,
           quiet = TRUE)
  # _________________________________________________________________________ #
}
