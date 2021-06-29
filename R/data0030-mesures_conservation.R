#' Data 0030 : Mesures spatiales de gestion parc marin
#'
#' Limites des différentes mesures spatiales de gestion en place au parc marin
#'
#' @keywords mesure de conservation
#'
#' @source Agence Parcs Canada (2021) Limites des différentes mesures spatiales de gestion en place au parc marin
#'
#' @export
#'
#' @details Cette fonction formatte les données
#'

get_data0030 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # WARNING:
  message("Les données brutes sont soumis à une entente de partage de données")

  # Output folder
  output <- "data0030-mesure_conservation/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # WARNING: Data transfered physically, no cloud access currently
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import and format data
  # ----------------------------------------
  # Unzip dataset
  unzip(zipfile = paste0(folder, "mesures_spatiales.zip"),
        exdir = folder)

  # Import data
  d <- list()

  # Zone ralentissement
  d[[1]] <- st_read(paste0(folder, "Zone_ralentissement_EMB_RAM.shp"), quiet = TRUE) %>%
            rename(Periode = Période, Annee = Année_1, Application = applicatio) %>%
            select(Nom, Periode, Annee, Application, geometry)


  # Zone rouge
  d[[2]] <- st_read(paste0(folder, "Zone_rouge_BSM_2018.shp"), quiet = TRUE) %>%
            rename(Nom = Nom_Secteu, Annee = Année_1, Application = Apllicatio,
                   Periode = Période) %>%
            select(Nom, Periode, Annee, Application, geometry)


  # Zone sans OBS
  d[[3]] <- st_read(paste0(folder, "Zone_sans_OBS_CoRe_EstMoy.shp"), quiet = TRUE) %>%
            rename(Nom = Info, Annee = Année_1, Application = Applicatio) %>%
            mutate(Periode = NA) %>%
            select(Nom, Periode, Annee, Application, geometry)

  # Zones rouges
  d[[4]] <- st_read(paste0(folder, "Zones_rouges_G2T3M.shp"), quiet = TRUE) %>%
            rename(Nom = info, Annee = Année_1, Application = Applicatio, Periode = Période) %>%
            select(Nom, Periode, Annee, Application, geometry)

  # Zones 1
  d[[5]] <- st_read(paste0(folder, "Zones1.shp"), quiet = TRUE) %>%
            rename(Nom = Nom_Secteu, Application = applicatio) %>%
            mutate(Periode = NA, Annee = NA) %>%
            select(Nom, Periode, Annee, Application, geometry)


  # Individual dataset
  data0030 <- bind_rows(d)
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0030,
           dsn = "./data/data-format/data0030-mesure_conservation.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #
}
