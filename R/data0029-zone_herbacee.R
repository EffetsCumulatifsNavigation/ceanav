#' Data 0029 : Zones herbacées
#'
#' Les données spatiales accompagnant le Portrait des zones herbacées du Parc marin du Saguenay–Saint-Laurent (Gilbert, 2004)
#'
#' @keywords marais
#' @keywords zone herbacée
#' @keywords zostère
#' @keywords spartine alternes
#' @keywords spartine pectinée
#' @keywords scirpe
#' @keywords habitat
#' @keywords composante valorisée
#'
#' @source Gilbert., H. 2004. Portrait des zones herbacées du Parc marin du Saguenay- Saint-Laurent. Bureau d’écologie appliquée, 21 p
#'
#' @export
#'
#' @details Cette fonction formatte les données
#'

get_data0029 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # WARNING:
  message("Les données brutes sont soumis à une entente de partage de données")

  # Output folder
  output <- "data0029-zone_herbacee/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # WARNING: Data transfered physically, no cloud access currently
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Unzip data
  # ----------------------------------------
  output <- paste0(folder, "Datas_layers_marais_PMSSL_2003_shapefiles/")
  unzip(zipfile = paste0(folder, "Datas_layers_marais_PMSSL_2003_shapefiles.zip"), exdir = folder)
  unzip(zipfile = paste0(output, "marais_sales_2003_Bergeronnes.zip"), exdir = output)
  unzip(zipfile = paste0(output, "marais_sales_2003.zip"), exdir = output)
  unzip(zipfile = paste0(output, "marais_sales_pmssl_2002.zip"), exdir = output)
  unzip(zipfile = paste0(output, "marais_sighap.zip"), exdir = output)
  unzip(zipfile = paste0(output, "scirpe_sighap.zip"), exdir = output)
  unzip(zipfile = paste0(output, "spartinealternes_sighap.zip"), exdir = output)
  unzip(zipfile = paste0(output, "spartinepectinee_sighap.zip"), exdir = output)
  unzip(zipfile = paste0(output, "Zostere_PMSSL.zip"), exdir = output)

  # # Unzip dataset (microsoft access db, not used here)
  # unzip(zipfile = paste0(folder, "marais.zip"), exdir = paste0(folder, "marais"))
  # unzip(zipfile = paste0(folder, "marais/Annexes_Gilbert2004.zip"), exdir = paste0(folder, "marais/Annexes_Gilbert2004"))

  # ----------------------------------------
  # WARNING:
  # The first dataset is a microsoft access database.
  # It requires additional tools to access it, although we are not using it here as
  # we access the shapefile directly.
  #
  # If you wish to use it, here is a way to import the data in R:
  #
  # First I use `mdbtools` available here: https://github.com/mdbtools/mdbtools
  #
  # brew install mdbtools
  #
  # Beware, I had to reinstall `gcc` after installing mdbtools because the
  # gfortran library was not accessible anymore on my system and I could not
  # load R anymore. It may have been only on my system, but try reinstalling
  # `gcc` if it happens to you as well
  #
  # brew reinstall gcc
  #
  # Then I use the `mdbr` R package to access data in the microsoft access database
  # https://github.com/kiernann/mdbr
  #
  # install.packages('mdbr')
  # ----------------------------------------
  # library(mdbr)
  #
  # # Get list of tables available in the access database
  # filepath  <- paste0(folder, 'marais/Datas_layers_marais_PMSSL_2003.mdb')
  # tab <- mdb_tables(filepath)
  #
  # # Get data
  # dat <- read_mdb(filepath, tab[3])
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import and format data
  # ----------------------------------------
  d <- list()

  # Marais sales 2003
  d[[1]] <- st_read(paste0(output, "marais_sales_2003_Bergeronnes.shp"), quiet = TRUE) %>%
            select(Groupe_dominant= GROUP_DOM, Espece_dominante = ESP_SS_DOM) %>%
            mutate(Type = "Marais sales", Annee = 2003)

  # marais_sales_2003
  d[[2]] <- st_read(paste0(output, "marais_sales_2003.shp"), quiet = TRUE) %>%
            select(Groupe_dominant= GROUP_DOM, Espece_dominante = ESP_SS_DOM) %>%
            mutate(Type = "Marais sales", Annee = 2003)

  # marais_sales_pmssl_2002
  d[[3]] <- st_read(paste0(output, "marais_sales_pmssl_2002.shp"), quiet = TRUE) %>%
            st_set_crs(st_crs(d[[1]])) %>%
            select(Plante_dominante1 = PL_DOM_1, Plante_dominante2= PL_DOM_2) %>%
            mutate(Type = "Marais sales", Source = "PMSSL", Annee = 2003)

  # marais_sighap
  d[[4]] <- st_read(paste0(output, "marais_sighap.shp"), quiet = TRUE) %>%
            st_set_crs(st_crs(d[[1]])) %>%
            select(ESPECE, THEME, CLASSE, PERIODE, LEGENDE) %>%
            select(Espece = ESPECE, Theme = THEME, Classe = CLASSE, Periode = PERIODE, Legende = LEGENDE) %>%
            mutate(Type = "Marais", Source = "SIGHAP")

  # scirpe_sighap
  d[[5]] <- st_read(paste0(output, "scirpe_sighap.shp"), quiet = TRUE) %>%
            st_set_crs(st_crs(d[[1]])) %>%
            select(Espece = ESPECE, Theme = THEME, Classe = CLASSE, Periode = PERIODE, Legende = LEGENDE) %>%
            mutate(Type = "Scirpe", Source = "SIGHAP")

  # spartinealternes_sighap
  d[[6]] <- st_read(paste0(output, "spartinealternes_sighap.shp"), quiet = TRUE) %>%
            st_set_crs(st_crs(d[[1]])) %>%
            select(Espece = ESPECE, Theme = THEME, Classe = CLASSE, Periode = PERIODE, Legende = LEGENDE) %>%
            mutate(Type = "Spartine alternes", Source = "SIGHAP")

  # spartinepectinee_sighap
  d[[7]] <- st_read(paste0(output, "spartinepectinee_sighap.shp"), quiet = TRUE) %>%
            st_set_crs(st_crs(d[[1]])) %>%
            select(Espece = ESPECE, Theme = THEME, Classe = CLASSE, Periode = PERIODE, Legende = LEGENDE) %>%
            mutate(Type = "Spartine pectinee", Source = "SIGHAP")

  # Zostere_PMSSL
  d[[8]] <- st_read(paste0(output, "Zostere_PMSSL.shp"), quiet = TRUE) %>%
            st_transform(st_crs(d[[1]])) %>%
            select(Espece = ESPECE, Theme = THEME, Classe = CLASSE, Periode = PERIODE, Legende = LEGENDE, Densite = DENSITE) %>%
            mutate(Type = "Zostere", Source = "PMSSL")



  # Single dataset
  data0029 <- bind_rows(d)
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0029,
           dsn = "./data/data-format/data0029-zone_herbacee.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #
}
