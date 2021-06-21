#' Data 0029 :
#'
#'
#'
#' @keywords marais
#' @keywords composante valorisée
#'
#' @source
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
  output <- "data0029-marais/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # WARNING: Data transfered physically, no cloud access currently
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import and format data
  # ----------------------------------------
  # Unzip dataset
  unzip(zipfile = paste0(folder, "marais.zip"), exdir = paste0(folder, "marais"))
  unzip(zipfile = paste0(folder, "marais/Annexes_Gilbert2004.zip"), exdir = paste0(folder, "marais/Annexes_Gilbert2004"))

  # ----------------------------------------
  # WARNING:
  # This dataset is a microsoft access database, so it requires additional tools to access it.
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
  library(mdbr)

  # Get list of tables available in the access database
  filepath  <- paste0(folder, 'marais/Datas_layers_marais_PMSSL_2003.mdb')
  tab <- mdb_tables(filepath)

  # Get data
  # dat <- read_mdb(filepath, tab[3])


  # data0029 <- st_read(paste0(folder, "grille_classe1/grille_500m_classe1.shp"))
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  # st_write(obj = data0029,
  #          dsn = "./data/data-format/data0029-marais.geojson",
  #          delete_dsn = TRUE)
  #
  # # RData
  # save(data0029, file = "./data/data0029.RData")
  # _________________________________________________________________________ #
}
