#' Data XXXX : Suivi de la qualité de l'eau du fleuve et des rivières
#'
#'
#'
#' @keywords a
#'
#' @source https://www.donneesquebec.ca/recherche/dataset/suivi-physicochimique-des-rivieres-et-du-fleuve#
#'
#' @export
#'
#' @details This function loads and formats the data
#'

get_dataXXXX <- function() {
  # # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # # Download data
  # # ----------------------------------------
  # # Output folder
  # output <- "data0001-bioregions/"
  # folder <- paste0("./data/data-raw/", output)
  # if (!file.exists(folder)) dir.create(folder)
  #
  # if (!file.exists(paste0(output, 'DFO_Marine_Bioregions.zip'))) {
  #   # Download from Open Canada portal
  #   uid <- "23eb8b56-dac8-4efc-be7c-b8fa11ba62e9"
  #   library(rgovcan)
  #   govcan_dl_resources(uid, path = folder)
  #
  #   # Unzip file
  #   unzip(zipfile = paste0(folder, 'DFO_Marine_Bioregions.zip'), exdir = folder)
  # }
  # # _________________________________________________________________________ #
  #
  # # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # # Import data
  # # ----------------------------------------
  # data0001 <- st_read(paste0(folder, 'DFO_Marine_Bioregions/DFO_Marine_Bioregions.gdb'),
  #                     layer = 'DFO_Marine_Bioregions') %>%
  #             st_transform(4326)
  # # _________________________________________________________________________ #
}
