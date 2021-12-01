#' Data 0062 : Inventory of vessel of concern
#'
#' Distribution of vessels of concern filtered for our study area. The majority of vessels have yet to be validated by CCG: this validation process is underway. As such, the accuracy of the coordinates cannot be guaranteed.
#'
#' @keywords stresseurs
#' @keywords naufrages
#'
#' @source Canadian Coast Guard (2021) Inventory of Vessels of Concern (VOC). [Date accessed: 2021-11-18]
#'
#' @export
#'
#' @details Cette fonction formatte les données
#'

get_data0062 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  #
  # WARNING: Données ne peuvent être partagées
  # WARNING: Données brutes doivent être supprimées suite au projet
  # _________________________________________________________________________ #

  # WARNING: Data transfered physically, no cloud access currently
  # Output folder
  output <- "data0062-vessel_of_concern_inventory/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)


  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import and format
  # ----------------------------------------
  # Load vessel type dataset
  data0062 <- read.csv(paste0(folder, 'CCG_VOC_Inventory-QC-TC.csv')) %>%
              st_as_sf(coords = c("X","Y"), crs = 4326) %>%
              st_transform(crs = global_parameters()$crs)
  # _____________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0062,
           dsn = "./data/data-format/data0062-vessel_of_concern_inventory.geojson",
           delete_dsn = TRUE,
           quiet = TRUE)
  # _________________________________________________________________________ #
}
