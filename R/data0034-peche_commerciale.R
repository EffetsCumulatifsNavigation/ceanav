#' Data 0034 : Index des engins de pêche pour la pêche commerciale (id: 0033)
#'
#' Index des engin pour les données de from Zonal Interchange File Format (ZIFF) entre 2000 et 2020
#'
#' @keywords pêche commerciale
#' @keywords stresseurs
#'
#' @source Fisheries and Oceans Canada (2021). Index of fishing gears in departement of Fisheries and Oceans Canada’s Fisheries and Oceans Canada Zonal Interchange File Format (ZIFF) data. A compilation of landing data from logbook data between 2000 and 2020. [Data accessed 2021-07-15]
#'
#' @export
#'
#' @details Cette fonction formatte les données
#'

get_data0034 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # WARNING:
  message("Les données brutes sont soumis à une entente de partage de données")

  # Output folder
  output <- "data0034-peche_commerciale/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # WARNING: Data transfered physically, no cloud access currently
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import and format data
  # ----------------------------------------
  data0034 <- read.csv(paste0(folder, 'Codes_engin.csv'))
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  write.csv(x = data0034,
            file = "./data/data-format/data0034-peche_commerciale.csv",
            row.names = FALSE)
  # _________________________________________________________________________ #
}
