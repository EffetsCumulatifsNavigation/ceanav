#' Data 0020 : Index des navires données AIS (id: 0021)
#'
#' Base de données contenant les informations des navires pour lesquels des données AIS satellitaire (id: 0021) sont disponibles
#'
#' @keywords navigation
#' @keywords stresseurs
#'
#' @source Transports Canada
#'
#' @export
#'
#' @details Cette fonction formatte les données
#'

get_data0020 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  #
  # WARNING: Données ne peuvent être partagées
  # WARNING: Données brutes doivent être supprimées suite au projet
  # _________________________________________________________________________ #

  # WARNING: Data transfered physically, no cloud access currently
  # Output folder
  output <- "data0020-navigation/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)


  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import and format
  # ----------------------------------------
  # Load vessel type dataset
  data0020 <- read.csv(paste0(folder, 'STATIC_DATA_2015_to_2019_forULAVAL.csv'))
  # _____________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  write.csv(x = data0020,
            file = "./data/data-format/data0020-navigation.csv",
            row.names = FALSE)


  # RData
  save(data0020, file = "./data/data0020.RData")
  # _________________________________________________________________________ #
}
