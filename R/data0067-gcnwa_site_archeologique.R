#' Data 0067 : Sites archéologiques GCNWA
#'
#' Rapport d’intrants fournis par le Bureau du Ndakina quant à l’étude des effets cumulatifs de la navigation commerciale. Rapport à l’intention de Transport Canada et Pêches et Océans Canada.
#'
#' @keywords GCNWA
#' @keywords Sites archéologiques
#' @keywords composante valorisée
#'
#' @source Grand Conseil de la Nation Waban-Aki (GCNWA), 2021. Rapport d’intrants fournis par le Bureau du Ndakina quant à l’étude des effets cumulatifs de la navigation commerciale. Rapport à l’intention de Transport Canada et Pêches et Océans Canada. Rédigé par E. Blanchet et G. Treyvaud, Bureau du Ndakina, 42 p. et annexes.
#'
#' @export
#'
#' @details Cette fonction formatte les données
#'

get_data0067 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # WARNING:
  message("Les données brutes sont soumis à une entente de partage de données et doivent être supprimées au terme du projet")

  # Output folder
  output <- "data0067-gcnwa_site_archeologique/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # WARNING: Data transfered physically, no cloud access
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import and format data
  # ----------------------------------------
  data0067 <- read.csv(paste0(folder, "sites_archeologiques.csv")) %>%
              st_as_sf(coords = c("Longitude","Latitude"), crs = 4326) %>%
              st_transform(crs = global_parameters()$crs) %>%
              st_buffer(1000)
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0067,
           dsn = "./data/data-format/data0067-gcnwa_site_archeologique.geojson",
           delete_dsn = TRUE,
           quiet = TRUE)
  # _________________________________________________________________________ #
}
