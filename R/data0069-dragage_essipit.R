#' Data 0069 : Sites de dragage et de dépôt de la Première Nation des Innus d'Essipit
#'
#' Position et volume des sites de dragages et de dépôt en 2001, 2011 et 2016 pour la Première Nation des Innus d'Essipit dans la région des Bergeronnes
#'
#' @keywords dragage
#'
#' @source Bourchard D. (communication personnelle) Position et volume dragués et den 2001, 2011 et 2016 pour la Première Nation des Innus d'Essipit dans la région des Bergeronnes
#'
#' @export
#'
#' @details Cette fonction formatte les données
#'

get_data0069 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0069-dragage_essipit/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import and format data
  # ----------------------------------------
  # Données
  # Dragage: 48°13.66’N, 069°33.20’W.         # 48.22767, -69.55333
  # Depot: 
  # Année 	Volume (m3) 	dépot
  # 2001 	700 	48°12.927’N, 069°33.335’W.    # 48.21545, -69.55558
  # 2011 	2025 	48°12.00’N, 069°34.80’W       # 48.2, -69.58
  # 2016 	5000​  48°12.00’N, 069°34.80’W.​      # 48.2, -69.58
  	



  dat <- data.frame(municipalite = "Les Bergeronnes",
                    name = "Première Nation des Innus d'Essipit",
                    type = c("dragage","dragage","dragage","depot","depot","depot"),
                    annee = c(2001,2011,2016,2001,2011,2016),
                    volume = c(700,2025,5000,700,2025,5000))

  # coordinates
  coords <- data.frame(longitude = c(-69.55333,-69.55333,-69.55333,-69.55558,-69.58,-69.58),
                       latitude = c(48.22767,48.22767,48.22767,48.21545,48.2,48.2))
  
  data0069 <- st_as_sf(coords, coords = c("longitude","latitude"), crs = 4326) %>%
              cbind(dat) %>%
              st_transform(crs = global_parameters()$crs) %>%
              st_buffer(100)
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0069,
           dsn = "./data/data-format/data0069-dragage_essipit.geojson",
           delete_dsn = TRUE,
           quiet = TRUE)
  # _________________________________________________________________________ #
}
