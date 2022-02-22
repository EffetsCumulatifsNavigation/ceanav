#' Data 0072 : Communauté Wolastoqiyik Wahsipekuk sites d'intérêt
#'
#' Localisation de sites d'intérêt pour l'observation du béluga et le développement portuaire
#'
#' @keywords Wolastoqiyik Wahsipekuk
#' @keywords sites d'interêt
#' @keywords composante valorisée
#'
#' @source Première Nation Wolastoqiyik Wahsipekuk (2022) Localisation de sites d'intérêt pour l'observation du béluga et le développement portuaire
#'
#' @export
#'
#' @details Cette fonction formatte les données
#'

get_data0072 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # WARNING:
  message("Les données brutes sont soumis à une entente de partage de données et doivent être supprimées au terme du projet")

  # Output folder
  output <- "data0072-wolastoqiyik_wahsipekuk_sites_interet/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # WARNING: Data transfered physically, no cloud access currently
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import and format data
  # ----------------------------------------
  data0072 <- list()
  data0072[[1]] <- st_read(paste0(folder, "Developpement_portuaire.shp")) 
  data0072[[2]] <- st_read(paste0(folder, "Site_observation_beluga.shp"))
  
  # Add names 
  data0072[[1]] <- mutate(data0072[[1]], nom = "Développement portuaire")    
  data0072[[2]] <- mutate(data0072[[2]], nom = "Observation du béluga")  
  
  # Single dataset
  data0072 <- bind_rows(data0072)

  # Transform projection
  data0072 <- st_transform(data0072, crs = global_parameters()$crs)
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0072,
           dsn = "./data/data-format/data0072-wolastoqiyik_wahsipekuk_sites_interet.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #
}
