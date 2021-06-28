#' Data 0023 : Saumon Atlantique AGHAMM
#'
#' Important sites for Atlantic salmon for the communities of Gesgapegiag, Gespeg and Viger
#'
#' @keywords saumon Atlantique
#' @keywords sites d'importance
#' @keywords composante valorisée
#'
#' @source Atlas AGHAMM
#'
#' @export
#'
#' @details Cette fonction formatte les données
#'

get_data0023 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  #
  # Important sites for Atlantic salmon for the communities of
  # Gesgapegiag, Gespeg and Viger
  #
  # Les données de saumon Atlantique, pour nos besoins, correspondraient à
  # l’embouchure des Rivières Rimouski et Métis puisque le bassin versant
  # n’est pas inclu dans notre aire d’étude. Je pourrai être en mesure de
  # cibler les embouchures moi-même et j’utiliserai  le rapport sur l’Atlas
  # comme citation:
  # https://aghamm.maps.arcgis.com/apps/Cascade/index.html?appid=73f20ada030f424d89200b8c5473f849
  #
  # Une citation de prise de bar rayé dans le secteur de la  la Matane.
  # Tu pourrais  mettre l’estuaire de cette rivière comme importante étant une
  # rivière à saumon également:
  # https://aghamm.maps.arcgis.com/apps/Cascade/index.html?appid=bfa444495fae4aa4bf8760100b8b6bf2
  #
  # Atlas:
  # Arsenault, L.M. Racine, M.-J. and Lambert Koizumi, C. (2017) Atlas of Marine
  # St. Lawrence Mi’gmaq and Maliseet Sites and Their Uses by the Gesgapegiag,
  # Gespeg and Viger Communities. Mi’gmaq Maliseet Aboriginal Fisheries Management
  # Association (MMAFMA), 46 p.

  # Output folder
  output <- "data0023-saumon_aghamm/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # WARNING: Data not available, had to identify sites manually
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import and format data
  # ----------------------------------------
  # Coordonnées de l'embouchure des rivières
  df <- data.frame(riviere = c('rimouski','metis','matane'),
                   longitude = c(-68.54104, -68.13395, -67.53268),
                   latitude = c(48.44548, 48.63192, 48.85499))

  # Crééer géometries pour l'embouchures des trois rivières d'intérêt
  data0023 <- st_as_sf(df, coords = c('longitude','latitude'), crs = 4326)

  # Simply add a buffer around points and add as presence to the grid
  # WARNING: Buffer size is arbitrary, to confirm
  message("Le buffer (3000m) autour des embouchures de rivères d'importance pour le saumon Atlantique est arbitraire et devrait être révisé au besoin.")
  data0023 <- st_transform(data0023, crs = 32198) %>%
              st_buffer(3000)
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0023,
           dsn = "./data/data-format/data0023-saumon_aghamm.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #
}
