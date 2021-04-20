getDragage <- function() {
  output <- './analysis/data/stresseurs/dragages/'

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Dragages ECCC - Simon Blais
  # ------------------------------------
  # dataID: 0028
  # ~~~~~~~~~~~~
  #
  # Bilan des activités de dragage dans l'estuaire du Saint-Laurent entre
  # 2001 et 2016
  #
  # Données transférées sous forme de rapport et de fichier excel
  #
  # Le travail de formattage et de préparation des données sera donc manuel
  #
  # Degrees minutes seconds to degrees decimals converter:
  # https://www.rapidtables.com/convert/number/degrees-minutes-seconds-to-degrees.html
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Output folder
  folder <- paste0(output, 'dragage_eccc/')
  if (!file.exists(folder)) dir.create(folder)


  # -------------------------------------------------------------------------
  # Halte Nautique de Saint-Michel-de-Bellechasse
  ## Coordonnées de la halte nautique
  coords <- rbind(c(46.87879077008841, -70.90834133311455),
                  c(46.87822770301899, -70.9100442274937),
                  c(46.877006174053804, -70.90990605635466),
                  c(46.877893989103704, -70.90781046074576),
                  c(46.87879077008841, -70.90834133311455))
  coords <- coords[,c(2,1)]

  ## Data.frame
  df <- data.frame(municipalite = "Saint-Michel-de-Bellechasse",
                   site_dragage = "Halte nautique de Saint-Michel-de-Bellechasse",
                   promoteur = "Société de développement de l'Anse Saint-Michel inc.",
                   organisme = "Marina privée",
                   type_dragage = "Entretien",
                   classification = "Marina",
                   type_equipement = "Mécanique",
                   depot = "Eau libre",
                   superficie_m2 = 17410,
                   volume_m3 = 5000,
                   annees  = 2009:2016,
                   type = 'Dragage')

  ## sf object
  smdb_dg <- list(coords) %>%
             st_polygon() %>%
             st_sfc(.,.,.,.,.,.,.,.,crs = 4326) %>%
             cbind(df, .) %>%
             st_sf()

  # Site de dépôt: 46˚53'01"N / 70˚54'33"O
  ## Coordonnées du site de dépôt
  coords <- matrix(c(46.88361, -70.90917), nrow = 1)
  coords <- coords[,c(2,1)]

  ## Data.frame
  df$type <- 'Depot'

  ## sf object
  smdb_dp <- st_point(coords) %>%
             st_sfc(.,.,.,.,.,.,.,.,crs = 4326) %>%
             cbind(df,.) %>%
             st_sf()


}
