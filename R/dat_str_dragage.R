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


  # -------------------------------------------------------------------------
  # Club nautique de l'Île Bacchus inc.
  ## Coordonnées de la marina
  coords <- rbind(c(46.85883628359575, -71.00592195539443),
                  c(46.85942860450257, -71.00401319047153),
                  c(46.8584064537137, -71.00326961538109),
                  c(46.85779315390031, -71.00530869758786),
                  c(46.85883628359575, -71.00592195539443))
  coords <- coords[,c(2,1)]

  ## Data.frame
  df <- data.frame(municipalite = "Saint-Laurent-de-L'Île-D'Orléans",
                   site_dragage = "Port de refuge de St-Laurent-de-L'Île-D'Orléans",
                   promoteur = "Club nautique de l'Île Bacchus inc.",
                   organisme = "Municipalité",
                   type_dragage = "Entretien",
                   classification = "Marina",
                   type_equipement = "Hydraulique",
                   depot = "Eau libre",
                   superficie_m2 = c(560, 2725),
                   volume_m3 = c(2000,4675),
                   annees  = c(2012,2014),
                   type = 'Dragage')

  ## sf object
  cnib_dg <- list(coords) %>%
             st_polygon() %>%
             st_sfc(.,.,crs = 4326) %>%
             cbind(df, .) %>%
             st_sf()

  # Site de dépôt: 46˚53'01"N / 70˚54'33"O
  ## Coordonnées du site de dépôt
  coords <- matrix(c(46.85683960985289, -71.00324129280196), nrow = 1)
  coords <- coords[,c(2,1)]

  ## Data.frame
  df$type <- 'Depot'

  ## sf object
  cnib_dp <- st_point(coords) %>%
             st_sfc(.,.,crs = 4326) %>%
             cbind(df,.) %>%
             st_sf()


  # -------------------------------------------------------------------------
  # Havre de Berthier-sur-Mer
  ## Coordonnées de la marina
  coords <- rbind(c(-70.73491, 46.93354),
                  c(-70.73600, 46.93315),
                  c(-70.73667, 46.93304),
                  c(-70.73715, 46.93304),
                  c(-70.73759, 46.93323),
                  c(-70.73801, 46.93391),
                  c(-70.73853, 46.93546),
                  c(-70.73797, 46.93564),
                  c(-70.73742, 46.93402),
                  c(-70.73731, 46.93382),
                  c(-70.73715, 46.93371),
                  c(-70.73575, 46.93435),
                  c(-70.73491, 46.93354))


  ## Data.frame
  df <- data.frame(municipalite = "Berthier-sur-Mer",
                   site_dragage = "Havre de Berthier-sur-Mer",
                   promoteur = "Havre de Berthier-sur-Mer inc.",
                   organisme = "Marina privée",
                   type_dragage = "Entretien",
                   classification = "Marina",
                   type_equipement = "Mécanique",
                   depot = "Eau libre",
                   superficie_m2 = c(13400,4990,7000),
                   volume_m3 = c(23000,10000,12000),
                   annees  = c(2011,2014,2016),
                   type = 'Dragage')

  ## sf object
  hbm_dg <- list(coords) %>%
             st_polygon() %>%
             st_sfc(.,.,.,crs = 4326) %>%
             cbind(df, .) %>%
             st_sf()

  # Site de dépôt: 46˚53'01"N / 70˚54'33"O
  ## Coordonnées du site de dépôt
  coords <- rbind(c(-70.72694, 46.94006),
                  c(-70.73460, 46.93695),
                  c(-70.73619, 46.93857),
                  c(-70.72907, 46.94159),
                  c(-70.72694, 46.94006))

  ## Data.frame
  df$type <- 'Depot'

  ## sf object
  hbm_dp <- list(coords) %>%
            st_polygon() %>%
            st_sfc(.,.,.,crs = 4326) %>%
            cbind(df, .) %>%
            st_sf()


}
