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
  #
  #
  # ------------------------------------
  # Pour plusieurs sites de dépôts et dragage, les données sur l'entretien
  # de la voie maritime de la garde côtière canadienne sont utilisés
  #
  # dataID: 0029
  # ~~~~~~~~~~~~
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Output folder
  folder <- paste0(output, 'dragage_eccc/')
  if (!file.exists(folder)) dir.create(folder)

  # Importer données de la GCC
  secteurs <- st_read(paste0(output, 'dragage_gcc/secteurs.shp')) %>%
              st_transform(4326)

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
             st_sf() %>%
             st_transform(32198) %>%
             st_buffer(200) %>%
             st_transform(4326)


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
             st_sf() %>%
             st_transform(32198) %>%
             st_buffer(200) %>%
             st_transform(4326)


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


  # -------------------------------------------------------------------------
  # Traverse Nord de l’Île d’Orléans
  ## Secteur de la voie navigable du Saint-Laurent

  ## Sites de dragage
  uid <- c('G04amont','G04aval','G05','G06','G07','G08','G09','G10',
           'G11amont','G11centre','G11aval','G12','G13','G14','G15')
  tn_dg <- secteurs[secteurs$name %in% uid, ] %>%
           st_union()

  ## Data.frame
  df <- data.frame(municipalite = "NA",
                   site_dragage = "Traverse Nord I.O.",
                   promoteur = "Garde côtière canadienne",
                   organisme = "Gouvernement du Canada",
                   type_dragage = "Entretien",
                   classification = "Chenal",
                   type_equipement = "Hydraulique",
                   depot = "Eau libre",
                   superficie_m2 = NA,
                   volume_m3 = c(51162,51484,55945,53627,52694,55040,53032,49616),
                   annees  = c(2009:2016),
                   type = 'Dragage')

  ## Spatial object
  tn_dg <- cbind(df, tn_dg[rep(1, nrow(df)), ]) %>% st_sf()

  ## Sites de dépôt
  ## Maximum de de 10 000 m^3 pour X-02
  uid <- c('X02','X03')
  tn_dp <- secteurs[secteurs$name %in% uid, ] %>%
           select(geometry)

  dfx02 <- df %>%
           mutate(type = 'Depot',
                  volume_m3 = 10000)

  dfx03 <- dfx02 %>%
           mutate(volume_m3 = df$volume_m3 - 10000)

  ## Spatial objects
  x02 <- cbind(dfx02, tn_dp[rep(1, nrow(dfx02)), ])
  x03 <- cbind(dfx03, tn_dp[rep(2, nrow(dfx03)), ])
  tn_dp <- rbind(x02,x03) %>% st_sf()

  # -------------------------------------------------------------------------
  # Quai de Saint-Antoine-de-l’Isle-aux-Grues (Montmagny)
  ## Position du quai : 47° 3'18.75"N  70°31'54.43"O
  ## Data.frame
  df <- data.frame(X = -70.53179, Y = 47.05521,
                   municipalite = "Isle-aux-Grues",
                   site_dragage = "Quai de l'ïle-aux-Grues",
                   promoteur = "Société des traversiers du Québec",
                   organisme = "Gouvernement du Québec",
                   type_dragage = "Entretien",
                   classification = "Quai de traversier",
                   type_equipement = "Mécanique",
                   depot = "Eau libre",
                   superficie_m2 = NA,
                   volume_m3 = c(5052,4836,4609,4404,4781,5271,5666),
                   annees  = c(2009:2015),
                   type = 'Dragage')

  ## sf object
  qiag_dg <- st_as_sf(df, coords = c('X','Y'), crs = 4326) %>%
             st_transform(32198) %>%
             st_buffer(100) %>%
             st_transform(4326)

  # Coordonnées où les sédiments sont gérés, aka site de dépôt
  coords <- rbind(c(-70.49969, 47.05239), # 47°03’ 08.6’’ N / 70°29’58,9’’ O
                  c(-70.49767, 47.05125), # 47°03’ 04,5’’ N / 70°29’ 51,6’’ O
                  c(-70.49936, 47.04986), # 47°02’ 59,5’’ N / 70°29’ 57,7’’ O
                  c(-70.50139, 47.05103), # 47°03’ 03,7’’ N / 70°30’ 05,0’’ O
                  c(-70.49969, 47.05239))

  ## Data.frame
  df$type <- 'Depot'
  df <- select(df, -X, -Y)

  ## sf object
  qiag_dp <- list(coords) %>%
             st_polygon() %>%
             st_sfc(.,.,.,.,.,.,.,crs = 4326) %>%
             cbind(df, .) %>%
             st_sf()


  # -------------------------------------------------------------------------
  # Parc nautique de Saint-Jean-Port-Joli (marina)
  ## Coordonnées de la marina
  coords <- rbind(c(-70.27263, 47.21520),
                  c(-70.27379, 47.21420),
                  c(-70.27492, 47.21478),
                  c(-70.27549, 47.21601),
                  c(-70.27537, 47.21640),
                  c(-70.27490, 47.21701),
                  c(-70.27462, 47.21684),
                  c(-70.27448, 47.21656),
                  c(-70.27445, 47.21633),
                  c(-70.27460, 47.21614),
                  c(-70.27263, 47.21520))

  ## Data.frame
  df <- data.frame(municipalite = "Saint-Jean-Port-Joli",
                   site_dragage = "Parc nautique de Saint-Jean-Port-Joli",
                   promoteur = "Parc nautique de Saint-Jean-Port-Joli",
                   organisme = "Marina privée",
                   type_dragage = "Entretien",
                   classification = "Marina",
                   type_equipement = "Hydraulique",
                   depot = "Eau libre",
                   superficie_m2 = NA,
                   volume_m3 = rep(10000,6),
                   annees  = c(2009:2011, 2014:2016),
                   type = 'Dragage')

  ## sf object
  sjpj_dg <- list(coords) %>%
             st_polygon() %>%
             st_sfc(.,.,.,.,.,.,crs = 4326) %>%
             cbind(df, .) %>%
             st_sf()

  # Site de dépôt: 600m de la jetée
  # Figure et coorodnnées disponible dans un rapport d'évaluation environnementale:
  # http://collections.banq.qc.ca/ark:/52327/bs47822
  # "Ce site de rejet des sédiments est situé à 70° 18 de longitude ouest et
  # 47° 15 de latitude nord (Figure 1)"
  ## sf object
  df$X <- -70.3
  df$Y <- 47.25
  df$type <- 'depot'
  sjpj_dp <- st_as_sf(df, coords = c('X','Y'), crs = 4326) %>%
             st_transform(32198) %>%
             st_buffer(200) %>%
             st_transform(4326)


  # -------------------------------------------------------------------------
  # Traverse de Saint-Joseph-de-la-Rive à l’Isle-aux-Coudres (quai de traversiers)
  #
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Quai de l’Isle-aux-Coudres : 47°25'14.75'' N, 70°23'32.48'' O
  ## Data.frame
  df <- data.frame(X = -70.39236, Y = 47.42076,
                   municipalite = "Isle-aux-coudres",
                   site_dragage = "Quai de l'Île-aux-Coudres",
                   promoteur = "Société des traversiers du Québec",
                   organisme = "Gouvernement du Québec",
                   type_dragage = "Entretien",
                   classification = "Quai de traversier",
                   type_equipement = "Mécanique",
                   depot = "Eau libre",
                   superficie_m2 = c(8500,13500,11000,11000,7000,NA,5000,44300),
                   volume_m3 = c(26450,25304,24019,21447,25656,15914,16142,74190),
                   annees  = c(2009:2016),
                   type = 'Dragage')

  ## sf object
  qiac_dg <- st_as_sf(df, coords = c('X','Y'), crs = 4326) %>%
             st_transform(32198) %>%
             st_buffer(100) %>%
             st_transform(4326)

  # Coordonnées où les sédiments sont gérés, aka site de dépôt
  coords <- rbind(c(-70.38958, 47.4391), # 47°26,346’ N, 70°23,375’ O
                  c(-70.3865, 47.4391), # 47°26,346’ N, 70°23,190’ O
                  c(-70.3865, 47.4375), # 47°26,250’ N, 70°23,190’ O
                  c(-70.38958, 47.4375), # 47°26,250’ N, 70°23,375’ O
                  c(-70.38958, 47.4391))

  ## Data.frame
  df$type <- 'Depot'
  df <- select(df, -X, -Y)

  ## sf object
  qiac_dp <- list(coords) %>%
             st_polygon() %>%
             st_sfc(.,.,.,.,.,.,.,.,crs = 4326) %>%
             cbind(df, .) %>%
             st_sf()

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Quai de Saint-Joseph-de-la-Rive : 47.448753, -70.365030
  ## Data.frame
  df <- data.frame(X = -70.365030, Y = 47.448753,
                   municipalite = "Saint-Joseph-de-la-Rive",
                   site_dragage = "Quai de Saint-Joseph-de-la-Rive",
                   promoteur = "Société des traversiers du Québec",
                   organisme = "Gouvernement du Québec",
                   type_dragage = "Entretien",
                   classification = "Quai de traversier",
                   type_equipement = "Mécanique",
                   depot = "Eau libre",
                   superficie_m2 = c(2800,8500,1500),
                   volume_m3 = c(1909,6500,2399),
                   annees  = c(2011,2012,2015),
                   type = 'Dragage')

  ## sf object
  qsjr_dg <- st_as_sf(df, coords = c('X','Y'), crs = 4326) %>%
             st_transform(32198) %>%
             st_buffer(100) %>%
             st_transform(4326)

  # Coordonnées où les sédiments sont gérés, aka site de dépôt
  # Même site de dépôt que le Quai de l'Ise-aux-Coudres
  coords <- rbind(c(-70.38958, 47.4391), # 47°26,346’ N, 70°23,375’ O
                  c(-70.3865, 47.4391), # 47°26,346’ N, 70°23,190’ O
                  c(-70.3865, 47.4375), # 47°26,250’ N, 70°23,190’ O
                  c(-70.38958, 47.4375), # 47°26,250’ N, 70°23,375’ O
                  c(-70.38958, 47.4391))

  ## Data.frame
  df$type <- 'Depot'
  df <- select(df, -X, -Y)

  ## sf object
  qsjr_dp <- list(coords) %>%
             st_polygon() %>%
             st_sfc(.,.,.,crs = 4326) %>%
             cbind(df, .) %>%
             st_sf()


















































  # -------------------------------------------------------------------------
  ## Bind together
  dragage  <- rbind(smdb_dg, cnib_dg, hbm_dg, tn_dg, qiag_dg, sjpj_dg, qiac_dg, qsjr_dg)
  depot    <- rbind(smdb_dp, cnib_dp, hbm_dp, tn_dp, qiag_dp, sjpj_dp, qiac_dp, qsjr_dp)
  secteurs <- st_read(paste0(output, 'dragage_gcc/secteurs.shp'))

  mapview(secteurs) + dragage + depot

}
