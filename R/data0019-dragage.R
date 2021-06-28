#' Data 0019 : Sites de dragage et de dépôt
#'
#' Bilan des activités de dragage dans l'estuaire du Saint-Laurent
#'
#' @keywords dragage
#' @keywords stresseurs
#'
#' @source ECCC 2020. Bilan des activités de dragage dans l’Estuaire du Saint-Laurent de 2009 à 2016. Document de travail.
#'
#' @export
#'
#' @details Cette fonction formatte les données
#'

get_data0019 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # WARNING:
  #
  # Le travail de formattage et de préparation des données sera donc manuel
  #
  # Degrees minutes seconds to degrees decimals converter:
  # https://www.rapidtables.com/convert/number/degrees-minutes-seconds-to-degrees.html
  #
  # Code to visualize point, draw polygon and extract coordinates
  # x <- st_point(matrix(c(-67.2848, 49.4214), nrow = 1)) %>% st_sfc(crs = 4326)
  # x <- mapview(x) %>% editMap()
  # st_coordinates(x[[1]])
  # ------------------------------------
  # Pour plusieurs sites de dépôts et dragage, les données sur l'entretien
  # de la voie maritime de la garde côtière canadienne sont utilisés (id:0018)
  # _________________________________________________________________________ #

  # Output folder
  output <- "data0019-dragage/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # WARNING: Data transfered physically, no cloud access currently

  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import and format
  # ----------------------------------------
  # Importer données de la GCC
  secteurs <- st_read("./data/data-raw/data0018-dragage/secteurs.shp") %>%
              st_transform(4326)

  # Listes pour stocker les sites de dragage et de depot
  dragage <- depot <- list()

  # Rayon des buffers pour les sites de depot identifiés uniquement par un point
  buf <- 100

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
  dragage[["smdb"]] <- list(coords) %>%
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
  depot[["smdb"]] <- st_point(coords) %>%
                        st_sfc(.,.,.,.,.,.,.,.,crs = 4326) %>%
                        cbind(df,.) %>%
                        st_sf() %>%
                        st_transform(32198) %>%
                        st_buffer(buf) %>%
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
                   annees  = c(2014,2012),
                   type = 'Dragage')

  ## sf object
  dragage[["cnib"]] <- list(coords) %>%
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
  depot[["cnib"]] <- st_point(coords) %>%
             st_sfc(.,.,crs = 4326) %>%
             cbind(df,.) %>%
             st_sf() %>%
             st_transform(32198) %>%
             st_buffer(buf) %>%
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
  dragage[["hbm"]] <- list(coords) %>%
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
  depot[["hbm"]] <- list(coords) %>%
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
  tn <- secteurs[secteurs$name %in% uid, ] %>%
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
                   annees  = c(2016:2009),
                   type = 'Dragage')

  ## Spatial object
  dragage[["tn"]] <- cbind(df, tn[rep(1, nrow(df)), ]) %>% st_sf()

  ## Sites de dépôt
  ## Maximum de de 10 000 m^3 pour X-02
  uid <- c('X02','X03')
  tn <- secteurs[secteurs$name %in% uid, ] %>%
           select(geometry)

  dfx02 <- df %>%
           mutate(type = 'Depot',
                  volume_m3 = 10000)

  dfx03 <- dfx02 %>%
           mutate(volume_m3 = df$volume_m3 - 10000)

  ## Spatial objects
  x02 <- cbind(dfx02, tn[rep(1, nrow(dfx02)), ])
  x03 <- cbind(dfx03, tn[rep(2, nrow(dfx03)), ])
  depot[["tn"]] <- rbind(x02,x03) %>% st_sf()

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
                   annees  = c(2015:2009),
                   type = 'Dragage')

  ## sf object
  dragage[["qiag"]] <- st_as_sf(df, coords = c('X','Y'), crs = 4326) %>%
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
  depot[["qiag"]] <- list(coords) %>%
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
  dragage[["sjpj"]] <- list(coords) %>%
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
  depot[["sjpj"]] <- st_as_sf(df, coords = c('X','Y'), crs = 4326) %>%
             st_transform(32198) %>%
             st_buffer(buf) %>%
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
                   annees  = c(2016:2009),
                   type = 'Dragage')

  ## sf object
  dragage[["qiac"]] <- st_as_sf(df, coords = c('X','Y'), crs = 4326) %>%
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
  depot[["qiac"]] <- list(coords) %>%
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
                   annees  = c(2015,2012,2011),
                   type = 'Dragage')

  ## sf object
  dragage[["qsjr"]] <- st_as_sf(df, coords = c('X','Y'), crs = 4326) %>%
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
  depot[["qsjr"]] <- list(coords) %>%
             st_polygon() %>%
             st_sfc(.,.,.,crs = 4326) %>%
             cbind(df, .) %>%
             st_sf()

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Secteur de Rivière-du-Loup (quai de traversier et marina)
  ## ------------------------------------------
  ## Quai de traversier : 47.847816, -69.570059
  ## Coordonnées de la marina
  coords <- rbind(c(-69.56939, 47.84786),
                  c(-69.57083, 47.84784),
                  c(-69.57080, 47.84752),
                  c(-69.56935, 47.84756),
                  c(-69.56939, 47.84786))

  ## Data.frame
  df <- data.frame(municipalite = "Rivière-du-Loup",
                   site_dragage = "Désserte",
                   promoteur = "Société des traversiers du Québec",
                   organisme = "Gouvernement du Québec",
                   type_dragage = "Entretien",
                   classification = "Quai de traversier",
                   type_equipement = "Mécanique",
                   depot = "Eau libre",
                   superficie_m2 = NA,
                   volume_m3 = c(32371,31770,29528,29257,36970,29899,42125,51625,50609),
                   annees  = c(2015:2009, 2017, 2016),
                   type = 'Dragage')


  ## sf object
  dragage[["qrdl"]] <- list(coords) %>%
             st_polygon() %>%
             st_sfc(.,.,.,.,.,.,.,.,.,crs = 4326) %>%
             cbind(df, .) %>%
             st_sf()

  # Coordonnées site de dépôt 1 (2009:2015, 2017)
  coords1 <- rbind(c(-69.57778, 47.86222), # A) 47°51’44’’ N, 69°34’40’’ O;
                   c(-69.59139, 47.86667), # B) 47°52’00’’ N, 69°35’29’’ O;
                   c(-69.57389, 47.88944), # C) 47°53’22’’ N, 69°34’26’’ O;
                   c(-69.56083, 47.86222), # D) 47°51’44’’ N, 69°33’39’’ O
                   c(-69.57778, 47.86222))

  # Coordonnées site de dépôt 2 (2016)
  coords2 <- rbind(c(-69.56676, 47.87908), # A) 47°52’44.6816’’N – 69°34’00.3367 O
                   c(-69.56442, 47.88231), # C) 47°52’56.3321’’N – 69°33’51.9119 O
                   c(-69.56923, 47.88389), # D) 47°53’01.9994’’ N – 69°34’09.2316 O
                   c(-69.57157, 47.88065), # B) 47°52’50.3485’’N – 69°34’17.6557 O
                   c(-69.56676, 47.87908))

  ## Data.frame
  df$type <- 'Depot'

  ## sf object
  ## Premier site de depot pour les années 2009 à 2015 et 2017
  depot[["qrdl1"]] <- list(coords1) %>%
              st_polygon() %>%
              st_sfc(.,.,.,.,.,.,.,.,crs = 4326) %>%
              cbind(df[df$annees %in% c(2009:2015,2017), ], .) %>%
              st_sf()

  ## Deuxième site de depot pour l'année 2016
  ## sf object
  depot[["qrdl2"]] <- list(coords2) %>%
              st_polygon() %>%
              st_sfc(.,crs = 4326) %>%
              cbind(df[df$annees %in% c(2016), ], .) %>%
              st_sf()


  ## ---------------------------------------------
  ## Marina : 47.846733, -69.569109
  coords <- rbind(c(-69.56964, 47.84726),
                  c(-69.57028, 47.84699),
                  c(-69.57045, 47.84628),
                  c(-69.57207, 47.84622),
                  c(-69.57215, 47.84572),
                  c(-69.56764, 47.84591),
                  c(-69.56783, 47.84738),
                  c(-69.56964, 47.84726))

  ## Data.frame
  df <- data.frame(municipalite = "Rivière-du-Loup",
                   site_dragage = "Parc maritime de Rivière-du-Loup",
                   promoteur = "Société Duvetnor ltée; La Corporation du Carrefour maritime de Rivière-du-Loup",
                   organisme = "Transport Québec",
                   type_dragage = c("Entretien","Entretien","Hybride","Entretien","Entretien","Entretien","Entretien","Entretien"),
                   classification = "Marina",
                   type_equipement = c(rep("Mécanique",5), rep("Hydraulique",3)),
                   depot = "Eau libre",
                   superficie_m2 = NA,
                   volume_m3 = c(6042,8550,25000,1600,2000,45000,11500,11500),
                   annees  = c(2016,2015,2014,2011,2010,2018,2019,2020),
                   type = 'Dragage')


  ## sf object
  dragage[["mrdl"]] <- list(coords) %>%
             st_polygon() %>%
             st_sfc(.,.,.,.,.,.,.,.,crs = 4326) %>%
             cbind(df, .) %>%
             st_sf()

  # Coordonnées site de dépôt: pour l'instant utilisation du site 1 du quai de Rivière-du-Loup
  # WARNING: Vérifier s'il s'agit réellement du bon site de dépôt
  coords <- coords1

  ## Data.frame
  df$type <- 'Depot'

  ## sf object
  ## Premier site de depot pour les années 2009 à 2015 et 2017
  depot[["mrdl"]] <- list(coords) %>%
              st_polygon() %>%
              st_sfc(.,.,.,.,.,.,.,.,crs = 4326) %>%
              cbind(df, .) %>%
              st_sf()


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Port de Gros Cacouna (port de marchandises)
  ## Centroide du port (manuel) : 47.93021664372833, -69.51570540967386
  ## Coordonnées (avec mapedit)
  coords <- rbind(c(-69.50984, 47.93378),
                  c(-69.51883, 47.93625),
                  c(-69.52223, 47.93105),
                  c(-69.52180, 47.92788),
                  c(-69.51387, 47.92444),
                  c(-69.50984, 47.93378))

  ## Data.frame
  df <- data.frame(municipalite = "Gros-Cacouna",
                   site_dragage = "Port de Gros-Cacouna",
                   promoteur = NA,
                   organisme = NA,
                   type_dragage = "Entretien",
                   classification = "Port",
                   type_equipement = "Hydraulique",
                   depot = "Terrestre",
                   superficie_m2 = NA,
                   volume_m3 = 69250,
                   annees  = 2019,
                   type = 'Dragage')

  ## sf object
  dragage[["pgc"]] <- list(coords) %>%
            st_polygon() %>%
            st_sfc(crs = 4326) %>%
            cbind(df, .) %>%
            st_sf()



  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Notre-Dame de-l’Îsle-Verte, L’Isle-Verte (quai de traversiers)
  ## Coordonnées du quai : 48.039772, -69.406583
  ## Aucun détail disponible sur les activités de dragage
  ## Contact à la STQ : Michel Lefrançois
  df <- data.frame(X = -69.406583, Y = 48.039772,
                   municipalite = "Notre-Dame de-l’Îsle-Verte",
                   site_dragage = "Quai de la traverse de l'Îsle-Verte",
                   promoteur = "Société des traversiers du Québec",
                   organisme = "Gouvernement du Québec",
                   type_dragage = "Entretien",
                   classification = "Quai de traversier",
                   type_equipement = NA,
                   depot = NA,
                   superficie_m2 = NA,
                   volume_m3 = NA,
                   annees  = NA,
                   type = 'Dragage')

  ## sf object
  dragage[["qiv"]] <- st_as_sf(df, coords = c('X','Y'), crs = 4326) %>%
             st_transform(32198) %>%
             st_buffer(100) %>%
             st_transform(4326)


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Tadoussac (quai de traversier et marina)
  ## --------------------------------------------------
  ## Marina de Tadoussac (48° 8'19.60"N ; 69°43'4.69"O)
  ## Coordonnées de la marina
  coords <- rbind(c(-69.71762, 48.13832),
                  c(-69.71746, 48.13921),
                  c(-69.71452, 48.13855),
                  c(-69.71497, 48.13770),
                  c(-69.71762, 48.13832))

  ## Data.frame
  df <- data.frame(municipalite = "Tadoussac",
                   site_dragage = "Marina",
                   promoteur = NA,
                   organisme = "Marina privée",
                   type_dragage = "Entretien",
                   classification = "Marina",
                   type_equipement = "Mécanique",
                   depot = "Eau libre",
                   superficie_m2 = NA,
                   volume_m3 = 6000,
                   annees  = 2017,
                   type = 'Dragage')

  ## sf object
  dragage[["mtad"]] <- list(coords) %>%
             st_polygon() %>%
             st_sfc(.,crs = 4326) %>%
             cbind(df, .) %>%
             st_sf()

  # Coordonnées site de dépôt
  # Site d’immersion des Bergeronnes : (48°12’00’’N ; 69°34’48’’O)
  coords <- matrix(c(48.2, -69.58), nrow = 1)
  coords <- coords[,c(2,1)]

  ## Data.frame
  df$type <- 'Depot'

  ## sf object
  depot[["mtad"]] <- st_point(coords) %>%
             st_sfc(.,crs = 4326) %>%
             cbind(df,.) %>%
             st_sf() %>%
             st_transform(32198) %>%
             st_buffer(buf) %>%
             st_transform(4326)

  ## --------------------------------------------------
  ## Desserte de la traverse Taddousac - Baie-Sainte-Catherine
  ## =====================
  ## Desserte est
  coords <- rbind(c(-69.72755, 48.13854),
                  c(-69.72688, 48.13941),
                  c(-69.72614, 48.13918),
                  c(-69.72687, 48.13833),
                  c(-69.72755, 48.13854))

  ## Data.frame
  df <- data.frame(municipalite = "Tadoussac",
                   site_dragage = "Desserte est traverse Tadoussac - Baie-Sainte-Catherine",
                   promoteur = "Société des traversiers du Québec",
                   organisme = "Gouvernement du Québec",
                   type_dragage = "Entretien",
                   classification = "Quai de traversier ",
                   type_equipement = "Mécanique",
                   depot = "Eau libre",
                   superficie_m2 = NA,
                   volume_m3 = 1146,
                   annees  = 2014,
                   type = 'Dragage')

  ## sf object
  dragage[["detd"]] <- list(coords) %>%
             st_polygon() %>%
             st_sfc(.,crs = 4326) %>%
             cbind(df, .) %>%
             st_sf()

  ## Site de dépôt
  ## WARNING: À vérifier que le site est le bon!!
  df$type <- 'Depot'
  depot[["detg"]] <- cbind(df, st_geometry(depot[["mtad"]])) %>% st_sf()

  ## =====================
  ## Desserte ouest
  coords <- rbind(c(-69.73103, 48.12714),
                  c(-69.73011, 48.12770),
                  c(-69.72878, 48.12671),
                  c(-69.72959, 48.12613),
                  c(-69.73103, 48.12714))

  ## Data.frame
  df <- data.frame(municipalite = "Tadoussac",
                   site_dragage = "Desserte ouest traverse Tadoussac - Baie-Sainte-Catherine",
                   promoteur = "Société des traversiers du Québec",
                   organisme = "Gouvernement du Québec",
                   type_dragage = "Entretien",
                   classification = "Quai de traversier ",
                   type_equipement = "Mécanique",
                   depot = "Eau libre",
                   superficie_m2 = NA,
                   volume_m3 = 1146,
                   annees  = 2014,
                   type = 'Dragage')

  ## sf object
  dragage[["dotd"]] <- list(coords) %>%
             st_polygon() %>%
             st_sfc(.,crs = 4326) %>%
             cbind(df, .) %>%
             st_sf()

  ## Site de dépôt
  ## WARNING: À vérifier que le site est le bon!!
  df$type <- 'Depot'
  depot[["dotg"]] <- cbind(df, st_geometry(depot[["mtad"]])) %>% st_sf()


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Secteur port de Rimouski Est (port de marchandises et marina)
  ## --------------------------------------------------
  ## Coordonnées de la marina (mapedit)
  coords <- rbind(c(-68.50995, 48.47923),
                  c(-68.51281, 48.48108),
                  c(-68.51503, 48.48117),
                  c(-68.51602, 48.48049),
                  c(-68.51195, 48.47784),
                  c(-68.50995, 48.47923))

  ## Data.frame
  df <- data.frame(municipalite = "Rimouski",
                   site_dragage = "Marina de Rimouski",
                   promoteur = NA,
                   organisme = NA,
                   type_dragage = "Entretien",
                   classification = "Marina",
                   type_equipement = "Mécanique",
                   depot = "Eau libre",
                   superficie_m2 = NA,
                   volume_m3 = 3500,
                   annees  = 2019,
                   type = 'Dragage')

  ## sf object
  dragage[["mrim"]] <- list(coords) %>%
             st_polygon() %>%
             st_sfc(.,crs = 4326) %>%
             cbind(df, .) %>%
             st_sf()

  # Site de dépôt:
  ## Coordonnées du site de dépôt
  coords <- rbind(c(-68.55113, 48.52129), # 68° 33' 04.06" W; 48° 31' 16.65" N
                  c(-68.54856, 48.51892), # 68° 32' 54.80" W; 48° 31' 08.11" N
                  c(-68.55212, 48.51721), # 68° 33' 07.64" W; 48° 31' 01.96" N
                  c(-68.5547, 48.51958), # 68° 33' 16.91" W; 48° 31' 10.49" N
                  c(-68.55113, 48.52129))

  ## Data.frame
  df$type <- 'Depot'

  ## sf object
  depot[["mrim"]] <- list(coords) %>%
            st_polygon() %>%
            st_sfc(.,.,.,crs = 4326) %>%
            cbind(df, .) %>%
            st_sf()

  ## --------------------------------------------------
  ## Port: https://buyandsell.gc.ca/cds/public/2018/07/19/03b09a391aa4b24241247b8a1c2cb162/ABES.PROD.PW_QCM.B008.B17442.ATTA002.PDF
  ## Coordonnées du port (mapedit)
  coords <- rbind(c(-68.51351, 48.47823),
                  c(-68.51531, 48.47710),
                  c(-68.51735, 48.47842),
                  c(-68.51769, 48.47820),
                  c(-68.52002, 48.47978),
                  c(-68.51932, 48.48203),
                  c(-68.51868, 48.48230),
                  c(-68.51852, 48.48703),
                  c(-68.51774, 48.48697),
                  c(-68.51801, 48.48202),
                  c(-68.51751, 48.48106),
                  c(-68.51775, 48.48094),
                  c(-68.51351, 48.47823))

  ## Data.frame
  df <- data.frame(municipalite = "Rimouski",
                   site_dragage = "Port de Rimouski",
                   promoteur = NA,
                   organisme = NA,
                   type_dragage = "Entretien",
                   classification = "Port",
                   type_equipement = c("Mécanique","Hybride","Mécanique"),
                   depot = "Eau libre",
                   superficie_m2 = NA,
                   volume_m3 = c(134701,129613,5169),
                   annees  = c(2010,2019,2014),
                   type = 'Dragage')

  ## sf object
  dragage[["prim"]] <- list(coords) %>%
             st_polygon() %>%
             st_sfc(.,.,.,crs = 4326) %>%
             cbind(df, .) %>%
             st_sf()

  ## Site de dépôt
  ## WARNING: À vérifier que le site est le bon pour 2014!!
  df$type <- 'Depot'
  depot[["prim"]] <- cbind(df, st_geometry(depot[["mrim"]])) %>% st_sf()


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Secteur de Matane (port de marchandises, quai des pêcheurs et marina)
  ## --------------------------------------------------
  ## Coordonnées de la port (mapedit)
  coords <- rbind(c(-67.57563, 48.83775),
                  c(-67.58122, 48.84495),
                  c(-67.58095, 48.84660),
                  c(-67.57995, 48.84761),
                  c(-67.57752, 48.84912),
                  c(-67.57005, 48.84175),
                  c(-67.57563, 48.83775))

  ## Data.frame
  df <- data.frame(municipalite = "Matane",
                   site_dragage = "Port de Matane",
                   promoteur = NA,
                   organisme = NA,
                   type_dragage = "Entretien",
                   classification = "Port",
                   type_equipement = c("Mécanique","Hybride"),
                   depot = "Eau libre",
                   superficie_m2 = NA,
                   volume_m3 = c(16168, 9225),
                   annees  = c(2009, 2015),
                   type = 'Dragage')

  ## sf object
  dragage[["pmat"]] <- list(coords) %>%
             st_polygon() %>%
             st_sfc(.,.,crs = 4326) %>%
             cbind(df, .) %>%
             st_sf()

  # Site de dépôt:
  ## Coordonnées du site de dépôt: 48°51’ 35’’ N; 67°35’50’’ O.
  ## Coordonnées du site de dépôt
  coords <- matrix(c(48.85972, -67.59722), nrow = 1)
  coords <- coords[,c(2,1)]

  ## Data.frame
  df$type <- 'Depot'

  ## sf object
  depot[["pmat"]] <- st_point(coords) %>%
             st_sfc(.,.,crs = 4326) %>%
             cbind(df,.) %>%
             st_sf() %>%
             st_transform(32198) %>%
             st_buffer(buf) %>%
             st_transform(4326)


  ## --------------------------------------------------
  ## Coordonnées de la marina: 48° 51’ 9’’ N ; 67° 31’ 44’’ O
  ## 48.8525, -67.52889
  ## Polygon with mapedit
  coords <- rbind(c(-67.52713, 48.85286),
                  c(-67.52950, 48.85287),
                  c(-67.53087, 48.85256),
                  c(-67.53072, 48.85207),
                  c(-67.52710, 48.85228),
                  c(-67.52713, 48.85286))

  ## Data.frame
  df <- data.frame(municipalite = "Matane",
                   site_dragage = "Marina de Matane",
                   promoteur = NA,
                   organisme = NA,
                   type_dragage = "Entretien",
                   classification = "Marina",
                   type_equipement = "Mécanique",
                   depot = "Eau libre",
                   superficie_m2 = NA,
                   volume_m3 = 21000,
                   annees  = 2019,
                   type = 'Dragage')

  ## sf object
  dragage[["mmat"]] <- list(coords) %>%
             st_polygon() %>%
             st_sfc(.,crs = 4326) %>%
             cbind(df, .) %>%
             st_sf()

  ## Site de dépôt
  ## WARNING: À vérifier que le site est le bon pour 2014!!
  df$type <- 'Depot'
  depot[["mmat"]] <- cbind(df, st_geometry(depot[["pmat"]])) %>% st_sf()


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Quai de Godbout: 49° 19'21.25" N ; 67°35'30.49" O
  # x <- st_point(matrix(c(-67.5918, 49.32257), nrow = 1)) %>% st_sfc(crs = 4326)
  # x <- mapview(x) %>% editMap()
  coords <- rbind(c(-67.59252, 49.32208),
                  c(-67.59093, 49.32265),
                  c(-67.59125, 49.32301),
                  c(-67.59285, 49.32243),
                  c(-67.59252, 49.32208))

  ## Data.frame
  df <- data.frame(municipalite = "Godbout",
                   site_dragage = "Quai de Godbout",
                   promoteur = "Société des Traversiers du Québec",
                   organisme = "Gouvernement du Québec",
                   type_dragage = "Entretien",
                   classification = "Quai de traversier",
                   type_equipement = NA,
                   depot = "Eau libre",
                   superficie_m2 = NA,
                   volume_m3 = 4932,
                   annees  = 2017,
                   type = 'Dragage')

  ## sf object
  dragage[["qgod"]] <- list(coords) %>%
             st_polygon() %>%
             st_sfc(.,crs = 4326) %>%
             cbind(df, .) %>%
             st_sf()

  # Site de dépôt:
  ## Coordonnées du site de dépôt:49.2835; -67.5590
  coords <- matrix(c(-67.5590, 49.2835), nrow = 1)

  ## Data.frame
  df$type <- 'Depot'

  ## sf object
  depot[["qgod"]] <- st_point(coords) %>%
             st_sfc(.,crs = 4326) %>%
             cbind(df,.) %>%
             st_sf() %>%
             st_transform(32198) %>%
             st_buffer(buf) %>%
             st_transform(4326)


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Les Méchins 49º0'17.65"N; 66º58'27.11"O
  # x <- st_point(matrix(c(-66.9742, 49.0049), nrow = 1)) %>% st_sfc(crs = 4326)
  # x <- mapview(x) %>% editMap()
  coords <- rbind(c(-66.97288, 49.00407),
                  c(-66.97299, 49.00442),
                  c(-66.97355, 49.00467),
                  c(-66.97383, 49.00439),
                  c(-66.97323, 49.00391),
                  c(-66.97288, 49.00407))

  ## Data.frame
  df <- data.frame(municipalite = "Les Méchins",
                   site_dragage = "Quai de Les Méchins",
                   promoteur = "Groupe Maritime Verreault Inc.",
                   organisme = NA,
                   type_dragage = "Entretien",
                   classification = "Quai",
                   type_equipement = NA,
                   depot = "Terrestre",
                   superficie_m2 = NA,
                   volume_m3 = 4000,
                   annees  = 2016,
                   type = 'Dragage')

  ## sf object
  dragage[["qmec"]] <- list(coords) %>%
             st_polygon() %>%
             st_sfc(.,crs = 4326) %>%
             cbind(df, .) %>%
             st_sf()

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Havre de Baie-Trinité
  ## Coordonnées: 49º25'17.03"N; 67º17'5.27"O
  coords <- rbind(c(-67.28354, 49.42130),
                  c(-67.28397, 49.42101),
                  c(-67.28451, 49.42012),
                  c(-67.28539, 49.42028),
                  c(-67.28452, 49.42157),
                  c(-67.28354, 49.42130))

  ## Data.frame
  df <- data.frame(municipalite = "Baie-Trinité",
                   site_dragage = "Havre de Baie-Trinité",
                   promoteur = NA,
                   organisme = NA,
                   type_dragage = "Entretien",
                   classification = "Quai",
                   type_equipement = NA,
                   depot = "Terrestre",
                   superficie_m2 = NA,
                   volume_m3 = 3000,
                   annees  = 2016,
                   type = 'Dragage')

  ## sf object
  dragage[["hbtr"]] <- list(coords) %>%
             st_polygon() %>%
             st_sfc(.,crs = 4326) %>%
             cbind(df, .) %>%
             st_sf()


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Secteur de Baie-Comeau (ports de marchandises, quai de traversiers)

  ## ------------------------------
  ## Havre de Baie-Comeau
  coords <- rbind(c(-68.13563, 49.22962),
                  c(-68.13324, 49.23028),
                  c(-68.13186, 49.22977),
                  c(-68.13203, 49.22887),
                  c(-68.13358, 49.22840),
                  c(-68.13466, 49.22839),
                  c(-68.13563, 49.22962))

  ## Data.frame
  df <- data.frame(municipalite = "Baie-Comeau",
                   site_dragage = "Havre de Baie-Comeau",
                   promoteur = NA,
                   organisme = NA,
                   type_dragage = "Entretien",
                   classification = "Quai",
                   type_equipement = NA,
                   depot = "Eau libre",
                   superficie_m2 = NA,
                   volume_m3 = c(5100, 3328),
                   annees  = c(2017, 2014),
                   type = 'Dragage')

  ## sf object
  dragage[["hbc"]] <- list(coords) %>%
             st_polygon() %>%
             st_sfc(.,.,crs = 4326) %>%
             cbind(df, .) %>%
             st_sf()

  ## Site de dépôt de la Baie-des-Anglais
  ## Environ 1 km du quai
  ## Voir rapport: https://www.bape.gouv.qc.ca/fr/dossiers/programme-decennal-dragage-abords-quais-cargill-limitee-baie-comeau/documentation/
  ## Coordonnées du site de dépôt: 49.256618, -68.119232
  coords <- matrix(c(-68.119232, 49.256618), nrow = 1)

  ## Data.frame
  df$type <- 'Depot'

  ## sf object
  depot[["hbc"]] <- st_point(coords) %>%
             st_sfc(.,.,crs = 4326) %>%
             cbind(df,.) %>%
             st_sf() %>%
             st_transform(32198) %>%
             st_buffer(buf) %>%
             st_transform(4326)

  ## ------------------------------
  ## Quai de Cargill
  ## 49°15'17.02'' N ; 68°8'0.31'' O
  coords <- rbind(c(-68.13773, 49.24890),
                  c(-68.13546, 49.24908),
                  c(-68.13556, 49.24930),
                  c(-68.13692, 49.24920),
                  c(-68.13695, 49.24942),
                  c(-68.13527, 49.24955),
                  c(-68.13490, 49.24848),
                  c(-68.13761, 49.24834),
                  c(-68.13773, 49.24890))

  ## Data.frame
  df <- data.frame(municipalite = "Baie-Comeau",
                   site_dragage = "Quai de Cargill",
                   promoteur = NA,
                   organisme = NA,
                   type_dragage = "Entretien",
                   classification = "Quai",
                   type_equipement = NA,
                   depot = "Terrestre",
                   superficie_m2 = NA,
                   volume_m3 = 2240,
                   annees  = 2017,
                   type = 'Dragage')

  ## sf object
  dragage[["carg"]] <- list(coords) %>%
             st_polygon() %>%
             st_sfc(.,crs = 4326) %>%
             cbind(df, .) %>%
             st_sf()

  ## ------------------------------
  ## Société Alcoa - secteur de l’Anse-du moulin
  coords <- rbind(c(-68.13292, 49.25245),
                  c(-68.13055, 49.25468),
                  c(-68.13348, 49.25536),
                  c(-68.13400, 49.25424),
                  c(-68.13391, 49.25348),
                  c(-68.13292, 49.25245))

  ## Data.frame
  df <- data.frame(municipalite = "Baie-Comeau",
                   site_dragage = "Quai Société Alcoa",
                   promoteur = NA,
                   organisme = NA,
                   type_dragage = "Entretien",
                   classification = "Quai",
                   type_equipement = NA,
                   depot = "Terrestre",
                   superficie_m2 = NA,
                   volume_m3 = 56000,
                   annees  = 2017,
                   type = 'Dragage')

  ## sf object
  dragage[["alco"]] <- list(coords) %>%
             st_polygon() %>%
             st_sfc(.,crs = 4326) %>%
             cbind(df, .) %>%
             st_sf()


  # -------------------------------------------------------------------------
  ## Bind together
  dragage  <- bind_rows(dragage)
  depot <- bind_rows(depot)
  # mv <- mapview(dragage) + depot
  data0019 <- bind_rows(dragage, depot)
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0019,
           dsn = "./data/data-format/data0019-dragage.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #
}
