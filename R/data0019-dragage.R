#' Data 0019 : Sites de dragage et de dépôt
#'
#' Compilation des dragages dans l’ensemble du Saint-Laurent entre 2001 et 2016.
#'
#' @keywords dragage
#' @keywords stresseurs
#'
#' @source Environnement et Changement Climatique Canada (2020) Bilan des activités de dragage dans l’Estuaire du Saint-Laurent de 2009 à 2016. Document de travail
#' @source Ministères des Transports du Québec (2016) Compilation des dragages dans l’ensemble du Saint-Laurent entre 2001 et 2016. Document de travail
#' @source Ministères des Transports du Québec (2017) Bref portrait des activités de dragage dans la voie navigable du fleuve Saint-Laurent de 2001 à 2016. 50 + iv
#'
#' @export
#'
#' @details Cette fonction formatte les données
#'

get_data0019 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # WARNING:
  #   Les données brutes sont intégrées à 2 rapports et 1 tabulateur excel qui
  #   se prêtent mal à une intégration programmée. Le travail de formattage et
  #   de préparation initial des données brute a donc été effectué manuellement
  #   et la base de données résultante est nommée:
  #     `dragage_format_DavidBeauchesne_UniversiteLaval.csv`
  #
  # NOTE:
  #   Pour plusieurs sites de dépôts et dragage, les données sur l'entretien
  #   de la voie maritime de la garde côtière canadienne sont utilisés (id:0018)
  #
  # RESOURCES:
  #   Degrees minutes seconds to degrees decimals converter:
  #   https://www.rapidtables.com/convert/number/degrees-minutes-seconds-to-degrees.html
  #
  #   Code to visualize point, draw polygon and extract coordinates
  #   x <- st_point(matrix(c(-67.2848, 49.4214), nrow = 1)) %>%
  #        st_sfc(crs = 4326)
  #   x <- mapview(x) %>% editMap()
  #   st_coordinates(x[[1]])
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data & general parameters
  # ----------------------------------------
  # Output folder
  output <- "data0019-dragage/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # BD manuellement formatée
  dat <- read.csv(paste0(folder, "dragage_format_DavidBeauchesne_UniversiteLaval.csv"))

  # Importer données de la GCC
  secteurs <- st_read("./data/data-raw/data0018-dragage/secteurs.shp") %>%
              st_transform(4326)

  # Rayon des buffers pour les sites de depot identifiés uniquement par un point
  buf <- 100
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Geometries dredging and deposit sites
  # ----------------------------------------
  # Listes pour stocker les sites de dragage et de dépôt
  dragage <- depot <- list()

  # ==================================================================
  # Baie-Comeau
  # Havre de Baie-comeau
  # ==================================================================
  # Dragage
  coords <- rbind(c(-68.13563, 49.22962),
                  c(-68.13324, 49.23028),
                  c(-68.13186, 49.22977),
                  c(-68.13203, 49.22887),
                  c(-68.13358, 49.22840),
                  c(-68.13466, 49.22839),
                  c(-68.13563, 49.22962))

  dragage[["hbc"]] <- list(coords) %>%
             st_polygon() %>%
             st_sfc(crs = 4326) %>%
             st_sf(id_dragage = "hbc")

  # Depot
  # Site de dépôt de la Baie-des-Anglais
  # Environ 1 km du quai
  # Voir rapport: https://www.bape.gouv.qc.ca/fr/dossiers/programme-decennal-dragage-abords-quais-cargill-limitee-baie-comeau/documentation/
  # Coordonnées du site de dépôt: 49.256618, -68.119232
  coords <- matrix(c(-68.119232, 49.256618), nrow = 1)
  depot[["bda"]] <- st_point(coords) %>%
             st_sfc(crs = 4326) %>%
             st_sf(id_depot = "bda") %>%
             st_transform(32198) %>%
             st_buffer(buf) %>%
             st_transform(4326)

  # ==================================================================
  # Bécancour
  # Port de Bécancour
  # TODO
  # ==================================================================

  # ==================================================================
  # Berthier-sur-Mer
  # Havre de Berthier-sur-Mer
  # ==================================================================
  # Dragage
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

  dragage[["hbm"]] <- list(coords) %>%
             st_polygon() %>%
             st_sfc(crs = 4326) %>%
             st_sf(id_dragage = "hbm")

  # Depot
  # Site de dépôt: 46˚53'01"N / 70˚54'33"O
  coords <- rbind(c(-70.72694, 46.94006),
                  c(-70.73460, 46.93695),
                  c(-70.73619, 46.93857),
                  c(-70.72907, 46.94159),
                  c(-70.72694, 46.94006))

  depot[["dhbm"]] <- list(coords) %>%
            st_polygon() %>%
            st_sfc(crs = 4326) %>%
            st_sf(id_depot = "dhbm")


  # ==================================================================
  # Cacouna
  # Port de Gros-Cacouna
  # ==================================================================
  # Dragage
  # Centroide du port (manuel) : 47.93021664372833, -69.51570540967386
  coords <- rbind(c(-69.50984, 47.93378),
                  c(-69.51883, 47.93625),
                  c(-69.52223, 47.93105),
                  c(-69.52180, 47.92788),
                  c(-69.51387, 47.92444),
                  c(-69.50984, 47.93378))

  dragage[["pgc"]] <- list(coords) %>%
            st_polygon() %>%
            st_sfc(crs = 4326) %>%
            st_sf(id_dragage = "pgc")

  # ==================================================================
  # Cap-aux-Meules
  # Havre de Cap-aux-Meules
  # WARNING: Not done, outside the study area
  # ==================================================================

  # ==================================================================
  # Cap-aux-Meules
  # Quai CTMA
  # WARNING: Not done, outside the study area
  # ==================================================================

  # ==================================================================
  # Chenal de navigation du fleuve Saint-Laurent
  # Bécancour à Batiscan
  # TODO
  # ==================================================================

  # ==================================================================
  # Chenal de navigation du fleuve Saint-Laurent
  # Cap Santé
  # TODO
  # ==================================================================

  # ==================================================================
  # Chenal de navigation du fleuve Saint-Laurent
  # Champlain à Deschambault
  # TODO
  # ==================================================================

  # ==================================================================
  # Chenal de navigation du fleuve Saint-Laurent
  # Lac Saint-Pierre
  # TODO
  # ==================================================================

  # ==================================================================
  # Chenal de navigation du fleuve Saint-Laurent
  # Sorel
  # TODO
  # ==================================================================

  # ==================================================================
  # Chenal de navigation du fleuve Saint-Laurent
  # Traverse Nord I.O.
  # TODO
  # ==================================================================

  # ==================================================================
  # Chenal de navigation du fleuve Saint-Laurent
  # Trois-Rivière à Bécancour
  # TODO
  # ==================================================================

  # ==================================================================
  # Gaspé
  # WARNING: Not done, outside the study area
  # ==================================================================

  # ==================================================================
  # Hudson/Oka
  # Chenal Hudson-Oka
  # WARNING: Not done, outside the study area
  # ==================================================================

  # ==================================================================
  # Ile-aux-Coudres
  # Quai de l'Île-aux-Coudres (Desserte)
  # ==================================================================
  # Dragage
  # Quai de l’Isle-aux-Coudres : 47°25'14.75'' N, 70°23'32.48'' O
  coords <- matrix(c(-70.39236, 47.42076), nrow = 1)
  dragage[["qiac"]] <- st_point(coords) %>%
             st_sfc(crs = 4326) %>%
             st_sf(id_dragage = "qiac") %>%
             st_transform(32198) %>%
             st_buffer(buf) %>%
             st_transform(4326)

  # Depot
  coords <- rbind(c(-70.38958, 47.4391), # 47°26,346’ N, 70°23,375’ O
                  c(-70.3865, 47.4391), # 47°26,346’ N, 70°23,190’ O
                  c(-70.3865, 47.4375), # 47°26,250’ N, 70°23,190’ O
                  c(-70.38958, 47.4375), # 47°26,250’ N, 70°23,375’ O
                  c(-70.38958, 47.4391))

  depot[["dqiac"]] <- list(coords) %>%
             st_polygon() %>%
             st_sfc(crs = 4326) %>%
             st_sf(id_depot = "dqiac")


  # ==================================================================
  # Ile-aux-Grues
  # Desserte
  # ==================================================================
  # Dragage
  # Position du quai : 47° 3'18.75"N  70°31'54.43"O
  coords <- matrix(c(-70.53179, 47.05521), nrow = 1)
  dragage[["qiag"]] <- st_point(coords) %>%
             st_sfc(crs = 4326) %>%
             st_sf(id_dragage = "qiag") %>%
             st_transform(32198) %>%
             st_buffer(buf) %>%
             st_transform(4326)

  # Depot
  coords <- rbind(c(-70.49969, 47.05239), # 47°03’ 08.6’’ N / 70°29’58,9’’ O
                  c(-70.49767, 47.05125), # 47°03’ 04,5’’ N / 70°29’ 51,6’’ O
                  c(-70.49936, 47.04986), # 47°02’ 59,5’’ N / 70°29’ 57,7’’ O
                  c(-70.50139, 47.05103), # 47°03’ 03,7’’ N / 70°30’ 05,0’’ O
                  c(-70.49969, 47.05239))

  depot[["dqiag"]] <- list(coords) %>%
             st_polygon() %>%
             st_sfc(crs = 4326) %>%
             st_sf(id_depot = "dqiag")

  # ==================================================================
  # Îles-de-la-Madeleine
  # Chenal de Cap-aux-Meules
  # WARNING: Not done, outside the study area
  # ==================================================================

  # ==================================================================
  # Îles-de-la-Madeleine
  # Chenal de navigation de la Grande Entrée
  # WARNING: Not done, outside the study area
  # ==================================================================

  # ==================================================================
  # Matane
  # Port de Matane
  # ==================================================================
  # Dragage
  coords <- rbind(c(-67.57563, 48.83775),
                  c(-67.58122, 48.84495),
                  c(-67.58095, 48.84660),
                  c(-67.57995, 48.84761),
                  c(-67.57752, 48.84912),
                  c(-67.57005, 48.84175),
                  c(-67.57563, 48.83775))

  dragage[["pmat"]] <- list(coords) %>%
            st_polygon() %>%
            st_sfc(crs = 4326) %>%
            st_sf(id_dragage = "pmat")

  # Depot
  # Coordonnées du site de dépôt: 48°51’ 35’’ N; 67°35’50’’ O.
  coords <- matrix(c(48.85972, -67.59722), nrow = 1)
  depot[["dpmat"]] <- st_point(coords) %>%
             st_sfc(crs = 4326) %>%
             st_sf(id_dragage = "dpmat") %>%
             st_transform(32198) %>%
             st_buffer(buf) %>%
             st_transform(4326)


  # ==================================================================
  # Matane
  # Marina de Matane
  # ==================================================================
  # Dragage
  coords <- rbind(c(-67.52713, 48.85286),
                  c(-67.52950, 48.85287),
                  c(-67.53087, 48.85256),
                  c(-67.53072, 48.85207),
                  c(-67.52710, 48.85228),
                  c(-67.52713, 48.85286))

  dragage[["mmat"]] <- list(coords) %>%
            st_polygon() %>%
            st_sfc(crs = 4326) %>%
            st_sf(id_dragage = "mmat")

  # Depot
  # Même que le Port de Matane: dpmat


  # ==================================================================
  # Montréal
  # Port de Montréal
  # TODO
  # ==================================================================

  # ==================================================================
  # Rimouski
  # Marina de Rimouski
  # ==================================================================
  # Dragage
  coords <- rbind(c(-68.50995, 48.47923),
                  c(-68.51281, 48.48108),
                  c(-68.51503, 48.48117),
                  c(-68.51602, 48.48049),
                  c(-68.51195, 48.47784),
                  c(-68.50995, 48.47923))

  dragage[["mrim"]] <- list(coords) %>%
            st_polygon() %>%
            st_sfc(crs = 4326) %>%
            st_sf(id_dragage = "mrim")

  # Depot
  coords <- rbind(c(-68.55113, 48.52129), # 68° 33' 04.06" W; 48° 31' 16.65" N
                  c(-68.54856, 48.51892), # 68° 32' 54.80" W; 48° 31' 08.11" N
                  c(-68.55212, 48.51721), # 68° 33' 07.64" W; 48° 31' 01.96" N
                  c(-68.5547, 48.51958), # 68° 33' 16.91" W; 48° 31' 10.49" N
                  c(-68.55113, 48.52129))

  depot[["dmrim"]] <- list(coords) %>%
            st_polygon() %>%
            st_sfc(crs = 4326) %>%
            st_sf(id_depot = "dmrim")


  # ==================================================================
  # Rimouski
  # Port de Rimouski - Bassin est
  # ==================================================================
  # Dragage
  # Port: https://buyandsell.gc.ca/cds/public/2018/07/19/03b09a391aa4b24241247b8a1c2cb162/ABES.PROD.PW_QCM.B008.B17442.ATTA002.PDF
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

  dragage[["prim"]] <- list(coords) %>%
            st_polygon() %>%
            st_sfc(crs = 4326) %>%
            st_sf(id_dragage = "prim")

  # Depot
  # Même que la marina: dmrim


  # ==================================================================
  # Rivière-du-Loup
  # Desserte
  # ==================================================================

  # ==================================================================
  # Rivière-du-Loup
  # Marina de Rivière-du-Loup
  # ==================================================================

  # ==================================================================
  # Rivière-du-Loup
  # Parc maritime de Rivière-du-Loup
  # ==================================================================

  # ==================================================================
  # Saint-Jean-Port-Joli
  # Parc nautique de Saint-Jean-Port-Joli
  # ==================================================================

  # ==================================================================
  # Saint-Joseph-de-la-Rive
  # Desserte
  # ==================================================================
  # Dragage
  # Quai de Saint-Joseph-de-la-Rive : 47.448753, -70.365030
  coords <- matrix(c(-70.365030, 47.448753), nrow = 1)
  dragage[["qsjr"]] <- st_point(coords) %>%
             st_sfc(crs = 4326) %>%
             st_sf(id_dragage = "qsjr") %>%
             st_transform(32198) %>%
             st_buffer(buf) %>%
             st_transform(4326)

  # Depot
  # Même que pour l'îles au coudres id: dqiac


  # ==================================================================
  # Saint-Joseph-de-Sorel - zone portuaire QIT-Fer et Titane inc.
  # TODO
  # ==================================================================

  # ==================================================================
  # Saint-Laurent-de-L'Île-D'Orléans
  # Port de refuge de St-Laurent-de-L'Île-D'Orléans
  # ==================================================================

  # ==================================================================
  # Saint-Michel-de-Bellechasse
  # Halte nautique de Saint-Michel-de-Bellechasse
  # ==================================================================

  # ==================================================================
  # Sept-Îles
  # Bassin des remorqueurs de la Mine IOC
  # WARNING: Not done, outside the study area
  # ==================================================================

  # ==================================================================
  # Sept-Îles
  # Installations portuaires de la compagnie IOC
  # WARNING: Not done, outside the study area
  # ==================================================================

  # ==================================================================
  # Sept-Îles - Quais 2
  # WARNING: Not done, outside the study area
  # ==================================================================

  # ==================================================================
  # Sorel-Tracy - Marina de Saurel
  # ==================================================================

  # ==================================================================
  # Sorel-Tracy - Port de Sorel-Tracy
  # TODO
  # ==================================================================

  # ==================================================================
  # Tadoussac - Desserte
  # ==================================================================

  # ==================================================================
  # Valleyfield - Port de Valleyfield
  # TODO
  # ==================================================================

  # ==================================================================
  # Voie Maritime
  # WARNING: Not done, outside the study area
  # ==================================================================



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
