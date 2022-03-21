#' Navigation
#'
#' Couche de données transformées pour la navigation dans le Saint-Laurent
#'
#' @keywords navigation
#' @keywords stresseurs
#'
#' @export
#'
#' @details Cette fonction importe et formatte les données pour l'analyse d'effets cumulatifs
#'

st_navigation <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # AIS data
  # -------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load vessel type dataset and shipping tracks
  load_format("data0020") # Vessel index
  load_format("data0021") # AIS

  # ---------
  data0020$NTYPE <- gsub(" ", ".", data0020$NTYPE)
  data0020$NTYPE <- gsub("/", ".", data0020$NTYPE)
  data0020$NTYPE <- gsub("-", ".", data0020$NTYPE)
  vessel_type <- unique(data0020$NTYPE)

  # Add vessel type to ais data
  ais <- data0021 %>%
         left_join(data0020, 'MMSI')

  # Save memory
  rm(data0021)
  
  # -----
  # Export this file 
  st_write(obj = ais,
           dsn = './data/data-raw/data0021-navigation/ais.geojson',
           delete_dsn = TRUE,
           quiet = TRUE)
  # _____________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Update metadata
  # ----------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  meta <- load_metadata("int_st_navigation")

  # -----
  meta$rawData <- c("0020","0021")

  # -----
  meta$dataDescription$spatial$extent <- st_bbox(ais) # extent of 0021 includes 0028

  # -----
  years <- unique(ais$year)
  meta$dataDescription$temporal$start <- min(years)
  meta$dataDescription$temporal$end <- max(years)

  # -----
  obs <- st_drop_geometry(ais) %>% group_by(year) %>% summarize(total = n())
  meta$dataDescription$observations$total <- sum(obs$total)
  meta$dataDescription$observations$moyenne <- round(mean(obs$total), 0)
  meta$dataDescription$observations$sd <- round(sd(obs$total), 0)
  rm(obs)

  # -----
  obs <- st_drop_geometry(ais)
  observations <- data.frame(accronyme = vessel_type, observations = 0)
  for(i in 1:length(vessel_type)) {
    uid <- ais$NTYPE == vessel_type[i]
    observations$observations[i] <- sum(uid, na.rm = TRUE)
  }
  rm(obs)
  # _____________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # AIS segments
  # -------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  ais <- ais_segment(ais)
  # _____________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Remove tracks significantly overlapping with land
  # -------------------------------------------------
  #
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load data from `ceanav` package
  data(aoi)
  basemap("egsl")

  # Union
  sa <- bind_rows(aoi, egsl) %>%
        st_union()

  # Add buffer around study area to make sure I do not remove data in ports
  tc <- st_buffer(aoi, 2000)
  stl <- st_buffer(egsl, 2000)

  # Combine and union
  sabuf <- bind_rows(tc, stl) %>%
           st_union()

  # Bounding box of sa as polygon
  bb <- st_bbox(sa) %>%
        st_as_sfc()

  # Difference with bounding box
  sa <- st_difference(bb, sabuf)

  # Intersect points with study area (This should be discussed and reevaluated)
  uid <- st_intersects(sa, ais) %>%
         unlist()
  ais <- ais[-uid, ]

  # -----
  meta$dataDescription$segments <- nrow(ais)
  
  # _____________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Vessel types
  # -------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Add vessel type to ais data (again)
  ais <- ais %>%
         left_join(data0020[,c("MMSI","NTYPE")], 'MMSI')

  # -----
  # Export this object 
  st_write(obj = ais,
           dsn = './data/data-raw/data0021-navigation/ais_segments.geojson',
           delete_dsn = TRUE,
           quiet = TRUE)
  # -----

  # Divide vessel data by type
  vessels <- list()
  for(i in 1:length(vessel_type)) {
    uid <- ais$NTYPE == vessel_type[i]
    vessels[[i]] <- ais[uid, ]
  }
  names(vessels) <- vessel_type


  # _____________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Shipping intensity
  # -------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  data(grid1p)

  system.time({
  grd <- list()
  # Intensity for each vessel type (~15 minutes to run on my laptop)
  for(i in 1:length(vessels)) {
    grd[[i]] <- st_intersects(grid1p, vessels[[i]]) %>%
           lapply(length) %>%
           unlist() %>%
           ifelse(. == 0, NA, .) %>%
           mutate(grid1p, intensity = .)
  }
  })

  # In matrix
  df <- matrix(NA, nrow = nrow(grid1p), ncol = length(vessel_type), dimnames = list(c(), vessel_type))
  for(i in 1:length(grd)) df[,i] <- grd[[i]]$intensity

  # As single sf object
  navigation <- cbind(grid1p, df)

  # Remove others and fishing
  navigation <- dplyr::select(navigation, -OTHERS, -FISHING)

  # Set cells that do not touch water to NA
  data(aoi)
  uid <- st_intersects(aoi, grid1p) %>% unlist()
  for(i in 1:(ncol(navigation)-1)) navigation[-uid,i] <- NA

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Parc marin
  # ----------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  load_format("data0028") # Parc marin observation
  meta$rawData <- c(meta$rawData, "0028")

  # -----
  library(raster)
  obs <- st_rasterize(data0028[,'nb_transit']) %>%
         as("Raster") %>%
         exact_extract(grid1p, 'sum', progress = FALSE) %>%
         ifelse(. == 0, NA, .)

  # -----
  navigation$Observation <- obs
  # --------------------------------------------------------------------------------


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Navigation portuaire
  # ----------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  load_format("data0041")
  load_format("data0047")
  meta$rawData <- c(meta$rawData, "0041", "0047")

  # WARNING:
  # Retrait de la zone marine pour les données de zones industrialo-portuaires
  # Ceci est fait surtout pour Bécancour, puisque la zone s'étend jusqu'au milieu
  # du chenal de navigation. On ajoute ensuite un buffer pour identifier la zone d'influence marine
  data0047 <- st_difference(data0047, aoi)

  # -----
  port <- bind_rows(data0041, data0047) %>%
          st_buffer(200) %>%
          st_union() %>%
          st_intersection(aoi) %>%
          st_cast("POLYGON") %>%
          st_sf()

  # ------------------------------------
  # Number of transits per port area
  # ------------------------------------
  uid <- st_intersects(port, ais)

  # -----
  ntransit <- list()
  for(i in 1:length(uid)) ntransit[[i]] <- length(uid[[i]])

  # -----
  port$port <- unlist(ntransit)

  # -----
  # Navigation portuaire - nombre de transit total
  meta$dataDescription$navigationPortuaire$transits <- sum(port$port, na.rm = TRUE)


  # ------------------------------------
  # Integrate to study grid
  # ------------------------------------
  grid1p$id <- 1:nrow(grid1p)

  # -----
  uid <- st_intersects(port, grid1p)

  # -----
  dat <- list()
  for(i in 1:length(uid)) {
    dat[[i]] <- data.frame(id = uid[[i]], port = port$port[i])
  }
  dat <- bind_rows(dat)

  # -----
  dat <- dat %>%
         group_by(id) %>%
         summarise(port = max(port))

  # -----
  port <- left_join(grid1p, dat, by = 'id') %>%
          dplyr::select(-id)

  # -----
  navigation$navigation_portuaire <- port$port

  # ------------------------------------------------------------------------- #

  # --------------------------------------------------------------------------------

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Update metadata
  # ----------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #

  # -----
  dat <- data.frame(accronyme = c("CARGO","CONTAINER","DRY.BULK","FERRY.RO.RO",
                                  "GOVERNMENT.RESEARCH","Observation","PASSENGER",
                                  "PLEASURE.VESSELS","SPECIAL.SHIPS","TANKER",
                                  "TUGS.PORT","navigation_portuaire"),
                    english = c("Cargo","Containers","Dry bulk","Ferry / ro-ro",
                                "Government / research","Marine mammals observation",
                                "Passenger","Pleasure vessels","Special ships",
                                "Tanker","Tugboat / port", "Port shipping"),
                    francais = c("Cargo","Porte-conteneurs","Cargaison sèche",
                                 "Traversier / roulier","Gouvernement / recherche",
                                 "Observation mammifères marins","Passager",
                                 "Navires de plaisance","Navires spéciaux",
                                 "Pétrolier","Remorqueur / port",
                                 "Navigation portuaire"))

  # ---
  # Shipping segments
  seg <- data.frame(accronyme = names(vessels), segments = 0)
  for(i in 1:length(vessels)) seg$segments[i] <- nrow(vessels[[i]])
  dat <- left_join(dat, seg, by = "accronyme")

  # ---
  # Info on transits per km2
  nav <- st_drop_geometry(navigation)
  dat2 <- data.frame(accronyme = colnames(nav), transit_km2 = 0, source = NA)
  for(i in 1:ncol(nav)) dat2$transit_km2[i] <- sum(nav[,i], na.rm = TRUE) / (st_area(aoi) * 1e-6)
  uid <- !dat2$accronyme %in% c("Observation","navigation_portuaire")
  dat2$transit_km2[uid] <- dat2$transit_km2[uid] / length(years) # only 1 year of observation
  dat2$source[uid] <- "0020,0021"
  dat2$source[dat2$accronyme == "Observation"] <- "0028"
  dat2$source[dat2$accronyme == "navigation_portuaire"] <- "0041,0047"
  dat <- left_join(dat, dat2, by = "accronyme")

  # ---
  dat2 <- data.frame(accronyme = names(vessels), boats = NA)
  for(i in 1:length(vessels)) dat2$boats[i] <- length(unique(vessels[[i]]$MMSI))
  dat <- left_join(dat, dat2, by = "accronyme")

  # ---
  # WARNING: Number of observations are calculated earlier in script
  dat <- left_join(dat, observations, by = "accronyme")

  # -----
  meta$dataDescription$categories$accronyme <- dat$accronyme
  meta$dataDescription$categories$english <- dat$english
  meta$dataDescription$categories$francais <- dat$francais
  meta$dataDescription$categories$source <- dat$source
  meta$dataDescription$categories$observations <- dat$observations
  meta$dataDescription$categories$segments <- dat$segments
  meta$dataDescription$categories$transit_km2 <- dat$transit_km2
  meta$dataDescription$categories$boats <- dat$boats

  # -----
  meta$dataDescription$categories$description <- c(
    "Trafic maritime de navires de commerce transportant des marchandises sous diverses
      formes.",
    "Trafic maritime de navires de commerce transportant des marchandises exclusivement
      dans des conteneurs de transport.",
    "Trafic maritime de navires de commerce transportant des marchandises en vrac,
      e.g. du grain et du charbon.",
    "Trafic maritime de navires gouvernementaux et de navires de recherche, tels des embarcations 
      militaires, des navires de patrouille de la Garde côtière et des navires de recherche 
      scientifique.",
    "Activités d’observation en mer des mammifères marins sur des embarcations avec
      permis de classe 1.",
    "Trafic maritime de navires dédiés au transport de passagers, et de traversiers
      transportant des passagers et/ou des véhicules.",
    "Trafic maritime de navires récréatifs.",
    "Trafic maritime de navires divers tels des dragueurs, des poseurs de câbles,
      des plateformes, des navires de forage, etc.",
    "Trafic maritime de navires transportant des produits pétroliers en vrac.",
    "Trafic maritime de navires remorqueurs.",
    "Activités de navigation qui ont lieu près des installations portuaires telles
      les ports et les marinas."
  )

  meta$dataDescription$categories$description_en <- c(
    "Marine traffic of commercial vessels transporting goods in various forms.",
    "Marine traffic of commercial vessels transporting goods exclusively in transport containers.",
    "Marine traffic of commercial vessels transporting goods in bulk, i.e. grain and coal.",
    "Marine traffic of government and research vessels, such as military craft, Coast Guard patrol vessels and scientific research vessels.",
    "Marine mammal observation activities at sea on craft with a class 1 permit.",
    "Marine traffic of passenger transportation vessels and ferries carrying passengers and/or vehicles.",
    "Recreational vessel marine traffic.",
    "Marine traffic of miscellaneous vessels such as dredgers, cable layers, platforms, drilling vessels, etc.",
    "Marine traffic of vessels transporting petroleum products in bulk",
    "Marine traffic of tugboats.",
    "Navigation activities that take place near port facilities such as ports and marinas."
  )

  # --- For proper referencing in markdown syntax
  meta$dataDescription$categories$mdref <- modif_md(meta$dataDescription$categories$accronyme)


  # --------------------------------------------------------------------------------

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export
  # ------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # -----
  write_yaml(meta, "./data/data-metadata/int_st_navigation.yml")

  # -----
  st_write(obj = navigation,
           dsn = "./data/data-integrated/st_navigation.geojson",
           delete_dsn = TRUE,
           quiet = TRUE)
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Clean global environment
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  clean()
  # ------------------------------------------------------------------------- #}

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # 2022-02-10
  # Combiner Passagers & Traversiers 
  # Je prends la décision de le faire en fin de script pour laiser l'option de 
  # revenir en arrière facilement au besoin. J'aurais pu modifier tout le script, 
  # mais à ce stade-ci je préfère ajouter du code pour modifier ce qui a déjà été fait. 
  # Ceci est possible parce que la combinaison peut se faire simplement par une 
  # addition des deux catégories, alors c'est simple de le faire à postériori.
  # ------
  # 
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Modifier données intégrées 
  load_integrated("navigation")
  navigation$PASSENGER.FERRY.RO.RO <- rowSums(navigation[,c("PASSENGER","FERRY.RO.RO"), drop = TRUE], na.rm = TRUE)
  navigation <- dplyr::select(navigation, -PASSENGER, -FERRY.RO.RO)
  
  # Modifier les métadonnées
  meta <- load_metadata("int_st_navigation")
  pas <- which(meta$dataDescription$categories$accronyme == "PASSENGER")
  fer <- which(meta$dataDescription$categories$accronyme == "FERRY.RO.RO")
  
  # Nouvelle catégorie
  meta$dataDescription$categories$accronyme[pas] <- "PASSENGER.FERRY.RO.RO"
  meta$dataDescription$categories$english[pas] <- "Passenger / ferry / ro-ro"
  meta$dataDescription$categories$francais[pas] <- "Passager / traversier / roulier"
  meta$dataDescription$categories$boats[pas] <- sum(meta$dataDescription$categories$boats[c(pas,fer)])
  meta$dataDescription$categories$observations[pas] <- sum(meta$dataDescription$categories$observations[c(pas,fer)])
  meta$dataDescription$categories$segments[pas] <- sum(meta$dataDescription$categories$segments[c(pas,fer)])
  meta$dataDescription$categories$transit_km2[pas] <- sum(meta$dataDescription$categories$transit_km2[c(pas,fer)])
  meta$dataDescription$categories$mdref[pas] <- "PASSENGERFERRYRORO"
  meta$dataDescription$categories$description[pas] <- "Trafic maritime de navires dédiés au transport de passagers, et de traversiers transportant des passagers et/ou des véhicules."

  # Retirer catégorie
  meta$dataDescription$categories$accronyme <- meta$dataDescription$categories$accronyme[-fer]  
  meta$dataDescription$categories$english <- meta$dataDescription$categories$english[-fer]  
  meta$dataDescription$categories$francais <- meta$dataDescription$categories$francais[-fer]  
  meta$dataDescription$categories$boats <- meta$dataDescription$categories$boats[-fer]  
  meta$dataDescription$categories$observations <- meta$dataDescription$categories$observations[-fer] 
  meta$dataDescription$categories$segments <- meta$dataDescription$categories$segments[-fer] 
  meta$dataDescription$categories$transit_km2 <- meta$dataDescription$categories$transit_km2[-fer] 
  meta$dataDescription$categories$mdref <- meta$dataDescription$categories$mdref[-fer]  
  meta$dataDescription$categories$description <- meta$dataDescription$categories$description[-fer] 
  meta$dataDescription$categories$source <- meta$dataDescription$categories$source[-fer] 

  # --------------------------------------------------------------------------------

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export
  # ------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # -----
  write_yaml(meta, "./data/data-metadata/int_st_navigation.yml")

  # -----
  st_write(obj = navigation,
           dsn = "./data/data-integrated/st_navigation.geojson",
           delete_dsn = TRUE,
           quiet = TRUE)
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Clean global environment
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  clean()
  # ------------------------------------------------------------------------- #}

}
