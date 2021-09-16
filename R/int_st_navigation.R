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
  data_metadata <- c("0020","0021")

  # ---------
  data0020$NTYPE <- gsub(" ", ".", data0020$NTYPE)
  data0020$NTYPE <- gsub("/", ".", data0020$NTYPE)
  data0020$NTYPE <- gsub("-", ".", data0020$NTYPE)
  vessel_type <- unique(data0020$NTYPE)

  # Add vessel type to ais data
  ais <- data0021 %>%
         left_join(data0020, 'MMSI')

  # Divide vessel data by type
  vessels <- list()
  for(i in 1:length(vessel_type)) {
    uid <- ais$NTYPE == vessel_type[i]
    vessels[[i]] <- ais[uid, ]
  }
  names(vessels) <- vessel_type

  # -------------------------
  # Evaluate intensity
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

  # Remove others
  navigation <- select(navigation, -OTHERS, -FISHING)

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
  data_metadata <- c(data_metadata, "0028")

  # -----
  library(raster)
  obs <- st_rasterize(data0028[,'nb_transit']) %>%
         as("Raster") %>%
         exact_extract(grid1p, 'sum', progress = FALSE) %>%
         ifelse(. == 0, NA, .)

  # -----
  navigation$Observation <- obs

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Update metadata
  # ----------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  meta <- load_metadata("int_st_navigation")

  # -----
  meta$rawData <- data_metadata

  # -----
  meta$dataDescription$spatial$extent <- st_bbox(data0021) # extent of 0021 includes 0028

  # -----
  years <- unique(data0021$year)
  meta$dataDescription$temporal$start <- min(years)
  meta$dataDescription$temporal$end <- max(years)

  # -----
  obs <- st_drop_geometry(data0021) %>% group_by(year) %>% summarize(total = n())
  meta$dataDescription$observations$total <- sum(obs$total)
  meta$dataDescription$observations$moyenne <- round(mean(obs$total), 0)
  meta$dataDescription$observations$sd <- round(sd(obs$total), 0)


  # -----
  dat <- data.frame(accronyme = c("CARGO","CONTAINER","DRY.BULK","FERRY.RO.RO",
                                  "GOVERNMENT.RESEARCH","Observation","PASSENGER",
                                  "PLEASURE.VESSELS","SPECIAL.SHIPS","TANKER",
                                  "TUGS.PORT"),
                    english = c("Cargo","Containers","Dry bulk","Ferry / ro-ro",
                                "Government / research","Marine mammals observation",
                                "Passenger","Pleasure vessels","Special ships",
                                "Tanker","Tugboat / port"),
                    francais = c("Cargo","Porte-conteneurs","Cargaison sèche",
                                 "Traversier / roulier","Gouvernement / recherche",
                                 "Observation mammifères marins","Passager",
                                 "Navires de plaisance","Navires spéciaux",
                                 "Pétrolier","Remorqueur / port"))

  # ---
  # Info on transits per km2
  nav <- st_drop_geometry(navigation)
  dat2 <- data.frame(accronyme = colnames(nav), transit = 0, source = NA)
  for(i in 1:ncol(nav)) dat2$transit[i] <- sum(nav[,i], na.rm = TRUE) / (st_area(aoi) * 1e-6)
  uid <- dat2$accronyme != "Observation"
  dat2$transit[uid] <- dat2$transit[uid] / length(years) # only 1 year of observation
  dat2$source[uid] <- "0020,0021"
  dat2$source[!uid] <- "0028"
  dat <- left_join(dat, dat2, by = "accronyme")

  # ---
  dat2 <- data.frame(accronyme = names(vessels), boats = NA)
  for(i in 1:length(vessels)) dat2$boats[i] <- length(unique(vessels[[i]]$MMSI))
  dat <- left_join(dat, dat2, by = "accronyme")

  # -----
  meta$dataDescription$categories$accronyme <- dat$accronyme
  meta$dataDescription$categories$english <- dat$english
  meta$dataDescription$categories$francais <- dat$francais
  meta$dataDescription$categories$source <- dat$source
  meta$dataDescription$categories$transit <- dat$transit
  meta$dataDescription$categories$boats <- dat$boats
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
