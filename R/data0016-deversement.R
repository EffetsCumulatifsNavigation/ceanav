#' Data 0016 : Déversements accidentels
#'
#' Distribution des sites de déversements accidentels répertoriés dans le Saint-Laurent
#'
#' @keywords déversement accidentel
#' @keywords stresseurs
#'
#' @source Garde côtière canadienne, Direction des Interventions, Intervention environnementale, région du Centre. Base de données SGI-WEB -2007-2020.
#'
#' @export
#'
#' @details Cette fonction formatte les données
#'

get_data0016 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data

  # Output folder
  output <- "data0016-deversement/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # WARNING: Data transfered physically, no cloud access currently

  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import and format data
  # ----------------------------------------
  # aspects to respect
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load data
  dev <- read.csv(paste0(folder, '20210223_Demande_UL.csv')) %>%
         drop_na("ID_CAS")

  # ------------------
  # Longitude
  ## Certaines longitudes sont positives alors qu'elles devraient être négatives
  uid <- c(8017, 8944, 9227, 9801, 9804, 10057, 10071, 10107, 10169, 10272,
           10288, 10444, 10458, 10497, 10499, 10538, 10540, 10555, 10585,
           10749, 10752, 11282, 11349, 11569, 11590, 11678, 11770, 11780,
           11782, 12055, 12089)
  dev$LONGITUDE[dev$ID_CAS %in% uid] <- -dev$LONGITUDE[dev$ID_CAS %in% uid]

  # ------------------
  # Transform Volume data names
  dev$VOLUME_DEVERSE <- gsub('100- 1000 litres', '100 - 1000 litres', dev$VOLUME_DEVERSE)

  # ------------------
  # Transform Volume of 0 l to 0 - 100 l 
  uid <- dev$VOLUME_DEVERSE == "0 litre"
  dev$VOLUME_DEVERSE[uid] <- "0 - 100 litres"

  # ------------------
  # Erroneous locations
  uid <- c(6871, 7686, 7716, 8377, 9343, 9883, 9916, 10057, 10188, 11809,
           11936, 11940)
  dat <- data.frame(ID_CAS = numeric(length(uid)), LATITUDE = numeric(length(uid)), LONGITUDE = numeric(length(uid)))
  dat[1,] <- c(6871, 50.16501, -66.46168)
  dat[2,] <- c(7686, 46.78328, -71.20790)
  dat[3,] <- c(7716, NA, NA)
  dat[4,] <- c(8377, 45.52914, -73.54195)
  dat[5,] <- c(9343, 48.49217, -68.49510)
  dat[6,] <- c(9883, 48.33589, -70.87230)
  dat[7,] <- c(9916, 46.78303, -71.20599)
  dat[8,] <- c(10057, NA, NA)
  dat[9,] <- c(10188, 46.28346, -72.69085)
  dat[10,] <- c(11809, 48.01778, -65.25655)
  dat[11,] <- c(11936, 48.33942, -70.87594)
  dat[12,] <- c(11940, NA, NA)

  # Make modifications
  for(i in 1:nrow(dat)) {
    uid <- which(dev$ID_CAS == dat$ID_CAS[i])
    dev[uid, c('LONGITUDE','LATITUDE')] <- dat[i, c('LONGITUDE','LATITUDE')]
  }

  # ------------------
  # Remove sensitive information
  dev <- select(dev, -NOM_CAS, -NOM)

  # ------------------
  # Remove NAs (just to respect initial structure)
  uid <- is.na(dev$LONGITUDE)
  dev[uid, c('LONGITUDE','LATITUDE')] <- ''

  # ------------------
  # Export
  ## That file will be sharable
  write.csv(dev, file = paste0(folder, 'deversements_modifs.csv'), row.names = FALSE)

  # ------------------
  # Spatial object
  data0016 <- read.csv(paste0(folder, 'deversements_modifs.csv'), na.strings = '') %>%
              drop_na('LATITUDE') %>%
              st_as_sf(coords = c('LONGITUDE','LATITUDE'), crs = 4326, remove = FALSE)

  # Transform projection
  data0016 <- st_transform(data0016, crs = global_parameters()$crs)
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0016,
           dsn = "./data/data-format/data0016-deversement.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #
}
