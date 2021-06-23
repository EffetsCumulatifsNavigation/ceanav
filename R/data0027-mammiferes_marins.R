#' Data 0027 : Mammifères marins
#'
#' Données cartographiques : Navires et baleines de l’Atlantique Nord-Ouest : Guide à l’intention de l’industrie maritime, 2e édition
#'
#' @keywords béluga du Saint-Laurent
#' @keywords rorqual bleu
#' @keywords baleine à bec commune
#' @keywords rorqual commun
#' @keywords rorqual à bosse
#' @keywords tortue luth
#' @keywords petit rorqual
#' @keywords baleine noire de l'atlantique Nord
#' @keywords rorqual boréal
#' @keywords cachalot macrocéphale
#' @keywords mammifères marins
#' @keywords composante valorisée
#'
#' @source Le WWF-Canada et le Réseau d’observation de mammifères marins. Données cartographiques : Navires et baleines de l’Atlantique Nord-Ouest : Guide à l’intention de l’industrie maritime, 2e édition. 2021. Données disponibles sur le site https://www.navigationbaleines.ca/fr/accueil/
#'
#' @export
#'
#' @details Cette fonction formatte les données
#'

get_data0027 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # WARNING:
  message("Id 0027: Ces données doivent être accompagnées d'une mise en garde sur leur utilisation. À établir avec le ROMM et WWF Canada")

  # Output folder
  output <- "data0027-mammiferes_marins/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # WARNING: Data transfered physically, no cloud access currently
  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'CetaceanHeatmapRasters/'))) {
    # Unzip
    unzip(zipfile = paste0(folder, 'CetaceanHeatmapRasters.zip'), exdir = folder)
  }
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import and format data
  # ----------------------------------------
  #
  # WARNING:
  # The methods to generate the data is described in the report from WWF.
  # It is important to note that the resulting maps do not represent occurrence
  # probabilities, but rather a proportion of total sightings for each species
  # considered.
  #
  # The dataset shared contains "raw" data and log transformed data. I will use
  # the log-transformed data for this project.
  # ----------------------------------------
  files <- dir(paste0(folder, 'CetaceanHeatmapRasters'), pattern = ".tif$", full.names = TRUE)

  # Species names
  nm <- data.frame(nm = c("Beluga","Rorqual_bleu","Baleine_a_bec_commune",
                        "Rorqual_commun","Rorqual_a_bosse","Tortue_luth",
                        "Petit_rorqual","Baleine_noire","Rorqual_boreal",
                        "Cachalot_macrocephale"),
                 fr = c("Béluga du Saint-Laurent","Rorqual bleu",
                        "Baleine à bec commune","Rorqual commun",
                        "Rorqual à bosse","Tortue luth","Petit rorqual",
                        "Baleine noire de l'atlantique Nord",
                        "Rorqual boréal","Cachalot macrocéphale"),
                 en = c("St. Lawrence beluga whale","Blue whale",
                        "Northern bottlenose whale","Fin whale",
                        "Humpback whale","Leatherback turtle","Minke whale",
                        "North Atlantic right whale","Sei whale",
                        "Sperm whale"))

  # ID of log-transformed data to load
  uid <- 4:13
  data0027 <- list()
  for(i in 1:length(uid)) {
    # Load raster
    dat <- read_stars(files[uid[i]])

    # 0 as NA
    dat[dat == 0] <- NA

    # Export in list
    data0027[[nm$nm[i]]] <- dat
  }

  # As a single stars object by including each marine mammal as "bands"
  dat <- c(data0027[[1]],
           data0027[[2]],
           data0027[[3]],
           data0027[[4]],
           data0027[[5]],
           data0027[[6]],
           data0027[[7]],
           data0027[[8]],
           data0027[[9]],
           data0027[[10]],
           along = "z") # This becomes the 3rd dimension of the raster

  # Set the names of the "z" dimension to species names
  dat <- st_set_dimensions(dat, 3, values = nm$nm)

  # Single object
  data0027 <- dat
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  write_stars(data0027,
              dsn = "./data/data-format/data0027-mammiferes_marins.tif",
              driver = "GTiff", delete_dsn = TRUE)

  # RData
  save(data0027, file = "./data/data0027.RData")
  # _________________________________________________________________________ #
}
