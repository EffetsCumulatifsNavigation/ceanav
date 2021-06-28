#' Data 0021 : Données AIS satellite de navigation
#'
#' Satellite navigation data from automatic identification system (AIS) in the St. Lawrence estuary from 2017 to 2019.
#'
#' @keywords navigation
#' @keywords stresseurs
#'
#' @source Transports Canada, Maerospace Corporation
#'
#' @export
#'
#' @details Cette fonction formatte les données
#'

get_data0021 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # The data provided was produced by Maerospace Corporation,
  # and was acquired via a contract with PWGSC on behalf of the Canadian Space
  # Agency. No metadata came with the source and there are no specific requirement
  # for citation/attribution.
  #
  # WARNING: Données ne peuvent être partagées
  # WARNING: Données brutes doivent être supprimées suite au projet
  # _________________________________________________________________________ #

  # WARNING: Data transfered physically, no cloud access currently
  # Output folder
  output <- "data0021-navigation/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)


  # Unzip data -----
  ## 2017
  unzip(zipfile = paste0(folder, "2017-AIS.zip"),
        exdir = paste0(folder, "2017-AIS"))

  ## 2018
  unzip(zipfile = paste0(folder, "2018-AIS.zip"),
        exdir = paste0(folder, "2018-AIS"))

  ## 2019
  unzip(zipfile = paste0(folder, "2019-AIS.zip"),
        exdir = paste0(folder, "2019-AIS"))
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import and format
  # ----------------------------------------
  #
  # Info:
  # ~~~~~~~~~~~~~
  #
  # https://www.marinecadastre.gov/ais/
  # https://coast.noaa.gov/data/marinecadastre/ais/AISTrackBuilder.pdf
  # https://coast.noaa.gov/data/marinecadastre/ais/AISVesselTransitCounterTool.pdf
  # https://www.stat.berkeley.edu/~s133/dates.html
  # https://gis.stackexchange.com/questions/289608/calculating-distances-between-consecutive-points-using-r
  #
  # The fields vary by message type (even though they are all stored together in
  # CSV files). The reference I use for each message type is
  # https://www.navcen.uscg.gov/?pageName=AISMessages
  #
  # For example, I typically extract messages 1 and 3 for Class A position
  # reports. Or I use message 18 for Class B positions.
  #
  # The SOG field (Speed Over Ground) is the actual speed reported by the vessel
  # at the time of the message. I’m not sure whether it’s better to use it, or
  # calculate speed based on timestamps & distance between points. I suppose one
  # consideration might be that calculating the speed in this way assumes that
  # the vessel was always moving “as the crow flies”… I don’t know how the
  # results would differ compared to using the reported speeds.
  #
  # The vessel categories I’m referring to come from a vessel database which
  # has been assembled internally using best available attributes. The fields
  # in this vessel database can be merged with the AIS data (or datasets derived
  # from the AIS data). I can share this database with you but please note that,
  # like the AIS data, it contains information that can only be used as part of
  # the contract and must be deleted/destroyed once the work is completed.
  #
  # https://tcgis-hub.maps.arcgis.com/home/item.html?id=60e7e3f631944e7a8a8574fb09e4b402
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import and segment -----
  # 2017
  files <- dir(paste0(folder, '2017-AIS/'), full.names = TRUE)
  ais2017 <- list()
  for(i in 1:length(files)) {
    ais2017[[i]] <- read.csv(files[i]) %>%
                    ais_segment()
  }

  # 2018
  files <- dir(paste0(folder, '2018-AIS/'), full.names = TRUE)
  ais2018 <- list()
  for(i in 1:length(files)) {
    ais2018[[i]] <- read.csv(files[i]) %>%
                    ais_segment()
  }

  # 2019
  files <- dir(paste0(folder, '2019-AIS/'), full.names = TRUE)
  ais2019 <- list()
  for(i in 1:length(files)) {
    ais2019[[i]] <- read.csv(files[i]) %>%
                    ais_segment()
  }
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Add years and month
  # -------------------------
  #
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # 2017
  for(i in 1:length(ais2017)) {
    ais2017[[i]] $year <- 2017
    ais2017[[i]]$month <- i+2 #
  }
  message("WARNING: Modifier données AIS navigation pour 2017 si les données sont éventuellement ajoutées à la base de données")

  # 2018
  for(i in 1:length(ais2018)) {
    ais2018[[i]]$year <- 2018
    ais2018[[i]]$month <- i
  }

  # 2019
  for(i in 1:length(ais2019)) {
    ais2019[[i]]$year <- 2019
    ais2019[[i]]$month <- i
  }
  # _____________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Single dataset
  # -------------------------
  #
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # 2017
  ais2017 <- bind_rows(ais2017)

  # 2018
  ais2018 <- bind_rows(ais2018)

  # 2019
  ais2019 <- bind_rows(ais2019)

  # All years
  ais <- bind_rows(ais2017, ais2018, ais2019)

  # Remove partial objects to save memory
  rm(ais2017,ais2018,ais2019)
  # _____________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Remove tracks significantly overlapping with land
  # -------------------------------------------------
  #
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load data from `ceanav` package
  data(aoi_studyarea)
  data(aoi_egsl)
  message("Change data names in package and adjust in function `data0021-navigation()`")

  # Union
  sa <- bind_rows(studyarea, egslSimple) %>%
        st_union()

  # Add buffer around study area to make sure I do not remove data in ports
  tc <- st_buffer(studyarea, 2000)
  stl <- st_buffer(egslSimple, 2000)

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
  # _____________________________________________________________________________ #



  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0021,
           dsn = "./data/data-format/data0021-navigation.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #
}
