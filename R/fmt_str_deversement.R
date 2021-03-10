fmtDeversement <- function () {

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  folder <- './analysis/data/stresseurs/deversements/deversement_gcc/'

  # dataID: 0013
  # Déversements accidentels GCC
  dev <- read.csv(paste0(folder, '20210223_Demande_UL.csv'), na.strings = '') %>%
         drop_na('LATITUDE') %>%
         mutate(LONGITUDE = ifelse(LONGITUDE > 0, -LONGITUDE, LONGITUDE)) %>%
         st_as_sf(coords = c('LONGITUDE','LATITUDE'), crs = 4326, remove = FALSE) %>%
         st_transform(32198)

  # Transform Volume
  dev$VOLUME_DEVERSE <- gsub('100- 1000 litres', '100 - 1000 litres', dev$VOLUME_DEVERSE)


  # # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # # Travail de vérification avec Pierre
  # mapviewOptions(fgb = FALSE)
  #
  # # Longitude problématique
  # uid <- c(8017, 8944, 9227, 9801, 9804, 10057, 10071, 10107, 10169, 10272, 10288, 10444, 10458, 10497, 10499, 10538, 10540, 10555, 10585, 10749, 10752, 11282, 11349, 11569, 11590, 11678, 11770, 11780, 11782, 12055, 12089)
  # dev <- read.csv(paste0(folder, '20210223_Demande_UL.csv'), na.strings = '') %>%
  #        drop_na('LATITUDE') %>%
  #        filter(ID_CAS %in% uid) %>%
  #        st_as_sf(coords = c('LONGITUDE','LATITUDE'), crs = 4326, remove = FALSE) %>%
  #        mapview() %>%
  #        mapshot(url = 'delete/longitude.html')
  #
  # # Positionnement problématique
  # uid <- c(6871, 7686, 7716, 8377, 9343, 9883, 9916, 10057, 10188, 11809, 11936, 11940)
  # dev <- read.csv(paste0(folder, '20210223_Demande_UL.csv'), na.strings = '') %>%
  #        drop_na('LATITUDE') %>%
  #        filter(ID_CAS %in% uid) %>%
  #        st_as_sf(coords = c('LONGITUDE','LATITUDE'), crs = 4326, remove = FALSE) %>%
  #        mapview() %>%
  #        mapshot(url = 'delete/positionnement.html')
  # # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  # ------------------------------------------------------------------------- #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Format data
  # ------------------------------------
  #
  # WARNING: Method has to be revisited and document
  # https://github.com/EffetsCumulatifsNavigation/ceanav/issues/7
  #
  # For now, I will do something simple:
  #   1. Modify volume in classes from 1 to 5
  #       1 = 0 litre
  #       2 = 0 - 100 litres
  #       3 = 100 - 1000 litres
  #       4 = 1000 - 7000 litres
  #       5 = 100000 - 1000000 litres
  #   2. 5km buffer around spills (totally arbitrary, no scientific basis, just for viz)
  #   3. Intersect with grid
  #   4. Sum of intersects in each grid cell
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load grid
  data(aoi_grid1000poly)

  # Classify by volume
  lvls <- c('0 litre','0 - 100 litres','100 - 1000 litres','1000 - 7000 litres','100000 - 1000000 litres')
  dev$volume <- as.numeric(factor(dev$VOLUME_DEVERSE, levels = lvls))
  dev$volume[is.na(dev$volume)] <- 6


  # Identify grid cells with zostera
  dev <- st_buffer(dev, 5000) %>%
         st_intersects(aoi,.) %>%
         lapply(., sum) %>%
         unlist()

  # Add info to grid
  deversement <- aoi %>% mutate(deversement = dev)
  # ------------------------------------------------------------------------- #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  save(deversement, file = './data/str_deversement.RData')
  # ------------------------------------------------------------------------- #

}
