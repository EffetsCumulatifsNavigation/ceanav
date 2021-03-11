fmtDeversement <- function () {

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  folder <- './analysis/data/stresseurs/deversements/deversement_gcc/'

  # dataID: 0013
  # Déversements accidentels GCC
  dev <- read.csv(paste0(folder, 'deversements_modifs.csv'), na.strings = '') %>%
         drop_na('LATITUDE') %>%
         st_as_sf(coords = c('LONGITUDE','LATITUDE'), crs = 4326, remove = FALSE) %>%
         st_transform(32198)
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


  # Identify grid cells
  dev <- st_buffer(dev, 5000) %>%
         st_intersects(aoi,.) %>%
         lapply(., function(x) sum(dev$volume[x])) %>%
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
