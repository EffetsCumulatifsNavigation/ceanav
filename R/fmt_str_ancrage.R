fmtAncrage <- function () {

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  folder <- './analysis/data/stresseurs/ancrages/ancrages_innav/'

  # dataID: 0012
  # Ancrages INNAV
  ancrage <- st_read(paste0(folder, 'INNAV_Marine_Navigation_Objects_ANCHORAGES_2019_EAST.geojson')) %>%
             st_transform(32198)
  # ------------------------------------------------------------------------- #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Format data
  # ------------------------------------
  #
  # WARNING: Method has to be revisited
  # https://github.com/EffetsCumulatifsNavigation/ceanav/issues/6
  #
  # For now, I will do something simple:
  #   1. 5km buffer around anchorages (totally arbitrary)
  #   2. Intersect with grid
  #   3. Number of buffers intersecting grid cell
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load grid
  data(aoi_grid1000poly)

  # Identify grid cells with zostera
  ancrage <- st_buffer(ancrage, 5000) %>%
             st_intersects(aoi,.) %>%
             lapply(., length) %>%
             unlist()

  # Add info to grid
  ancrage <- aoi %>% mutate(ancrage = ancrage)
  # ------------------------------------------------------------------------- #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  save(ancrage, file = './data/str_ancrage.RData')
  # ------------------------------------------------------------------------- #

}
