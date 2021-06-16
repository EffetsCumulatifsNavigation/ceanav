
fmtSaumon <- function () {

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  folder <- './analysis/data/cv/site_important/saumon_atlantique/'

  # dataID: 0017
  # Saumon Atlantique AGHAMM
  saumon_aghamm <- st_read(paste0(folder, 'saumon_atlantique_aghamm/saumon_atlantique_aghamm.geojson'))


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Format data
  # ------------------------------------
  #
  # Simply add a buffer around points and add as presence to the grid
  # WARNING: Buffer size is arbitrary, to confirm
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Buffer around points
  saumon_aghamm <- st_buffer(saumon_aghamm, 3000)

  # Load grid
  data(aoi_grid1000poly)

  # Identify grid cells with zostera
  saumonID <- st_intersects(saumon_aghamm, aoi) %>%
               unlist() %>%
               unique()

  # Add info to grid
  saumon <- aoi %>% mutate(saumon = 0)
  saumon$saumon[saumonID] <- 1
  # ------------------------------------------------------------------------- #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  save(saumon, file = './data/cv_ste_saumon.RData')
  # ------------------------------------------------------------------------- #
}
