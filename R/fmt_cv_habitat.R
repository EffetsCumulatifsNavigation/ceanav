# Functions to format the habitats data for the project
fmtHabitat <- function(zostere = TRUE) {
  if (zostere) fmtZostere()
}


fmtZostere <- function () {
  folder <- './analysis/data/cv/habitats/zostere/'

  # Load zostere data
  # dataID: 0001
  # Inventaire zostère
  zostere_inv <- st_read(paste0(folder, 'zostere_inventaire/Zostera_Zostere.shp')) %>%
                 st_transform(32198)

  # dataID: 0002
  # Zostères Pointe-au-Père
  # WARNING: Non considéré, capturée par l'inventaire zostère dataID: 0001
  # WARNING: Tout de même cité, par contre
  # zostere_pap <- st_read(paste0(folder, 'zostere_pointe_au_pere/herbier-zostere_continu.shp')) %>%
  #                 st_transform(32198)
  #
  # zostere_pap_plantation <- st_read(paste0(folder, 'zostere_pointe_au_pere/zones_plantation_zostere.shp')) %>%
  #                           st_transform(32198)

  # dataID: 0003
  # Zostères Mitis
  # WARNING: Non considéré, capturée par l'inventaire zostère dataID: 0001
  # WARNING: Tout de même cité, par contre
  # zostere_mitis_12 <- st_read(paste0(folder, 'zostere_mitis/Plantation_12_juin.shp')) %>%
  #                      st_transform(32198)
  #
  # zostere_mitis_13 <- st_read(paste0(folder, 'zostere_mitis/Plantation_13_juin.shp')) %>%
  #                      st_transform(32198)
  #
  # zostere_mitis_14 <- st_read(paste0(folder, 'zostere_mitis/Plantation_14_juin.shp')) %>%
  #                      st_transform(32198)

  # All we will do for now for this dataset is include it as presence-absence in the study grid
  # So I intersect the zostere db with the grid to identify which grid cell intersect with the db
  # Load grid
  data(aoi_grid1000poly)

  # Identify grid cells with zostera
  zostereID <- st_intersects(zostere_inv, aoi) %>%
               unlist() %>%
               unique()

  # Add info to grid
  zostere <- aoi %>% mutate(zostere = 0)
  zostere$zostere[zostereID] <- 1

  # Save data
  save(zostere, file = './data/cv_hab_zostere.RData')
}
