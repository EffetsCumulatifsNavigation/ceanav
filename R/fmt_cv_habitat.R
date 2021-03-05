# Functions to format the habitats data for the project
fmtHabitat <- function(zostere = TRUE) {
  if (zostere) fmtZostere()
}


fmtZostere <- function () {

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  folder <- './analysis/data/cv/habitats/zostere/'

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
  # ------------------------------------------------------------------------- #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Format data
  # ------------------------------------
  #
  # All we will do for now for this dataset is include it as presence-absence
  # in the study grid
  #
  # So I intersect the zostere db with the grid to identify which grid cell
  # intersect with the db
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load grid
  data(aoi_grid1000poly)

  # Identify grid cells with zostera
  zostereID <- st_intersects(zostere_inv, aoi) %>%
               unlist() %>%
               unique()

  # Add info to grid
  zostere <- aoi %>% mutate(zostere = 0)
  zostere$zostere[zostereID] <- 1
  # ------------------------------------------------------------------------- #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  save(zostere, file = './data/cv_hab_zostere.RData')
  # ------------------------------------------------------------------------- #

}


# Format milieu humide db
fmtMilieu_humide <- function () {

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  folder <- './analysis/data/cv/habitats/milieu_humide/'

  # Load milieu humide data
  # dataID: 0004
  # Milieux humides lac Saint-Pierre
  milieu_humide_lsp <- read.csv(paste0(folder, 'milieu_humide_lac_saint-pierre/MilieuxHumides-lacSaintPierre-sites-2012.csv')) %>%
                       filter(Type.de.milieux %in% c('Bas marais','Haut marais','Marécage arbustif','Marécage arboré','Eau peu profonde')) %>%
                       st_as_sf(coords = c("Longitude","Latitude"), crs = 4326) %>%
                       st_transform(32198) %>%
                       st_buffer(500)


  # dataID: 0005
  # Milieux humides Saint-Laurent
  milieu_humide_stl <- st_read(paste0(folder, 'milieu_humide_saint-laurent/MilieuxHum_Publ_janv2019/MilieuxHum_Publ_janv2019.shp')) %>%
                       st_transform(32198)
  # ------------------------------------------------------------------------- #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Format data
  # ------------------------------------
  #
  # All we will do for now for this dataset is include it as presence-absence
  # in the study grid
  #
  # So I intersect the milieu humide db with the grid to identify which grid
  # cell intersect with the db
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Combine datasets
  milieu_humide <- bind_rows(milieu_humide_lsp, milieu_humide_stl)

  # Load grid
  data(aoi_grid1000poly)

  # Identify grid cells with zostera
  uid <- st_intersects(milieu_humide, aoi) %>%
               unlist() %>%
               unique()

  # Add info to grid
  milieu_humide <- aoi %>% mutate(milieu_humide = 0)
  milieu_humide$milieu_humide[uid] <- 1
  # ------------------------------------------------------------------------- #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  save(milieu_humide, file = './data/cv_hab_milieu_humide.RData')
  # ------------------------------------------------------------------------- #

}
