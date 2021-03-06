# Functions to format the habitats data for the project
fmtHabitat <- function() {
  fmtZostere()
  fmtMilieu_humide()
  fmtMarais()
  fmtMilieu_sableux()
  fmtAlevinage()
  fmtFrayere()
  fmtEspece_statut()
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


# Format milieu humide db
fmtMarais <- function () {

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  folder <- './analysis/data/cv/habitats/marais/'

  # dataID: 0005
  # Marais Saint-Laurent
  marais_stl <- st_read(paste0(folder, 'marais_saint-laurent/MaraisCotiers_Publ_janv2019/MaraisCotiers_Publ_janv2019.shp')) %>%
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
  # Load grid
  data(aoi_grid1000poly)

  # Identify grid cells with zostera
  uid <- st_intersects(marais_stl, aoi) %>%
               unlist() %>%
               unique()

  # Add info to grid
  marais <- aoi %>% mutate(marais = 0)
  marais$marais[uid] <- 1
  # ------------------------------------------------------------------------- #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  save(marais, file = './data/cv_hab_marais.RData')
  # ------------------------------------------------------------------------- #

}


# Format milieu humide db
fmtMilieu_sableux <- function () {

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  folder <- './analysis/data/cv/habitats/milieu_sableux/'

  # dataID: 0005
  # Marais Saint-Laurent
  milieu_sableux_stl <- st_read(paste0(folder, 'milieu_sableux_saint-laurent/MilieuxSableux_Publ_janv2019/MilieuxSableux_Publ_janv2019.shp')) %>%
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
  # Load grid
  data(aoi_grid1000poly)

  # Identify grid cells with zostera
  uid <- st_intersects(milieu_sableux_stl, aoi) %>%
               unlist() %>%
               unique()

  # Add info to grid
  milieu_sableux <- aoi %>% mutate(milieu_sableux = 0)
  milieu_sableux$milieu_sableux[uid] <- 1
  # ------------------------------------------------------------------------- #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  save(milieu_sableux, file = './data/cv_hab_milieu_sableux.RData')
  # ------------------------------------------------------------------------- #

}


# Format alevinage db
fmtAlevinage <- function () {

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  folder <- './analysis/data/cv/habitats/alevinage/'

  # dataID: 0006
  # Sites d'alevinage dans le Saint-Laurent fluvial
  alevinage_mffp <- st_read(paste0(folder, 'alevinage_mffp/DonneesMFFP_PourPASL.gdb'), layer = 'Alevinage_DEFA_s_CEGRIM') %>%
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
  # Load grid
  data(aoi_grid1000poly)

  # Identify grid cells with zostera
  uid <- st_intersects(alevinage_mffp, aoi) %>%
               unlist() %>%
               unique()

  # Add info to grid
  alevinage <- aoi %>% mutate(alevinage = 0)
  alevinage$alevinage[uid] <- 1
  # ------------------------------------------------------------------------- #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  save(alevinage, file = './data/cv_hab_alevinage.RData')
  # ------------------------------------------------------------------------- #

}


# Format frayere db
fmtFrayere <- function () {

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  folder <- './analysis/data/cv/habitats/frayere/'
  # dataID: 0006
  # Frayères dans le Saint-Laurent fluvial
  frayere_mffp <- st_read(paste0(folder, 'frayere_mffp/DonneesMFFP_PourPASL.gdb'),
                          layer = 'Frayere_s_CEGRIM_25_02_2020') %>%
                  st_transform(32198)

  # WARNING: For some reason, this layer from the geodatabase does not play
  # nice with `sf`.
  #
  # See https://github.com/r-spatial/sf/issues/427
  #
  # For now, I export and reimport as geojson and it works, so I will leave it
  # at that.
  geoj <- paste0(folder, 'frayere_mffp/frayere.geojson')
  if (!file.exists(geoj)) st_write(frayere_mffp, geoj)
  frayere_mffp <- st_read(paste0(folder, 'frayere_mffp/frayere.geojson'))
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
  # Load grid
  data(aoi_grid1000poly)

  # Identify grid cells with zostera
  uid <- st_intersects(frayere_mffp, aoi) %>%
               unlist() %>%
               unique()

  # Add info to grid
  frayere <- aoi %>% mutate(frayere = 0)
  frayere$frayere[uid] <- 1
  # ------------------------------------------------------------------------- #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  save(frayere, file = './data/cv_hab_frayere.RData')
  # ------------------------------------------------------------------------- #

}


# Format frayere db
fmtEspece_statut <- function () {

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  folder <- './analysis/data/cv/habitats/espece_statut/'

  # dataID: 0006
  # Frayères dans le Saint-Laurent fluvial
  espece_statut_mffp <- st_read(paste0(folder, 'espece_statut_mffp/DonneesMFFP_PourPASL.gdb'),
                        layer = 'CDPNQ_CEGRIM_29_04_2020_3') %>%
                        st_transform(32198)

  # WARNING: For some reason, this layer from the geodatabase does not play
  # nice with `sf`.
  #
  # See https://github.com/r-spatial/sf/issues/427
  #
  # For now, I export and reimport as geojson and it works, so I will leave it
  # at that.
  geoj <- paste0(folder, 'espece_statut_mffp/frayere.geojson')
  if (!file.exists(geoj)) st_write(espece_statut_mffp, geoj)
  espece_statut_mffp <- st_read(paste0(folder, 'espece_statut_mffp/frayere.geojson'))
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
  # Load grid
  data(aoi_grid1000poly)

  # Identify grid cells with zostera
  uid <- st_intersects(espece_statut_mffp, aoi) %>%
               unlist() %>%
               unique()

  # Add info to grid
  espece_statut <- aoi %>% mutate(espece_statut = 0)
  espece_statut$espece_statut[uid] <- 1
  # ------------------------------------------------------------------------- #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  save(espece_statut, file = './data/cv_hab_espece_statut.RData')
  # ------------------------------------------------------------------------- #

}
