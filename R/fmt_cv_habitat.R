
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


fmtShoreline <- function () {
  # WARNING: For now, I will keep all coastline types in a single dataset.
  # Longterm, I should do like the other datasets, however, and seperate by
  # habitat types. Once I am better aware of the datasets available, I should
  # be able to make a better call on this issue.
  # https://github.com/EffetsCumulatifsNavigation/ceanav/issues/3

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  folder <- './analysis/data/cv/habitats/cote/'

  # dataID: 0007
  # Shoreline classification
  cote_classification <- st_read(paste0(folder, 'cote_classification/ShorelineClassification_QC_OpenDataCatalogue.gdb')) %>%
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
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load grid
  data(aoi_grid1000poly)

  # Use grid as dataset
  cote <- aoi

  # For each coastal habitat type
  hab <- unique(cote_classification$SCAT_Class_EN)
  for(i in hab) {
    # Segments for habitat i
    habid <- cote_classification$SCAT_Class_EN == i

    # Identify grid cells with coast habitat types
    uid <- st_intersects(cote_classification[habid, ], aoi) %>%
                 unlist() %>%
                 unique()

    # Add info to grid
    dat <- numeric(nrow(cote))
    dat[uid] <- 1
    cote <- cbind(cote, dat) %>%
            rename(!!i:=dat)
  }
  # ------------------------------------------------------------------------- #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  save(cote, file = './data/cv_hab_cote.RData')
  # ------------------------------------------------------------------------- #

}


# Format milieu humide db
fmtZone_inondable <- function () {

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  folder <- './analysis/data/cv/habitats/zone_inondable/'

  # dataID: 0008
  # Zones inondables MRC
  zone_inondable_mrc <- st_read(paste0(folder, 'zone_inondable_mrc/grillepresencezoneinondable.geojson')) %>%
                        st_transform(32198)


  # dataID: 0009
  # Zones inondables BDZI
  # WARNING: La limite des hautes eaux dans le Lac Saint-Pierre est disponible
  #          dans ce jeu de données
  # WARNING: Les polygones non-simplifiés (`ZOI_s`) sont énormes et la
  #          résolution est assurément supérieure à ce dont on a besoin
  #
  # Pour visualiser les données disponibles dans la geodatabase:
  # st_layers(paste0(folder, 'zone_inondable_bdzi/BDZI.gdb'))
  zone_inondable_bdzi <- st_read(paste0(folder, 'zone_inondable_bdzi/BDZI.gdb'), layer = 'ZOI_s_Simplify') %>%
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
  #
  # WARNING: Both datasets are very similar, but do have some differences.
  #          I use them both for now, but I should make sure that I can
  #          properly understand the differences between the datasets, or the
  #          similarities.
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Combine datasets
  zone_inondable <- bind_rows(zone_inondable_mrc, zone_inondable_bdzi)

  # Load grid
  data(aoi_grid1000poly)

  # Identify grid cells with zostera
  uid <- st_intersects(zone_inondable, aoi) %>%
               unlist() %>%
               unique()

  # Add info to grid
  zone_inondable <- aoi %>% mutate(zone_inondable = 0)
  zone_inondable$zone_inondable[uid] <- 1
  # ------------------------------------------------------------------------- #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  save(zone_inondable, file = './data/cv_hab_zone_inondable.RData')
  # ------------------------------------------------------------------------- #

}



fmtBenthic <- function() {

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  folder <- './analysis/data/cv/habitats/benthique/'

  # dataID: 0011
  # Mégahabitats
  benthique_mega <- st_read(paste0(folder, 'benthique_mega/Megahabitats_DB.shp')) %>%
                    st_transform(32198)

  # dataID: 0010
  # Géologie Loring et Nota
  # WARNING: Non considéré, inclu dans mégahabitats benthiques dataID: 0011
  # benthique_ln <- st_read(paste0(folder, 'benthique_ln/Seafloor_SubstratBenthique.shp')) %>%
  #                 st_transform(32198)
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
  #
  # See report for a description of the different habitat types
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load grid
  data(aoi_grid1000poly)

  # Use grid as dataset
  benthique <- aoi

  # For each coastal habitat type
  hab <- unique(benthique_mega$Megahabita) %>%
         .[!is.na(.)]

  for(i in hab) {
    # Segments for habitat i
    habid <- benthique_mega$Megahabita == i

    # Identify grid cells with coast habitat types
    uid <- st_intersects(benthique_mega[habid, ], aoi) %>%
           unlist() %>%
           unique()

    # Add info to grid
    dat <- numeric(nrow(benthique))
    dat[uid] <- 1
    benthique <- cbind(benthique, dat) %>%
                 rename(!!i:=dat)
  }
  # ------------------------------------------------------------------------- #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  save(benthique, file = './data/cv_hab_benthique.RData')
  # ------------------------------------------------------------------------- #

}
