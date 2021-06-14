
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
