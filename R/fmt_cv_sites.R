# Functions to format the "sites d'importance" data for the project
fmtSite <- function() {
  fmtPeche()
}


fmtPeche <- function () {

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  folder <- './analysis/data/cv/site_important/peche_commerciale/'

  # dataID: 0016
  # Pêches commerciales AGHAMM
  uid <- st_layers(paste0(folder, 'peche_commerciale_aghamm/Peches_commerciales.gdb'))
  peche_aghamm <- list()
  for(i in 1:length(uid$name)) {
    peche_aghamm[[i]] <- st_read(paste0(folder, 'peche_commerciale_aghamm/Peches_commerciales.gdb'),
                                 layer = uid$name[i], quiet = TRUE) %>%
                    st_transform(32198)
  }
  names(peche_aghamm) <- uid$name


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Format data
  # ------------------------------------
  #
  # WARNING: certain fisheries are only delineated, while others have captures
  #          in kg associated.
  #
  # Sites as regions (presence-absence);
  #
  # Capture as the intensity of use of the area. Therefore if those regions
  # are affected by stressors, the overall impact should be greater.
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  kg <- uid$name[stringr::str_detect(uid$name, 'kg')]
  secteur <- uid$name[stringr::str_detect(uid$name, 'secteur')]

  # Secteur - présence-absence -------------------------------
  # Load grid
  data(aoi_grid1000poly)

  # Use grid as dataset
  peche_secteur <- aoi

  for(i in secteur) {
    # Identify grid cells within fisheries sectors
    uid <- st_intersects(peche_aghamm[[i]], aoi) %>%
           unlist() %>%
           unique()

    # Add info to grid
    dat <- numeric(nrow(aoi))
    dat[uid] <- 1
    peche_secteur <- cbind(peche_secteur, dat) %>%
                     rename(!!i:=dat)

  }

  # Remove fisheries with no data in aoi
  uid <- which(colSums(st_drop_geometry(peche_secteur)) == 0)
  peche_secteur <- peche_secteur[,-uid]


  # Kg - Capture totale -------------------------------
  peche_kg <- aoi
  peche_kg$ID <- 1:nrow(aoi)
  for(i in kg) {
    # Identify grid cells within fisheries sectors
    uid <- suppressWarnings(st_intersection(peche_kg, peche_aghamm[[i]]))

    # If there are intersections, calculate sum of kg / area
    if (nrow(uid) > 0) {
      peche_kg <- uid %>%
                  mutate(area = st_area(.)/1000000) %>%
                  group_by(ID) %>%
                  summarize(awa = sum((Qte_kg+1)*area)) %>%
                  mutate(awa = as.numeric(awa)) %>%
                  st_drop_geometry() %>%
                  left_join(peche_kg, ., by = 'ID') %>%
                  rename(!!i:=awa)
    }
  }

  # Remove ID column from grid
  peche_kg <- select(peche_kg, -ID)

  # Change NAs for 0
  peche_kg[is.na(peche_kg)] <- 0
  # 3886
  # Single dataset
  peche <- cbind(peche_secteur, st_drop_geometry(peche_kg))


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  save(peche, file = './data/cv_ste_peche.RData')
  # ------------------------------------------------------------------------- #

}
