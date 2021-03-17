# Functions to format the "sites d'importance" data for the project
fmtHabitat <- function() {
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
                            layer = uid$name[i]) %>%
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


  # Kg - Capture totale -------------------------------
  peche_kg <- aoi
  for(i in kg) {
    # Identify grid cells within fisheries sectors
    uid <- st_intersection(peche_aghamm[[i]], aoi) %>%
           mutate(area = st_area())

    # Add info to grid
    dat <- numeric(nrow(aoi))
    dat[uid] <- 1
    peche_kg <- cbind(peche_kg, dat) %>%
                     rename(!!i:=dat)
  }
}
