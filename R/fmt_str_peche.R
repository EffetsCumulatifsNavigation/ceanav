fmtPecheCommerciale <- function() {

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load data
  # -------------------------
  #
  # Info:
  # ~~~~~~~~~~~~~
  #
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  folder <- './analysis/data/stresseurs/peches/peches_ziff/'

  # Load data
  dd <- read_stars(paste0(folder, 'FisheriesDD.tif'))
  dnh <- read_stars(paste0(folder, 'FisheriesDNH.tif'))
  dnl <- read_stars(paste0(folder, 'FisheriesDNL.tif'))
  phb <- read_stars(paste0(folder, 'FisheriesPHB.tif'))
  plb <- read_stars(paste0(folder, 'FisheriesPLB.tif'))

  # Change projection
  aoi2 <- st_transform(aoi, st_crs(dd))

  # Crop and transform rasters
  dd <- st_crop(dd, st_bbox(aoi2)) %>% st_as_sf(as_points = FALSE, merge = TRUE)
  dnh <- st_crop(dnh, st_bbox(aoi2)) %>% st_as_sf(as_points = FALSE, merge = TRUE)
  dnl <- st_crop(dnl, st_bbox(aoi2)) %>% st_as_sf(as_points = FALSE, merge = TRUE)
  phb <- st_crop(phb, st_bbox(aoi2)) %>% st_as_sf(as_points = FALSE, merge = TRUE)
  plb <- st_crop(plb, st_bbox(aoi2)) %>% st_as_sf(as_points = FALSE, merge = TRUE)

  # Transform projections
  dd <- st_transform(dd, st_crs(aoi))
  dnh <- st_transform(dnh, st_crs(aoi))
  dnl <- st_transform(dnl, st_crs(aoi))
  phb <- st_transform(phb, st_crs(aoi))
  plb <- st_transform(plb, st_crs(aoi))

  # Intersection
  peche <- aoi
  peche$ID <- 1:nrow(aoi)
  dd <- suppressWarnings(st_intersection(peche, dd))
  dnh <- suppressWarnings(st_intersection(peche, dnh))
  dnl <- suppressWarnings(st_intersection(peche, dnl))
  phb <- suppressWarnings(st_intersection(peche, phb))
  plb <- suppressWarnings(st_intersection(peche, plb))

  # Function to do area weighted average
  awa <- function(dat, name) {
    colnames(dat)[2] <- 'dat'
    dat <- dat %>%
           mutate(area = st_area(.)/1000000) %>%
           group_by(ID) %>%
           summarize(awa = sum((dat+1)*area)) %>%
           mutate(awa = as.numeric(awa)) %>%
           st_drop_geometry() %>%
           left_join(peche, ., by = 'ID') %>%
           select(-ID)
    colnames(dat)[1] <- name
    dat
  }

  # Integrate in study grid
  dd <- awa(dd, 'FisheriesDD')
  dnh <- awa(dnh, 'FisheriesDNH')
  dnl <- awa(dnl, 'FisheriesDNL')
  phb <- awa(phb, 'FisheriesPHB')
  plb <- awa(plb, 'FisheriesPLB')

  # Remove geometries
  dnh <- st_drop_geometry(dnh)
  dnl <- st_drop_geometry(dnl)
  phb <- st_drop_geometry(phb)
  plb <- st_drop_geometry(plb)

  # Single dataset
  peche_commerciale <- cbind(dd,dnh,dnl,phb,plb)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  save(peche_commerciale, file = './data/str_peche_commerciale.RData')
}
