#' Déversements accidentels
#'
#' Couche de données transformées pour les déversements accidentels dans le Saint-Laurent
#'
#' @keywords déversement accidentel
#' @keywords stresseurs
#'
#' @export
#'
#' @details Cette fonction importe et formatte les données pour l'analyse d'effets cumulatifs
#'

st_deversement <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Format data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load  data
  load_format("data0016")
  meta <- read_yaml("./data/data-metadata/int_st_deversement.yml")
  meta$rawData <-  c("0016")

  # -----
  dev <- data0016

  # Classify by volume
  lvls <- c('0 - 100 litres','100 - 1000 litres','1000 - 7000 litres','100000 - 1000000 litres')
  dev$volume <- as.numeric(factor(dev$VOLUME_DEVERSE, levels = lvls)) + 1 # !!!!!!! This number is arbitrary

  # NA volume to 0-100 litres category
  uid <- is.na(dev$volume)
  dev$volume[uid] <- 2

  # Select points in study area
  data(aoi)
  uid <- st_intersects(aoi, dev) %>% unlist()
  dev <- dev[uid, ]

  # Classify by spill type
  hydrocarbures <- c("bunker C","Carburant diésel","Diesel","Essence","Gaoline",
                     "hydrocarbure inconnu","Petcoke","Pètrole brut","Propane",
                     "Tar","Huile de graissage","Huile Hydraulique","Huile moteur",
                     "bilge","BioSpec Hyd 32","Hydrox Bio 100","Lub oil",
                     "Huile hydraulique biodégradable","Asphalte liquide")

  autres <- c("Acide d'hypochlorite","Ballast","Débris","Déchet","Eau de cale",
              "eau huileuse","Eau usée","Eaux us","Lait","Matière organique",
              "MINERAI DE FER","Oxyde de calcium (chaux)","Phosphate d'ammonium",
              "Pollution","Sludge","Suie","Soude caustique","Huile lapio",
              "Huile Hydraulique Végétale","Huile végétale","Mélange huileux",
              "Charbon")

  inconnus <- c('Inconnu',"0","non identifi","P")
  # ------------------------------------------------------------------------- #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Diffusive model
  # ------------------------------------
  #
  # Diffusive model to simulate the area of influence of spills
  #   1. Modify volume in classes from 1 to 5
  #       1 = 0 litre
  #       2 = 0 - 100 litres
  #       3 = 100 - 1000 litres
  #       4 = 1000 - 7000 litres
  #       5 = 100000 - 1000000 litres
  #   2. Diffusive model, distribute class values with a devay function from the source with the
  #      following  parameters (see `R/fnc_diffusive_model.R`)
            field <- "volume"
            threshold <- .05
            globalmaximum <- 5
            decay <- 2
            distance <- 10
            increment <- 10
  #   3. Intersect with grid
  #   4. Sum of intersects in each grid cell
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # -----
  hyd <- diffusive(dat = dev[dev$TYPE_POLLUANT %in% hydrocarbures, ],
                   field = field,
                   threshold = threshold,
                   globalmaximum = globalmaximum,
                   decay = decay,
                   distance = distance,
                   increment = increment
                 ) %>%
                 bind_rows()

  # -----
  aut <- diffusive(dat = dev[dev$TYPE_POLLUANT %in% autres, ],
                   field = field,
                   threshold = threshold,
                   globalmaximum = globalmaximum,
                   decay = decay,
                   distance = distance,
                   increment = increment
                 ) %>%
                 bind_rows()

  # -----
  inc <- diffusive(dat = dev[dev$TYPE_POLLUANT %in% inconnus, ],
                   field = field,
                   threshold = threshold,
                   globalmaximum = globalmaximum,
                   decay = decay,
                   distance = distance,
                   increment = increment
                 ) %>%
                 bind_rows()
  # ------------------------------------------------------------------------- #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Add to grid
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load grid
  data(grid1p)
  grid1p$id <- 1:nrow(grid1p)

  # -----
  hyd2 <- hyd %>%
          mutate(area_tot = as.numeric(st_area(.)) * 1e-6) %>%
          st_intersection(grid1p, .) %>%
          mutate(area = as.numeric(st_area(.)) * 1e-6,
                 area_prop = area / area_tot,
                 intensite = intensity * area_prop) %>%
          group_by(id) %>%
          summarise(hydrocarbures = sum(intensite)) %>%
          st_drop_geometry()


  # -----
  aut2 <- aut %>%
          mutate(area_tot = as.numeric(st_area(.)) * 1e-6) %>%
          st_intersection(grid1p, .) %>%
          mutate(area = as.numeric(st_area(.)) * 1e-6,
                 area_prop = area / area_tot,
                 intensite = intensity * area_prop) %>%
          group_by(id) %>%
          summarise(autres = sum(intensite)) %>%
          st_drop_geometry()

  # -----
  inc2 <- inc %>%
          mutate(area_tot = as.numeric(st_area(.)) * 1e-6) %>%
          st_intersection(grid1p, .) %>%
          mutate(area = as.numeric(st_area(.)) * 1e-6,
                 area_prop = area / area_tot,
                 intensite = intensity * area_prop) %>%
          group_by(id) %>%
          summarise(inconnus = sum(intensite)) %>%
          st_drop_geometry()


  # -----
  deversement <- grid1p %>%
                 left_join(hyd2, by = "id") %>%
                 left_join(aut2, by = "id") %>%
                 left_join(inc2, by = "id") %>%
                 select(-id)

  # -------
  # Identify cells in the aoi that are only terrestrial and hence shouldn't be included.
  data(aoi)
  uid <- st_intersects(aoi, deversement) %>% unlist()
  nid <- !1:nrow(deversement) %in% uid
  deversement <- st_drop_geometry(deversement)
  for(i in 1:ncol(deversement)) deversement[nid, i] <- NA
  deversement <- cbind(grid1p, deversement) %>%
                 select(-id)
  # ------------------------------------------------------------------------- #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Update metadata
  # ----------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  meta$dataDescription$spatial$extent <- st_bbox(data0016)

  # -----
  uid <- st_intersects(aoi, data0016) %>% unlist()
  obs <- data0016[uid, ]

  # -----
  years <- sort(unique(data0016$ANNÉE))
  meta$dataDescription$temporal$start <- min(years)
  meta$dataDescription$temporal$end <- max(years)
  meta$dataDescription$observations$total <- nrow(obs)

  # -----
  meta$dataDescription$categories$accronyme <-  c("hydrocarbures","autres","inconnus")
  meta$dataDescription$categories$francais <-  c("Hydrocarbures","Autres","Contenu inconnu")
  meta$dataDescription$categories$english <-  c("Hydrocarbons","Others","Unknown content")
  meta$dataDescription$categories$source <-  rep(meta$rawData, length(meta$dataDescription$categories$francais))
  meta$dataDescription$categories$description <-  c(
    "Déversement d'hydrocarbures, e.g. carburant, diésel, pétrole, propane, huile hydraulique, etc.",
    "Déversements autres que des hydrocarbures, e.g. des eaux de ballast, des déchets, de l'eau de cale, du charbon, de la matière organique, etc.",
    "Déversements dont le contenu est inconnu")

  meta$dataDescription$categories$description_en <-  c(
    "Hydrocarbon spills, e.g. fuel, diesel, oil, propane, hydraulic oil, etc.",
    "Spills other than hydrocarbons, e.g. ballast water, waste, bilge water, coal, organic matter, etc.",
    "Spills content of which is unknown")

  # ---
  meta$dataDescription$categories$observations <- c(
    nrow(obs[obs$TYPE_POLLUANT %in% hydrocarbures, ]),
    nrow(obs[obs$TYPE_POLLUANT %in% autres, ]),
    nrow(obs[obs$TYPE_POLLUANT %in% inconnus, ])
  )

  # ---
  meta$dataDescription$categories$souscategories <- c(
    "Bunker C, Carburant diésel, Essence,Gasoline, Hydrocarbure inconnu, Petcoke, Pétrole brut, Propane, Tar, Huile de graissage, Huile Hydraulique, Huile moteur, Bilge, BioSpec Hyd 32, Hydrox Bio 100, Lub oil, Huile hydraulique biodégradable, Asphalte liquide",
    "Acide d'hypochlorite, Ballast, Débris, Déchet, Eau de cale, Eau huileuse, Eau usée, Lait, Matière organique, Minerai de fer, Oxyde de calcium (chaux), Phosphate d'ammonium, Pollution, Sludge, Suie, Soude caustique, Huile lapio, Huile hydraulique végétale, Huile végétale, Mélange huileux, Charbon",
    "Contenu inconnu"
  )

  # ---
  meta$dataDescription$categories$souscategories_en <- c(
    "Bunker C, Diesel fuel, Gasoline, Unknown hydrocarbon, Petcoke, Crude oil, Propane, Tar, Lubricating oil, Hydraulic oil, Motor oil, Bilge, BioSpec Hyd 32, Hydrox Bio 100, Lube oil, Biodegradable hydraulic oil, Liquid asphalt",
    "Hypochlorous acid, Ballast, Debris, Waste, Bilge water, Oily water, Wastewater, Milk, Organic matter, Iron ore, Calcium oxide (lime), Ammonium phosphate, Pollution, Sludge, Soot, Caustic soda, LAPIO oil, Hydraulic vegetable oil, Vegetable oil, Oily mixture, Coal",
    "Unknown content"
  )

  # -----
  tab <- table(dev$VOLUME_DEVERSE, useNA = "always")
  meta$dataDescription$classes$lvls <- c(
         "1 : 0 - 100 litres",
         "2 : 100 - 1000 litres",
         "3 : 1000 - 7000 litres",
         "4 : 100000 - 1000000 litres",
         "NA : Volume inconnu"
       )

  meta$dataDescription$classes$lvls_en <- c(
         "1 : 0 - 100 litres",
         "2 : 100 - 1000 litres",
         "3 : 1000 - 7000 litres",
         "4 : 100000 - 1000000 litres",
         "NA : Volume unknown"
       )

  lvls <- c('0 - 100 litres','100 - 1000 litres','1000 - 7000 litres','100000 - 1000000 litres','<NA>')
  meta$dataDescription$classes$nombre <- c(
    sum(dev$VOLUME_DEVERSE == lvls[1], na.rm = TRUE),
    sum(dev$VOLUME_DEVERSE == lvls[2], na.rm = TRUE),
    sum(dev$VOLUME_DEVERSE == lvls[3], na.rm = TRUE),
    sum(dev$VOLUME_DEVERSE == lvls[4], na.rm = TRUE),
    sum(is.na(dev$VOLUME_DEVERSE))
  )

  # --- For proper referencing in markdown syntax
  meta$dataDescription$categories$mdref <- modif_md(meta$dataDescription$categories$accronyme)

  # ------------------------------------------------------------------------- #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # -----
  write_yaml(meta, "./data/data-metadata/int_st_deversement.yml")

  # -----
  st_write(obj = deversement,
           dsn = "./data/data-integrated/st_deversement.geojson",
           delete_dsn = TRUE,
           quiet = TRUE)
  # ------------------------------------------------------------------------- #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Clean global environment
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  clean()
  # ------------------------------------------------------------------------- #}
}
