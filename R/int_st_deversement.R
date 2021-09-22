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
  # WARNING: Method has to be revisited and document
  # https://github.com/EffetsCumulatifsNavigation/ceanav/issues/7
  #
  # For now, I will do something simple:
  #   1. Modify volume in classes from 1 to 5
  #       1 = 0 litre
  #       2 = 0 - 100 litres
  #       3 = 100 - 1000 litres
  #       4 = 1000 - 7000 litres
  #       5 = 100000 - 1000000 litres
  #   2. 5km buffer around spills (totally arbitrary, no scientific basis, just for viz)
  #   3. Intersect with grid
  #   4. Sum of intersects in each grid cell
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load  data
  load_format("data0016")
  meta <- read_yaml("./data/data-metadata/int_st_deversement.yml")
  meta$rawData <-  c("0016")

  # -----
  dev <- data0016

  # Load grid
  data(grid1p)

  # Classify by volume
  lvls <- c('0 litre','0 - 100 litres','100 - 1000 litres','1000 - 7000 litres','100000 - 1000000 litres')
  dev$volume <- as.numeric(factor(dev$VOLUME_DEVERSE, levels = lvls))
  dev$volume[is.na(dev$volume)] <- 6

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

  # Identify grid cells ---------
  # Use grid as dataset
  deversement <- grid1p

  # Buffer around points
  dev <- st_buffer(dev, 5000)

  # Intersect polluant types with grid
  # Hydrocarbures
  hyd <- dev[dev$TYPE_POLLUANT %in% hydrocarbures, ] %>%
                   st_intersects(grid1p,.) %>%
                   lapply(., function(x) sum(dev$volume[x])) %>%
                   unlist()

  # Autres polluants
  aut <- dev[dev$TYPE_POLLUANT %in% autres, ] %>%
            st_intersects(grid1p,.) %>%
            lapply(., function(x) sum(dev$volume[x])) %>%
            unlist()

  # Inconnus
  inc <- dev[dev$TYPE_POLLUANT %in% inconnus, ] %>%
              st_intersects(grid1p,.) %>%
              lapply(., function(x) sum(dev$volume[x])) %>%
              unlist()

  # Add info to grid
  deversement <- grid1p %>%
                 mutate(hydrocarbures = hyd,
                        autres = aut,
                        inconnus = inc)


  # -------
  # Identify cells in the aoi that are only terrestrial and hence shouldn't be included.
  data(aoi)
  uid <- st_intersects(aoi, deversement) %>% unlist()
  nid <- !1:nrow(deversement) %in% uid
  deversement <- st_drop_geometry(deversement)
  for(i in 1:ncol(deversement)) deversement[nid, i] <- 0
  deversement <- cbind(grid1p, deversement)
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
  meta$dataDescription$categories$source <-  rep(meta$rawData, length(meta$dataDescription$categories$francais))

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

  # -----
  meta$dataDescription$classes$lvls <- c(
         "1 : 0 litre",
         "2 : 0 - 100 litres",
         "3 : 100 - 1000 litres",
         "4 : 1000 - 7000 litres",
         "5 : 100000 - 1000000 litres"
       )

  meta$dataDescription$classes$nombre <- as.numeric(table(dev$volume)[1:length(meta$dataDescription$classes$lvls)])

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
