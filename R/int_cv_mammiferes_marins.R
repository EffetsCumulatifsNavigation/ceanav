#' Mammifères marins: Données sur la présence de mammifères marins
#'
#' Intégration des données utilisées pour caractériser la composante valorisée sur les mammifères marins
#'
#' @keywords mammifères marins
#' @keywords composante valorisée
#'
#' @export
#'
#' @details Cette fonction télécharge et formatte les données
#'

cv_mammiferes_marins <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Prepare data from the mariner's guide
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  load_format("data0027")
  data(grid1p)
  data0027 <- as(data0027, "Raster")


  # -------
  # Identify cells in the aoi that are only terrestrial and hence shouldn't be included.
  data(aoi)
  uid <- st_intersects(aoi, grid1p) %>% unlist()
  nid <- !1:nrow(grid1p) %in% uid

  # -------
  grid1p <- st_transform(grid1p, st_crs(data0027))
  mm <- exact_extract(data0027, grid1p, 'mean', progress = FALSE)
  for(i in 1:ncol(mm)) mm[nid,i] <- NA

  # -------
  for(i in 1:ncol(mm)) {
    suppressWarnings({
      mm[,i] <- mm[,i] / max(mm[,i], na.rm = TRUE)
    })
  }

  # -------
  mm <- cbind(grid1p, mm) %>%
        st_transform(global_parameters()$crs)

  # -------
  # Select only relevant species
  #   Baleine à bosse
  #   Baleine bleue
  #   Petit rorqual
  #   Béluga
  #   Rorqual commun
  mm <- mm %>%
        select(
          rorqual_a_bosse = mean.Rorqual_a_bosse,
          rorqual_bleu = mean.Rorqual_bleu,
          petit_rorqual = mean.Petit_rorqual,
          beluga = mean.Beluga,
          rorqual_commun = mean.Rorqual_commun,
          geometry
        )

  # -------
  mammiferes_marins <- mm
  # ------------------------------------------------------------------------- #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Complement with ROMM data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  load_format("data0054")

  # Filter data
  # Data within study area
  uid <- st_intersects(aoi, data0054) %>% unlist()
  data0054 <- data0054[uid, ]

  # year >= 2000
  data0054$year <- format(data0054$Date, format = "%Y")
  uid <- data0054$year >= 2000
  data0054 <- data0054[uid, ]

  # Remove species for which we got data from Mariner's Guide
  nm <- c("Beluga whale","Blue whale","Fin whale","Humpback whale","Minke whale")
  uid <- !data0054$Taxon %in% nm
  data0054 <- data0054[uid, ]

  # species > 50 occurrences
  tab <- table(data0054$Taxon) %>% as.data.frame()
  nm <- tab$Var1[tab$Freq > 50] %>% as.character()
  uid <- data0054$Taxon %in% nm
  data0054 <- data0054[uid, ]

  # Implement same method used in the Mariner's Guide
  data0054 <- occurrence_distribution()



  occurrence_distribution <- function(dat, studygrid) {

    uid <- data0054$Taxon == "Grey seal"
    dat <- data0054[uid, ]
    studygrid <- grid1p

    # 1. Nombre d'occurrence par cellule de la grille
    dat <- st_intersects(studygrid, dat) %>%
           sapply(length) %>%
           mutate(studygrid, frequence = .)

    # 2. Normalisation par le nombre total d'observation
    dat$normalisation <- dat$frequence / max(dat$frequence)

    # 3. Lissage Gaussien de 0.2o

    dat$lissage <- lissage(dat, field = "normalisation")


    lissage <- function(dat, field) {
      dat <- dat
      field <- "normalisation"

      # -----
      coords <- st_centroid(dat) %>%
                st_coordinates() %>%
                as.data.frame() %>%
                rename(x = X, y = Y)

      # -----
      x <- st_drop_geometry(dat) %>%
             cbind(coords) %>%
             .[, c("x","y",field)] %>%
             set_colnames(c("x","y","Z")) %>%
             btb::kernelSmoothing(dfObservations = .,
                                  sEPSG = st_crs(dat),
                                  iCellSize = 1000,
                                  iBandwidth = 22200,
                                  vQuantiles = NULL,
                                  dfCentroids = NULL)

    }

    # 4. Transformation logarithmique
    dat$log <- log(dat$lissage + 1)

  }


  # ------------------------------------------------------------------------- #



  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Update metadata
  # ----------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  meta <- load_metadata("int_cv_mammiferes_marins")

  # -----
  meta$rawData <- c("0027")

  # -----
  meta$dataDescription$spatial$extent <- st_bbox(mammiferes_marins)

  # -----
  nm <- data.frame(accronyme = c("rorqual_a_bosse","rorqual_bleu","petit_rorqual",
                                 "beluga","rorqual_commun"),
                 francais = c("Rorqual à bosse","Rorqual bleu","Petit rorqual",
                              "Béluga du Saint-Laurent","Rorqual commun"),
                 english = c("Humpback whale","Blue whale","Minke whale",
                             "St. Lawrence beluga whale","Fin whale"),
                 scientific = c("Megaptera novaeangliae","Balaenoptera musculus",
                                "Balaenoptera acutorostrata", "Delphinapterus leucas",
                                "Balaenoptera physalus"),
                 source = "0027")

  meta$dataDescription$categories$accronyme <-  nm$accronyme
  meta$dataDescription$categories$francais <-  nm$francais
  meta$dataDescription$categories$english <-  nm$english
  meta$dataDescription$categories$scientific <-  nm$scientific
  meta$dataDescription$categories$source <-  nm$source

  # --- For proper referencing in markdown syntax
  meta$dataDescription$categories$mdref <- modif_md(meta$dataDescription$categories$accronyme)
  # _____________________________________________________________________________ #



  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # -----
  write_yaml(meta, "./data/data-metadata/int_cv_mammiferes_marins.yml")

  # -----
  st_write(obj = mammiferes_marins,
           dsn = "./data/data-integrated/cv_mammiferes_marins.geojson",
           delete_dsn = TRUE,
           quiet = TRUE)
  # ------------------------------------------------------------------------- #}

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Clean global environment
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  clean()
  # ------------------------------------------------------------------------- #}
}
