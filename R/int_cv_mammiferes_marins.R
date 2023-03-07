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
  grid1p_2 <- st_transform(grid1p, st_crs(data0027))
  mm <- exact_extract(data0027, grid1p_2, 'mean', progress = FALSE)
  for(i in 1:ncol(mm)) mm[nid,i] <- NA

  # -------
  for(i in 1:ncol(mm)) {
    suppressWarnings({
      mm[,i] <- mm[,i] / max(mm[,i], na.rm = TRUE)
    })
  }

  # -------
  mm <- cbind(grid1p_2, mm) %>%
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
  dat <- data0054[uid, ]

  # year >= 2000
  dat$year <- format(dat$Date, format = "%Y")
  uid <- dat$year >= 2000
  dat <- dat[uid, ]

  # Remove species for which we got data from Mariner's Guide
  nm <- c("Beluga whale","Blue whale","Fin whale","Humpback whale","Minke whale")
  uid <- !dat$Taxon %in% nm
  dat <- dat[uid, ]

  # species > 50 occurrences
  tab <- table(dat$Taxon) %>% as.data.frame()
  nm <- tab$Var1[tab$Freq > 50] %>% as.character()
  uid <- dat$Taxon %in% nm
  dat <- dat[uid, ]

  # Implement same method used in the Mariner's Guide
  mm <- observation_distribution(dat, grid1p)

  # -----
  mm <- st_drop_geometry(mm)

  # Remove terrestrial cells
  for(i in 1:ncol(mm)) mm[nid,i] <- NA

  # Normalize 0-1
  # -------
  for(i in 1:ncol(mm)) {
    suppressWarnings({
      mm[,i] <- mm[,i] / max(mm[,i], na.rm = TRUE)
    })
  }


  # Add to marine mammals data
  mammiferes_marins <- cbind(mammiferes_marins, mm)
  # ------------------------------------------------------------------------- #
  
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Update 2023-03: data0085 large whales dfo
  # ------------------------------------
  # Replace species with better data from the DFO modeling work
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  load_format("data0085")
  data(grid1p)
  data0085 <- as(data0085, "Raster")

  # -------
  # Identify cells in the aoi that are only terrestrial and hence shouldn't be included.
  data(aoi)
  uid <- st_intersects(aoi, grid1p) %>% unlist()
  nid <- !1:nrow(grid1p) %in% uid

  # -------
  # Remove outliers in data, likely along the coast
  for(i in 1:raster::nlayers(data0085)) {
    dat <- raster::values(data0085[[i]])
    out <- boxplot.stats(dat, coef = 3)$stat[c(1,5)]
    cap <- quantile(dat, probs=c(.05, .95), na.rm = T)
    dat[dat > out[2]] <- cap[2]
    raster::values(data0085[[i]]) <- dat
  }

  # -------
  grid1p_2 <- st_transform(grid1p, st_crs(data0085))
  mm <- exact_extract(data0085, grid1p_2, 'mean', progress = FALSE)
  for(i in 1:ncol(mm)) mm[nid,i] <- NA

  # -------
  for(i in 1:ncol(mm)) {
    suppressWarnings({
      mm[,i] <- mm[,i] / max(mm[,i], na.rm = TRUE)
    })
  }

  # -------
  mm <- cbind(grid1p_2, mm) %>%
        st_transform(global_parameters()$crs) |>
        dplyr::rename(
          rorqual_bleu = mean.Blue_pred.tif,
          rorqual_commun = mean.Fin_pred.tif,
          rorqual_a_bosse = mean.Humpback_pred.tif,
          petit_rorqual = mean.Minke_pred.tif
        )

  # -------
  # Replace species in mammiferes_marins
  mammiferes_marins <- dplyr::mutate(
    mammiferes_marins,
    rorqual_bleu = mm$rorqual_bleu,
    rorqual_commun = mm$rorqual_commun,
    rorqual_a_bosse = mm$rorqual_a_bosse,
    petit_rorqual = mm$petit_rorqual 
  )
  # ------------------------------------------------------------------------- #



  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Update metadata
  # ----------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  meta <- load_metadata("int_cv_mammiferes_marins")

  # -----
  meta$rawData <- c("0027","0054","0085")

  # -----
  meta$dataDescription$spatial$extent <- st_bbox(mammiferes_marins)

  # -----
  nm <- data.frame(accronyme = c("rorqual_a_bosse","rorqual_bleu","petit_rorqual",
                                 "beluga","rorqual_commun","harbour_porpoise",
                                 "grey_seal","harbor_seal","harp_seal"),
                 francais = c("Rorqual à bosse","Rorqual bleu","Petit rorqual",
                              "Béluga","Rorqual commun",
                              "Marsouin commun","Phoque gris","Phoque commun",
                              "Phoque du Groenland"),
                 english = c("Humpback whale","Blue whale","Minke whale",
                             "St. Lawrence beluga whale","Fin whale",
                             "Harbour porpoise","Grey seal","Harbor seal",
                             "Harp seal"),
                 scientific = c("Megaptera novaeangliae","Balaenoptera musculus",
                                "Balaenoptera acutorostrata", "Delphinapterus leucas",
                                "Balaenoptera physalus", "Phocoena phocoena",
                                "Halichoerus grypus","Phoca vitulina",
                                "Phoca groenlandica"),
                 type = c("Baleine","Baleine","Baleine","Baleine","Baleine","Baleine",
                          "Phoque","Phoque","Phoque"),
                 type_en = c("Whale","Whale","Whale","Whale","Whale","Whale",
                          "Seal","Seal","Seal"),
                 units = c(
                   "Probabilités relatives d'occurrence",
                   "Probabilités relatives d'occurrence",
                   "Probabilités relatives d'occurrence",
                   "Densité relative des observations",
                   "Probabilités relatives d'occurrence",
                   "Densité relative des observations",
                   "Densité relative des observations",
                   "Densité relative des observations",
                   "Densité relative des observations"
                 ),
                 units_en = c(
                   "Relative probability of occurrence",
                   "Relative probability of occurrence",
                   "Relative probability of occurrence",
                   "Relative density of observations",
                   "Relative probability of occurrence",
                   "Relative density of observations",
                   "Relative density of observations",
                   "Relative density of observations",
                   "Relative density of observations"
                 ),
                 source = c("0085","0085","0085","0027","0085","0054","0054","0054","0054"))

  meta$dataDescription$categories$accronyme <-  nm$accronyme
  meta$dataDescription$categories$francais <-  nm$francais
  meta$dataDescription$categories$english <-  nm$english
  meta$dataDescription$categories$scientific <-  nm$scientific
  meta$dataDescription$categories$type <-  nm$type
  meta$dataDescription$categories$units <-  nm$units
  meta$dataDescription$categories$units_en <-  nm$units_en
  meta$dataDescription$categories$source <-  nm$source

  meta$dataDescription$categories$description <- glue("Distribution du {tolower(nm$francais)}")

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
