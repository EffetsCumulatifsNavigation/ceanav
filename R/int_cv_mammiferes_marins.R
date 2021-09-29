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
  # Prepare data
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
