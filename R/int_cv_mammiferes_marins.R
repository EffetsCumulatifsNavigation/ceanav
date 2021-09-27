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
  data0027 <- raster("data/data-format/data0027-mammiferes_marins.tif")
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

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  st_write(obj = mammiferes_marins,
           dsn = "./data/data-integrated/cv_mammiferes_marins.geojson",
           delete_dsn = TRUE)
  # ------------------------------------------------------------------------- #}

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Clean global environment
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  clean()
  # ------------------------------------------------------------------------- #}
}
