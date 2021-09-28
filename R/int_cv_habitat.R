#' Habitats: Données sur les habitats
#'
#' Intégration des données utilisées pour caractériser la composante valorisée d'intégrité des habitats
#'
#' @keywords habitat
#' @keywords composante valorisée
#'
#' @export
#'
#' @details Cette fonction télécharge et formatte les données
#'

cv_habitat <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Notes
  # ------------------------------------
  #
  # Plusieurs catégories d'habitats composent la composante valorisée d'habitats.
  # Les types d'habitats ont été sélectionnés selon leur importance pour l'écologie
  # du milieu d'étude. Par exemple, les zostères sont des milieux qui sont importants
  # pour les cycles vitaux d'un nombre importants d'espèces, en plus d'influencer
  # le stockage de carbone et la dynamique hydrosédimentaire locale.
  #
  # Il est clair que d'autres habitats pourraient être ajoutés à la liste actuelle.
  # La liste des habitats considérés actuellement est la suivante:
  #
  #   - Zostères : 0001, 0002, 0003
  #   - Zones inondables : 0013, 0014
  #   - Milieux humides : 0004, 0005, 0006, 0042
  #   - Marais côtiers : 0007
  #   - Sites d'alevinage : 0009
  #   - Frayères : 0010
  #   - Zones herbacées : 0029
  #   - Espèces à statut : 0011
  #   - Habitats fauniques : 0036
  #   - Habitats floristiques : 0037
  #   - Colonies d'oiseaux : 0043
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # ------------------------------------------------------
  load_temp <- function(dat) {
    # -----
    dat <- paste0("data", dat)

    # -----
    res <- list()
    for(i in 1:length(dat)) {
      load_format(dat[i])
      res[[i]] <- get(dat[i])
    }
    res
  }

  # ------------------------------------------------------
  uid <- function(dat) {
    # -----
    res <- load_temp(dat)

    # -----
    uid <- bind_rows(res) %>%
           st_intersects(grid1p) %>%
           unlist() %>%
           unique()

    # -----
    dat <- numeric(nrow(grid1p))
    dat[uid] <- 1

    # -----
    dat
  }

  # ------------------------------------------------------
  superficie <- function(dat) {
    # -----
    data(aoi)

    # -----
    res <- load_temp(dat)

    # -----
    sup <- bind_rows(res) %>%
           st_intersection(st_simplify(aoi, dTolerance = 1000)) %>%
           st_union() %>%
           st_area() %>%
           as.numeric(.) * 1e-6

    # -----
    sup
  }

  # ------------------------------------------------------
  meta_update <- function(meta, dat, accr, fr) {
    meta$rawData <- c(meta$rawData, dat)
    meta$accronyme <- c(meta$accronyme, accr)
    meta$francais <- c(meta$francais, fr)
    meta$source <- c(meta$source, paste0(dat, collapse = ","))
    meta$superficie <- c(meta$superficie, superficie(dat))
    meta
  }

  # ------------------------------------------------------
  meta_temp <- list()

  # ------------------------------------------------------
  data(grid1p)
  habitat <- grid1p

  # ------------------------------------------------------
  # Zostères : 0001, 0002, 0003
  dat <- c("0001", "0002", "0003")
  meta_temp <- meta_update(meta_temp, dat, "zostere", "Zostères")

  # -----
  habitat$zostere <- uid(dat)

  # ------------------------------------------------------
  # Zones inondables : 0013, 0014
  dat <- c("0013", "0014")
  meta_temp <- meta_update(meta_temp, dat, "zone_inondable", "Zones inondables")

  # -----
  habitat$zone_inondable <- uid(dat)


  # ------------------------------------------------------
  # Milieux humides : 0004, 0005, 0006, 0042
  # NOTE: Point database 0004 already covered by the other datasets
  meta_temp$rawData <- c(meta_temp$rawData, "0004")
  dat <- c("0005", "0006", "0042")
  meta_temp <- meta_update(meta_temp, dat, "milieu_humide", "Milieux humides")

  # -----
  habitat$milieu_humide <- uid(dat)


  # ------------------------------------------------------
  # Marais côtiers : 0007
  dat <- "0007"
  meta_temp <- meta_update(meta_temp, dat, "milieu_cotier", "Milieux côtiers")

  # -----
  habitat$milieu_cotier <- uid(dat)

  # ------------------------------------------------------
  # Sites d'alevinage : 0009
  dat <- "0009"
  meta_temp <- meta_update(meta_temp, dat, "site_alevinage", "Sites d'alevinage")

  # -----
  habitat$site_alevinage <- uid(dat)

  # ------------------------------------------------------
  # Frayères : 0010
  dat <- "0010"
  meta_temp <- meta_update(meta_temp, dat, "frayere", "Frayères")

  # -----
  habitat$frayere <- uid(dat)


  # ------------------------------------------------------
  # Zones herbacées : 0029
  dat <- "0029"
  meta_temp <- meta_update(meta_temp, dat, "zone_herbacee", "Zones herbacées")

  # -----
  habitat$zone_herbacee <- uid(dat)


  # ------------------------------------------------------
  # Espèces à statut : 0011
  dat <- "0011"
  meta_temp <- meta_update(meta_temp, dat, "espece_statut", "Espèces à statut")

  # -----
  habitat$espece_statut <- uid(dat)


  # ------------------------------------------------------
  # Habitats fauniques : 0036
  dat <- "0036"
  meta_temp <- meta_update(meta_temp, dat, "faunique", "Habitats fauniques")

  # -----
  habitat$faunique <- uid(dat)


  # ------------------------------------------------------
  # Habitats floristiques : 0037
  dat <- "0037"
  meta_temp <- meta_update(meta_temp, dat, "floristique", "Habitats floristiques")

  # -----
  habitat$floristique <- uid(dat)


  # ------------------------------------------------------
  # Colonies d'oiseaux : 0043
  dat <- "0043"
  meta_temp <- meta_update(meta_temp, dat, "colonie_oiseaux", "Colonies d'oiseaux")

  # -----
  habitat$colonie_oiseaux <- uid(dat)
  # ------------------------------------------------------------------------- #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Update metadata
  # ----------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  meta <- load_metadata("int_cv_mammiferes_marins")

  # -----
  meta$rawData <- meta_temp$rawData
  meta$dataDescription$categories$accronyme <- meta_temp$accronyme
  meta$dataDescription$categories$francais <- meta_temp$francais
  meta$dataDescription$categories$source <- meta_temp$source
  meta$dataDescription$categories$superficie <- meta_temp$superficie

  # -----
  temp <- st_drop_geometry(habitat) %>%
          rowSums(na.rm = TRUE)
  temp <- habitat[temp > 0, ]
  meta$dataDescription$spatial$extent <- st_bbox(temp)
  # _____________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # -----
  write_yaml(meta, "./data/data-metadata/int_cv_habitat.yml")

  # -----
  st_write(obj = habitat,
           dsn = "./data/data-integrated/cv_habitat.geojson",
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
