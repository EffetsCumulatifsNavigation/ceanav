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
  #   - Milieux humides : 0004, 0005, 0006, 0042 - Modifié pour 0053
  #   - Marais côtiers : 0007 - Modifié pour 0053
  #   - Sites d'alevinage : 0009
  #   - Frayères : 0010
  #   - Zones herbacées : 0029
  #   - Espèces à statut : 0011
  #   - Habitats fauniques : 0036
  #   - Habitats floristiques : 0037
  #   - Colonies d'oiseaux : 0043
  #   - TODO: Milieux sableux : 0008
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
  uid <- function(dat, category = NULL, field = NULL) {
    # -----
    res <- load_temp(dat)

    # -----
    if (is.null(category)) {
      uid <- bind_rows(res) %>%
             st_intersects(grid1p) %>%
             unlist() %>%
             unique()
    } else {
      res <- bind_rows(res)
      res <- res[res[,field,drop = TRUE] == category, ]
      uid <- res %>%
             st_intersects(grid1p) %>%
             unlist() %>%
             unique()
    }

    # -----
    dat <- numeric(nrow(grid1p))
    dat[uid] <- 1

    # -----
    dat
  }

  # ------------------------------------------------------
  superficie <- function(dat, category = NULL, field = NULL) {
    # -----
    data(aoi)

    # -----
    res <- load_temp(dat)

    # -----
    if (!is.null(category)) {
      res <- bind_rows(res)
      res <- res[res[,field,drop = TRUE] == category, ]
    }

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
  meta_update <- function(meta, dat, accr, fr, descr = "", type = "") {
    meta$rawData <- c(meta$rawData, dat)
    meta$accronyme <- c(meta$accronyme, accr)
    meta$type <- c(meta$type, type)
    meta$francais <- c(meta$francais, fr)
    meta$source <- c(meta$source, paste0(dat, collapse = ","))
    # meta$superficie <- c(meta$superficie, superficie(dat))
    meta$description <- c(meta$description, descr)
    meta
  }

  # ------------------------------------------------------
  meta_temp <- list()
  sup <- numeric()

  # ------------------------------------------------------
  data(grid1p)
  habitat <- grid1p

  # ------------------------------------------------------
  # Zostères : 0001, 0002, 0003
  dat <- c("0001", "0002", "0003")
  meta_temp <- meta_update(meta_temp, dat, "zostere", "Zostères", "Zostères")
  habitat$zostere <- uid(dat)
  sup <- c(sup, superficie(dat))

  # ------------------------------------------------------
  # Zones inondables : 0013, 0014
  dat <- c("0013", "0014")
  meta_temp <- meta_update(meta_temp, dat, "zone_inondable", "Zones inondables", "Zones inondables")
  habitat$zone_inondable <- uid(dat)
  sup <- c(sup, superficie(dat))

  # ------------------------------------------------------
  # Milieux humides : 0004, 0005, 0006, 0042
  # NOTE (2021-10-08): switching to  0053, which incorporates all those datasets and divides
  #                    milieux humides into several categories
  # -----
  # Eau peu profonde
  dat <- c("0053")
  meta_temp <- meta_update(meta_temp, dat, "eau_peu_profonde", "Eau peu profonde", "Milieu humide dont le niveau d’eau est inférieur à 2 m et présentant des plantes aquatiques flottantes ou submergées ainsi que des plantes émergentes dont le couvert fait moins de 25 % de la superficie du milieu.", "Milieu humide")
  habitat$eau_peu_profonde <- uid(dat, "Eau", "TYPE")
  sup <- c(sup, superficie(dat, "Eau", "TYPE"))

  # -----
  # Marais
  meta_temp <- meta_update(meta_temp, dat, "marais", "Marais", "Milieu humide sur dépôt minéral, dominé par une végétation herbacée couvrant plus de 25 % de la superficie. Les arbustes et les arbres, lorsque présents, couvrent moins de 25 % de la superficie du milieu.", "Milieu humide")
  habitat$marais <- uid(dat, "Marais", "TYPE")
  sup <- c(sup, superficie(dat, "Marais", "TYPE"))



  # -----
  # Marécage
  meta_temp <- meta_update(meta_temp, dat, "marecage", "Marécage", "Milieu humide sur dépôt minéral, dominé par une végétation ligneuse arbustive ou arborescente, avec plus de 25 % de couvert.", "Milieu humide")
  habitat$marecage <- uid(dat, "Marécage", "TYPE")
  sup <- c(sup, superficie(dat, "Marécage", "TYPE"))


  # -----
  # Milieu humide
  meta_temp <- meta_update(meta_temp, dat, "milieu_humide", "Milieu humide", "Regroupe les milieux humides dont le type est inconnu.", "Milieu humide")
  habitat$milieu_humide <- uid(dat, "Milieu humide", "TYPE")
  sup <- c(sup, superficie(dat, "Milieu humide", "TYPE"))


  # -----
  # Tourbière
  meta_temp <- meta_update(meta_temp, dat, "tourbiere", "Tourbière", "Regroupe les milieux humides dans lesquels il y a une accumulation de tourbe d’au moins 30 cm d’épaisseur.", "Milieu humide")
  habitat$tourbiere <- uid(dat, "Tourbière", "TYPE")
  sup <- c(sup, superficie(dat, "Tourbière", "TYPE"))



  # ------------------------------------------------------
  # Marais côtiers : 0007
  # NOTE (2021-10-08): switching to  0053, which incorporates this data and divides
  #                    milieux humides into several categories
  # dat <- "0007"
  # meta_temp <- meta_update(meta_temp, dat, "marais_cotier", "Marais côtiers")
  # habitat$marais_cotier <- uid(dat)

  # ------------------------------------------------------
  # Sites d'alevinage : 0009
  dat <- "0009"
  meta_temp <- meta_update(meta_temp, dat, "site_alevinage", "Sites d'alevinage", "Sites d'alevinage")
  habitat$site_alevinage <- uid(dat)
  sup <- c(sup, superficie(dat))

  # ------------------------------------------------------
  # Frayères : 0010
  dat <- "0010"
  meta_temp <- meta_update(meta_temp, dat, "frayere", "Frayères", "Frayères")
  habitat$frayere <- uid(dat)
  sup <- c(sup, superficie(dat))


  # ------------------------------------------------------
  # Zones herbacées : 0029
  # NOTE (2021-10-08): switching to  0053, which incorporates this data and divides
  #                    milieux humides into several categories
  # dat <- "0029"
  # meta_temp <- meta_update(meta_temp, dat, "zone_herbacee", "Zones herbacées")
  # habitat$zone_herbacee <- uid(dat)


  # ------------------------------------------------------
  # Espèces à statut : 0011
  dat <- "0011"
  meta_temp <- meta_update(meta_temp, dat, "espece_statut", "Espèces à statut", "Espèces à statut")
  habitat$espece_statut <- uid(dat)
  sup <- c(sup, superficie(dat))


  # ------------------------------------------------------
  # Habitats fauniques : 0036
  dat <- "0036"
  meta_temp <- meta_update(meta_temp, dat, "faunique", "Habitats fauniques", "Habitats fauniques")
  habitat$faunique <- uid(dat)
  sup <- c(sup, superficie(dat))


  # ------------------------------------------------------
  # Habitats floristiques : 0037
  dat <- "0037"
  meta_temp <- meta_update(meta_temp, dat, "floristique", "Habitats floristiques", "Habitats floristiques")
  habitat$floristique <- uid(dat)
  sup <- c(sup, superficie(dat))


  # ------------------------------------------------------
  # Colonies d'oiseaux : 0043
  dat <- "0043"
  meta_temp <- meta_update(meta_temp, dat, "colonie_oiseaux", "Colonies d'oiseaux", "Colonies d'oiseaux")
  habitat$colonie_oiseaux <- uid(dat)
  sup <- c(sup, superficie(dat))
  # ------------------------------------------------------------------------- #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Update metadata
  # ----------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  meta <- load_metadata("int_cv_habitat")

  # -----
  meta$rawData <- meta_temp$rawData
  meta$dataDescription$categories$accronyme <- meta_temp$accronyme
  meta$dataDescription$categories$francais <- meta_temp$francais
  meta$dataDescription$categories$source <- meta_temp$source
  meta$dataDescription$categories$superficie <-sup
  meta$dataDescription$categories$type <- meta_temp$type

  # --- For proper referencing in markdown syntax
  meta$dataDescription$categories$mdref <- modif_md(meta$dataDescription$categories$accronyme)

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
