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
  uid <- function(...) {
    # -----
    uid <- bind_rows(...) %>%
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
  meta <- load_metadata("int_cv_habitat")
  meta <- metadata_int_cv_habitat
  meta$rawData <- NULL

  # ------------------------------------------------------
  data(grid1p)
  habitat <- grid1p

  # ------------------------------------------------------
  # Zostères : 0001, 0002, 0003
  meta$rawData <- c(meta$rawData, "0001", "0002", "0003")

  # -----
  load_format("data0001")
  load_format("data0002")
  load_format("data0003")

  # -----
  habitat$zostere <- uid(data0001, data0002, data0003)

  # ------------------------------------------------------
  # Zones inondables : 0013, 0014
  meta$rawData <- c(meta$rawData, "0013", "0014")

  # -----
  load_format("data0013")
  load_format("data0014")

  # -----
  habitat$zone_inondable <- uid(data0013, data0014)


  # ------------------------------------------------------
  # Milieux humides : 0004, 0005, 0006, 0042
  meta$rawData <- c(meta$rawData, "0004", "0005", "0006", "0042")

  # -----
  # load_format("data0004") Point database already covered by the other two datasets
  load_format("data0005")
  load_format("data0006")
  load_format("data0042")

  # -----
  habitat$milieu_humide <- uid(data0005, data0006, data0042)


  # ------------------------------------------------------
  # Marais côtiers : 0007
  meta$rawData <- c(meta$rawData, "0007")

  # -----
  load_format("data0007")

  # -----
  habitat$milieu_cotier <- uid(data0007)

  # ------------------------------------------------------
  # Sites d'alevinage : 0009
  meta$rawData <- c(meta$rawData, "0009")

  # -----
  load_format("data0009")

  # -----
  habitat$site_alevinage <- uid(data0009)

  # ------------------------------------------------------
  # Frayères : 0010
  meta$rawData <- c(meta$rawData, "0010")

  # -----
  load_format("data0010")

  # -----
  habitat$frayere <- uid(data0010)


  # ------------------------------------------------------
  # Zones herbacées : 0029
  meta$rawData <- c(meta$rawData, "0029")

  # -----
  load_format("data0029")

  # -----
  habitat$zone_herbacee <- uid(data0029)


  # ------------------------------------------------------
  # Espèces à statut : 0011
  meta$rawData <- c(meta$rawData, "0011")

  # -----
  load_format("data0011")

  # -----
  habitat$espece_statut <- uid(data0011)


  # ------------------------------------------------------
  # Habitats fauniques : 0036
  meta$rawData <- c(meta$rawData, "0036")

  # -----
  load_format("data0036")

  # -----
  habitat$faunique <- uid(data0036)


  # ------------------------------------------------------
  # Habitats floristiques : 0037
  meta$rawData <- c(meta$rawData, "0037")

  # -----
  load_format("data0037")

  # -----
  habitat$floristique <- uid(data0037)


  # ------------------------------------------------------
  # Colonies d'oiseaux : 0043
  meta$rawData <- c(meta$rawData, "0043")

  # -----
  load_format("data0043")

  # -----
  habitat$colonie_oiseaux <- uid(data0043)
  # ------------------------------------------------------------------------- #

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
           delete_dsn = TRUE)
  # ------------------------------------------------------------------------- #}

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Clean global environment
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  clean()
  # ------------------------------------------------------------------------- #}
}
