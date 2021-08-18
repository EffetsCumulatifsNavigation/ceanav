#' Sites d’intérêt culturels, patrimoniaux et archéologiques
#'
#' Intégration des données utilisées pour caractériser la composante valorisée des sites d’intérêt culturels, patrimoniaux et archéologiques
#'
#' @keywords sites d’intérêt culturels, patrimoniaux et archéologiques
#' @keywords composante valorisée
#'
#' @export
#'
#' @details Cette fonction télécharge et formatte les données
#'

cv_site <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Notes
  # ------------------------------------
  #
  # Plusieurs sites ont été identifiés à travers ce projet grâce aux données
  # ouvertement disponibles ainsi qu'aux collaborations développées avec les
  # Premières Nations. Il est toutefois important de mentionner que ces sites
  # sont représentatifs des objectifs de notre étude, des collaborations
  # développées et des personnes impliquées dans le projet. Ces sites ne
  # devraient donc pas être perçus comme une liste exhaustive de l'ensemble des
  # sites d'importance dans la région d'étude. De plus, bien que les représentants
  # des Premières Nations ont identifiés des sites ayant une importance
  # particulière pour leurs communautés, ça ne veut pas dire que les milieux
  # qui ne sont pas identifiés n'ont aucune importance et qu'ils renoncent à
  # ces territoires.
  #
  # Il est clair que d'autres sites pourraient être ajoutés à la liste actuelle.
  # La liste des sites considérés actuellement est la suivante:
  #
  #   - AGHAMM - pêche commerciale : 0022
  #   - AGHAMM - pêche au saumon Atlantique : 0023
  #   - Essipit - Culture et patrimoine : 0024
  #   - Essipit - Pêche traditionnelle : 0024
  #   - Essipit - Chasse oiseaux migrateurs : 0024
  #   - Essipit - Chasse phoque : 0024
  #   - Essipit - Pêche commerciale : 0024
  #   - Essipit - Activités touristiques : 0024
  #   - Essipit - Accès au plan d'eau : 0024
  #   - Wolastoqiyik Wahsipekuk - Pêche commerciale : 0025, 0026
  #   - Kahnawake - Cultural Sites : 0032
  #   - Kahnawake - Fishing Commercial : 0032
  #   - Kahnawake - Hunting : 0032
  #   - Kahnawake - Near Shore Fishing : 0032
  #   - Kahnawake - Open Water Fishing : 0032
  #   - Kahnawake - SSSM : 0032
  #   - Kahnawake - Traffic : 0032
  #   - Kahnawake - Vegetation : 0032
  #   - Milieux protégés : 0030, 0038, 0039, 0040
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
  meta <- load_metadata("int_cv_site")
  meta <- metadata_int_cv_site
  meta$rawData <- NULL

  # ------------------------------------------------------
  data(grid1p)
  site <- grid1p

  # ------------------------------------------------------
  # AGHAMM - pêche commerciale : 0022
  meta$rawData <- c(meta$rawData, "0022")

  # -----
  load_format("data0022")

  # -----
  site$aghamm_peche_commerciale <- uid(data0022)

  # ----------------------------
  # AGHAMM - pêche au saumon Atlantique : 0023
  meta$rawData <- c(meta$rawData, "0023")

  # -----
  load_format("data0023")

  # -----
  site$aghamm_peche_traditionnelle <- uid(data0023)

  # ----------------------------
  # Essipit - Culture et patrimoine : 0024
  meta$rawData <- c(meta$rawData, "data0024")

  # -----
  load_format("data0024")

  # -----
  iid <- data0024$Categorie == "Culture et patrimoine"
  site$essipit_culture_patrimoine <- uid(data0024[iid, ])

  # ----------------------------
  # Essipit - Pêche traditionnelle : 0024
  iid <- data0024$Categorie == "Pêche traditionnelle"
  site$essipit_peche_traditionnelle <- uid(data0024[iid, ])

  # ----------------------------
  # Essipit - Chasse oiseaux migrateurs : 0024
  iid <- data0024$Categorie == "Chasse oiseaux migrateurs"
  site$essipit_chasse_oiseaux <- uid(data0024[iid, ])

  # ----------------------------
  # Essipit - Chasse phoque : 0024
  iid <- data0024$Categorie == "Chasse phoque"
  site$essipit_chasse_phoque <- uid(data0024[iid, ])

  # ----------------------------
  # Essipit - Pêche commerciale : 0024
  iid <- data0024$Categorie == "Pêche commerciale"
  site$essipit_peche_commerciale <- uid(data0024[iid, ])

  # ----------------------------
  # Essipit - Activités touristiques : 0024
  iid <- data0024$Categorie == "Activités touristiques"
  site$essipit_tourisme <- uid(data0024[iid, ])

  # ----------------------------
  # Essipit - Accès au plan d'eau : 0024
  iid <- data0024$Categorie == "Accès au plan d'eau"
  site$essipit_acces_eau <- uid(data0024[iid, ])

  # ----------------------------
  # Wolastoqiyik Wahsipekuk - Pêche commerciale : 0025, 0026
  meta$rawData <- c(meta$rawData, "0025", "0026")

  # -----
  load_format("data0025")
  load_format("data0026")
  data0026 <- rename(data0026, iid = id)

  # -----
  site$wolastoqiyik_wahsipekuk_peche_commerciale <- uid(data0025, data0026)

  # ----------------------------
  # Kahnawake - Cultural Sites : 0032
  meta$rawData <- c(meta$rawData, "0032")

  # -----
  load_format("data0032")

  # -----
  iid <- data0032$category == "Cultural Sites"
  site$kahnawake_culture_patrimoine <- uid(data0032[iid, ])

  # ----------------------------
  # Kahnawake - Fishing Commercial : 0032
  iid <- data0032$category == "Fishing Commercial"
  site$kahnawake_peche_commerciale <- uid(data0032[iid, ])


  # ----------------------------
  # Kahnawake - Hunting : 0032
  iid <- data0032$category == "Hunting"
  site$kahnawake_chasse <- uid(data0032[iid, ])


  # ----------------------------
  # Kahnawake - Near Shore Fishing : 0032
  iid <- data0032$category == "Near Shore Fishing"
  site$kahnawake_peche_rivage <- uid(data0032[iid, ])


  # ----------------------------
  # Kahnawake - Open Water Fishing : 0032
  iid <- data0032$category == "Open Water Fishing"
  site$kahnawake_peche_offshore <- uid(data0032[iid, ])


  # ----------------------------
  # Kahnawake - SSSM : 0032
  iid <- data0032$category == "SSSM"
  site$kahnawake_sssm <- uid(data0032[iid, ])


  # ----------------------------
  # Kahnawake - Traffic : 0032
  iid <- data0032$category == "Traffic"
  site$kahnawake_traffic <- uid(data0032[iid, ])


  # ----------------------------
  # Kahnawake - Vegetation : 0032
  iid <- data0032$category == "Vegetation"
  site$kahnawake_vegatation <- uid(data0032[iid, ])


  # ----------------------------
  # Milieux protégés : 0030, 0038, 0039, 0040
  meta$rawData <- c(meta$rawData, "0030", "0038", "0039", "0040")

  # -----
  load_format("data0030")
  load_format("data0038")
  load_format("data0039")
  load_format("data0040")

  # -----
  site$milieu_protege <- uid(data0030, data0038, data0039, data0040)


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # -----
  write_yaml(meta, "./data/data-metadata/int_cv_site.yml")

  # -----
  st_write(obj = site,
           dsn = "./data/data-integrated/cv_site.geojson",
           delete_dsn = TRUE)
  # ------------------------------------------------------------------------- #}

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Clean global environment
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  clean()
  # ------------------------------------------------------------------------- #}
}
