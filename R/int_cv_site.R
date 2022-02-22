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
  #   - Wolastoqiyik Wahsipekuk - Pêche commerciale : 0025, 0026, 0072
  #   - Kahnawake - Cultural Sites : 0032
  #   - Kahnawake - Fishing Commercial : 0032
  #   - Kahnawake - Hunting : 0032
  #   - Kahnawake - Near Shore Fishing : 0032
  #   - Kahnawake - Open Water Fishing : 0032
  #   - Kahnawake - SSSM : 0032
  #   - Kahnawake - Traffic : 0032
  #   - Kahnawake - Vegetation : 0032
  #   - Milieux protégés : 0030, 0038, 0039, 0040
  #   - Sites patrimoniaux : 0044, 0045
  #   - GCNWA - Gibier : 0066
  #   - GCNWA - Oiseaux migrateurs : 0066
  #   - GCNWA - Animaux à fourrure : 0066
  #   - GCNWA - Cueillette et collecte : 0066
  #   - GCNWA - Sites de coucher : 0066
  #   - GCNWA - Sites culturels : 0066
  #   - GCNWA - Sites essentiels : 0066
  #   - GCNWA - Problèmes liés au territoire : 0066
  #   - GCNWA - Zones d'activités : 0066
  #   - GCNWA - Pêche : 0066
  #   - GCNWA - Navigation : 0066
  #   - GCNWA - Sites archéologiques : 0067
  #   - GCNWA - Sites à potentiel archéologique : 0068
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
  meta_update <- function(meta, dat, accr, fr, type = "") {
    meta$rawData <- c(meta$rawData, dat)
    meta$accronyme <- c(meta$accronyme, accr)
    meta$francais <- c(meta$francais, fr)
    meta$source <- c(meta$source, paste0(dat, collapse = ","))
    meta$type <- c(meta$type, type)
    meta
  }

  # ------------------------------------------------------
  meta_temp <- list()

  # ------------------------------------------------------
  data(grid1p)
  site <- grid1p


  # ================================================================================================
  # ------------------------------------------------------
  # AGHAMM - pêche commerciale : 0022
  dat <- "0022"
  meta_temp <- meta_update(meta_temp, dat, "aghamm_peche_commerciale", "Pêche commerciale", type = "Association de gestion halieutique Mi'kmaq et Malécite")

  # -----
  site$aghamm_peche_commerciale <- uid(dat)

  # ----------------------------
  # AGHAMM - pêche au saumon Atlantique : 0023
  dat <- "0023"
  meta_temp <- meta_update(meta_temp, dat, "aghamm_peche_traditionnelle", "Pêche traditionnelle", type = "Association de gestion halieutique Mi'kmaq et Malécite")

  # -----
  site$aghamm_peche_traditionnelle <- uid(dat)

  # ================================================================================================
  # ----------------------------
  # Essipit - Culture et patrimoine : 0024
  dat <- "0024"
  meta_temp <- meta_update(meta_temp, dat, "essipit_culture_patrimoine", "Culture et patrimoine", type = "Nation des Innus d’Essipit")

  # -----
  site$essipit_culture_patrimoine <- uid(dat, "Culture et patrimoine", "Categorie")

  # ----------------------------
  # Essipit - Pêche traditionnelle : 0024
  meta_temp <- meta_update(meta_temp, dat, "essipit_peche_traditionnelle", "Pêche traditionnelle", type = "Nation des Innus d’Essipit")
  site$essipit_peche_traditionnelle <- uid(dat, "Pêche traditionnelle", "Categorie")

  # ----------------------------
  # Essipit - Chasse oiseaux migrateurs : 0024
  meta_temp <- meta_update(meta_temp, dat, "essipit_chasse_oiseaux", "Chasse aux oiseaux migrateurs", type = "Nation des Innus d’Essipit")
  site$essipit_chasse_oiseaux <- uid(dat, "Chasse oiseaux migrateurs", "Categorie")

  # ----------------------------
  # Essipit - Chasse phoque : 0024
  meta_temp <- meta_update(meta_temp, dat, "essipit_chasse_phoque", "Chasse aux phoques", type = "Nation des Innus d’Essipit")
  site$essipit_chasse_phoque <- uid(dat, "Chasse phoque", "Categorie")

  # ----------------------------
  # Essipit - Pêche commerciale : 0024
  meta_temp <- meta_update(meta_temp, dat, "essipit_peche_commerciale", "Pêche commerciale", type = "Nation des Innus d’Essipit")
  site$essipit_peche_commerciale <- uid(dat, "Pêche commerciale", "Categorie")

  # ----------------------------
  # Essipit - Activités touristiques : 0024
  meta_temp <- meta_update(meta_temp, dat, "essipit_tourisme", "Activités touristiques", type = "Nation des Innus d’Essipit")
  site$essipit_tourisme <- uid(dat, "Activités touristiques", "Categorie")

  # ----------------------------
  # Essipit - Accès au plan d'eau : 0024
  meta_temp <- meta_update(meta_temp, dat, "essipit_acces_eau", "Accès au plan d'eau", type = "Nation des Innus d’Essipit")
  site$essipit_acces_eau <- uid(dat, "Accès au plan d'eau", "Categorie")

  # ================================================================================================
  # ----------------------------
  # Wolastoqiyik Wahsipekuk - Pêche commerciale : 0025, 0026
  dat <- c("0025", "0026")
  meta_temp <- meta_update(meta_temp, dat, "wolastoqiyik_wahsipekuk_peche_commerciale", "Pêche commerciale", type = "Nation Wolastoqiyik Wahsipekuk")

  # -----
  site$wolastoqiyik_wahsipekuk_peche_commerciale <- uid(dat)

  # Sites d'intérêt supplémentaires partagé en février 2022
  dat <- "0072"
  meta_temp <- meta_update(meta_temp, dat, "wolastoqiyik_wahsipekuk_observation_beluga", "Observation du béluga", type = "Nation Wolastoqiyik Wahsipekuk")
  site$wolastoqiyik_wahsipekuk_observation_beluga <- uid(dat, "Observation du béluga", "nom")

  # ---
  meta_temp <- meta_update(meta_temp, dat, "wolastoqiyik_wahsipekuk_developpement_portuaire", "Développement portuaire", type = "Nation Wolastoqiyik Wahsipekuk")
  site$wolastoqiyik_wahsipekuk_developpement_portuaire <- uid(dat, "Développement portuaire", "nom")


  # ================================================================================================
  # ----------------------------
  # Kahnawake - Cultural Sites : 0032
  dat <- "0032"
  meta_temp <- meta_update(meta_temp, dat, "kahnawake_culture_patrimoine", "Culture et patrimoine", type = "Nation Mohawk de Kahnawà:ke")
  site$kahnawake_culture_patrimoine <- uid(dat, "Cultural Sites", "category")

  # ----------------------------
  # Kahnawake - Hunting : 0032
  meta_temp <- meta_update(meta_temp, dat, "kahnawake_chasse", "Chasse à la sauvagine", type = "Nation Mohawk de Kahnawà:ke")
  site$kahnawake_chasse <- uid(dat, "Hunting", "category")


  # ----------------------------
  # Kahnawake - Near Shore Fishing : 0032
  meta_temp <- meta_update(meta_temp, dat, "kahnawake_peche_rivage", "Pêche de rivage", type = "Nation Mohawk de Kahnawà:ke")
  site$kahnawake_peche_rivage <- uid(dat, "Near Shore Fishing", "category")

  # ----------------------------
  # Kahnawake - Open Water Fishing : 0032
  meta_temp <- meta_update(meta_temp, dat, "kahnawake_peche_offshore", "Pêche en eau libre", type = "Nation Mohawk de Kahnawà:ke")
  site$kahnawake_peche_offshore <- uid(dat, "Open Water Fishing", "category")

  # ----------------------------
  # Kahnawake - SSSM : 0032
  meta_temp <- meta_update(meta_temp, dat, "kahnawake_sssm", "Seigneurie du Sault-Saint-Louis", type = "Nation Mohawk de Kahnawà:ke")
  site$kahnawake_sssm <- uid(dat, "SSSM", "category")


  # ----------------------------
  # Kahnawake - Traffic : 0032
  meta_temp <- meta_update(meta_temp, dat, "kahnawake_traffic", "Navigation", type = "Nation Mohawk de Kahnawà:ke")
  site$kahnawake_traffic <- uid(dat, "Traffic", "category")

  # ----------------------------
  # Kahnawake - Vegetation : 0032
  meta_temp <- meta_update(meta_temp, dat, "kahnawake_vegatation", "Récolte de végétation", type = "Nation Mohawk de Kahnawà:ke")
  site$kahnawake_vegatation <- uid(dat, "Vegetation", "category")


  # ================================================================================================
  # ----------------------------
  # Milieux protégés : 0030, 0038, 0039, 0040
  dat <- c("0030", "0038", "0039")#, "0040")
  meta_temp <- meta_update(meta_temp, dat, "public_milieu_protege", "Milieux protégés", type = "Public")


  # ================================================================================================
  # ------------------------------------------------------
  # -----
  # NOTE: Function not working for this one
  uid2 <- function(...) {
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
  # -----
  load_format("data0030")
  load_format("data0038")
  load_format("data0039")
  # load_format("data0040") # Pas de données dans notre zone d'étude
  nm <- c("Habitat faunique","Habitat d'une espèce floristique menacée ou vulnérable",
          "Réserve de territoire aux fins d'aire protégée")
  iid <- data0038$DESIG_GR %in% nm
  data0038 <- data0038[!iid, ]
  site$public_milieu_protege <- uid2(data0030, data0038, data0039)#, data0040)


  # ================================================================================================
  # ----------------------------
  # Sites patrimoniaux : 0044, 0045
  dat <- c("0044", "0045")
  meta_temp <- meta_update(meta_temp, dat, "public_sites_patrimoniaux", "Sites patrimoniaux", type = "Public")

  # -----
  load_format("data0044")
  load_format("data0045")
  data0044 <- select(data0044, geometry)
  data0045 <- select(data0045, geometry)

  # -----
  site$public_sites_patrimoniaux <- uid2(data0044, data0045)


  # ================================================================================================
  # ----------------------------
  # Nation Huronne-Wendat
  # Huronne-Wendat - Activités récréatives - 0055
  # Terme modifié suite aux commentaires à Edgar. Éviter d'utiliser le terme récréatif puisque ça minimise l'importance de ces activités
  dat <- "0055"
  meta_temp <- meta_update(meta_temp, dat, "huronne_wendat_activite_recreative", "Activités rituelles ou sociales", type = "Nation Huronne-Wendat")
  site$huronne_wendat_activite_recreative <- uid(dat, "Activité récréative", "Thème")

  # ----------------------------
  # Huronne-Wendat Pêche - 0055
  meta_temp <- meta_update(meta_temp, dat, "huronne_wendat_peche", "Pêche", type = "Nation Huronne-Wendat")
  site$huronne_wendat_peche <- uid(dat, "Pêche", "Thème")

  # ----------------------------
  # Huronne-Wendat Chasse - 0055
  meta_temp <- meta_update(meta_temp, dat, "huronne_wendat_chasse", "Chasse", type = "Nation Huronne-Wendat")
  site$huronne_wendat_chasse <- uid(dat, "Chasse", "Thème")

  # ----------------------------
  # Huronne-Wendat Récolte de végétaux - 0055
  meta_temp <- meta_update(meta_temp, dat, "huronne_wendat_vegetaux", "Récolte de végétaux", type = "Nation Huronne-Wendat")
  site$huronne_wendat_vegetaux <- uid(dat, "Récolte de végétaux", "Thème")

  # ----------------------------
  # Huronne-Wendat - Occupation - 0055 : Ne se retrouve pas dans notre zone d'étude
  meta_temp <- meta_update(meta_temp, dat, "huronne_wendat_occupation", "Occupation du territoire", type = "Nation Huronne-Wendat")
  site$huronne_wendat_occupation <- uid(dat, "Occupation H-W", "Thème")

  # ----------------------------
  # Huronne-Wendat - Archéologie - 0055
  meta_temp <- meta_update(meta_temp, dat, "huronne_wendat_archeologie", "Archéologie", type = "Nation Huronne-Wendat")
  site$huronne_wendat_archeologie <- uid(dat, "Archéologie", "Thème")

  # ----------------------------
  # Huronne-Wendat - Toponymie - 0055
  meta_temp <- meta_update(meta_temp, dat, "huronne_wendat_toponymie", "Toponymie", type = "Nation Huronne-Wendat")
  site$huronne_wendat_toponymie <- uid(dat, "Toponymie", "Thème")

  # ----------------------------
  # Huronne-Wendat - Histoire - 0055
  meta_temp <- meta_update(meta_temp, dat, "huronne_wendat_histoire", "Histoire", type = "Nation Huronne-Wendat")
  site$huronne_wendat_histoire <- uid(dat, "Histoire", "Thème")

  # ----------------------------
  # Huronne-Wendat - Espèce en péril - 0055
  meta_temp <- meta_update(meta_temp, dat, "huronne_wendat_espece_peril", "Espèce en péril", type = "Nation Huronne-Wendat")
  site$huronne_wendat_espece_peril <- uid(dat, "Espèce en péril", "Thème")

  # ================================================================================================
  # ----------------------------
  # GCNWA - Gibier : 0066
  dat <- "0066"
  meta_temp <- meta_update(meta_temp, dat, "gcnwa_gibier", "Chasse au gibier", type = "Nation W8banaki")
  site$gcnwa_gibier <- uid(dat, "Gibier", "category_ressource")

  # ----------------------------
  # GCNWA - Oiseaux migrateurs : 0066
  meta_temp <- meta_update(meta_temp, dat, "gcnwa_oiseaux_migrateurs", "Chasse aux oiseaux migrateurs", type = "Nation W8banaki")
  site$gcnwa_oiseaux_migrateurs <- uid(dat, "Oiseaux migrateurs", "category_ressource")

  # ----------------------------
  # GCNWA - Animaux à fourrure : 0066
  meta_temp <- meta_update(meta_temp, dat, "gcnwa_animaux_fourrure", "Trappe d'animaux à fourrure", type = "Nation W8banaki")
  site$gcnwa_animaux_fourrure <- uid(dat, "Animaux à fourrure", "category_ressource")

  # ----------------------------
  # GCNWA - Cueillette et collecte : 0066
  meta_temp <- meta_update(meta_temp, dat, "gcnwa_cueillette_collecte", "Cueillette et collecte de végétaux", type = "Nation W8banaki")
  site$gcnwa_cueillette_collecte <- uid(dat, "Cueillette et collecte", "category_ressource")

  # ----------------------------
  # GCNWA - Sites de coucher : 0066
  meta_temp <- meta_update(meta_temp, dat, "gcnwa_sites_coucher", "Sites de coucher", type = "Nation W8banaki")
  site$gcnwa_sites_coucher <- uid(dat, "Sites de coucher", "category_ressource")

  # ----------------------------
  # GCNWA - Sites culturels : 0066
  meta_temp <- meta_update(meta_temp, dat, "gcnwa_sites_culturels", "Sites culturels", type = "Nation W8banaki")
  site$gcnwa_sites_culturels <- uid(dat, "Sites culturels", "category_ressource")

  # ----------------------------
  # GCNWA - Sites essentiels : 0066
  meta_temp <- meta_update(meta_temp, dat, "gcnwa_sites_essentiels", "Sites essentiels", type = "Nation W8banaki")
  site$gcnwa_sites_essentiels <- uid(dat, "Sites essentiels", "category_ressource")

  # ----------------------------
  # GCNWA - Problèmes liés au territoire : 0066
  meta_temp <- meta_update(meta_temp, dat, "gcnwa_problemes_territoire", "Problèmes liés à l'accès au territoire", type = "Nation W8banaki")
  site$gcnwa_problemes_territoire <- uid(dat, "Problèmes liés au territoire", "category_ressource")

  # ----------------------------
  # GCNWA - Zones d'activités : 0066
  meta_temp <- meta_update(meta_temp, dat, "gcnwa_zones_activites", "Zones d'activités", type = "Nation W8banaki")
  site$gcnwa_zones_activites <- uid(dat, "Zones d'activités", "category_ressource")

  # ----------------------------
  # GCNWA - Pêche : 0066
  meta_temp <- meta_update(meta_temp, dat, "gcnwa_peche", "Pêche", type = "Nation W8banaki")
  site$gcnwa_peche <- uid(dat, "Pêche", "category_ressource")

  # ----------------------------
  # GCNWA - Navigation : 0066
  meta_temp <- meta_update(meta_temp, dat, "gcnwa_navigation", "Navigation", type = "Nation W8banaki")
  site$gcnwa_navigation <- uid(dat, "Navigation", "category_ressource")


  # ----------------------------
  # GCNWA - Sites archéologiques : 0067
  dat <- "0067"
  meta_temp <- meta_update(meta_temp, dat, "gcnwa_sites_archeologiques", "Sites archéologiques", type = "Nation W8banaki")
  site$gcnwa_sites_archeologiques <- uid(dat)

  # ----------------------------
  # GCNWA - Sites à potentiel archéologique : 0068
  dat <- "0068"
  meta_temp <- meta_update(meta_temp, dat, "gcnwa_sites_potentiel_archeologique", "Sites à potentiel archéologique", type = "Nation W8banaki")
  site$gcnwa_sites_potentiel_archeologique <- uid(dat)


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Update metadata
  # ----------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  meta <- load_metadata("int_cv_site")

  # -----
  meta$rawData <- meta_temp$rawData
  meta$dataDescription$categories$accronyme <- meta_temp$accronyme
  meta$dataDescription$categories$francais <- meta_temp$francais
  meta$dataDescription$categories$source <- meta_temp$source
  meta$dataDescription$categories$superficie <- meta_temp$superficie
  meta$dataDescription$categories$type <- meta_temp$type

  # --- For proper referencing in markdown syntax
  meta$dataDescription$categories$mdref <- modif_md(meta$dataDescription$categories$accronyme)

  # -----
  temp <- st_drop_geometry(site) %>%
          rowSums(na.rm = TRUE)
  temp <- site[temp > 0, ]
  meta$dataDescription$spatial$extent <- st_bbox(temp)
  # _____________________________________________________________________________ #


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
