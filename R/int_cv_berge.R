#' Berges: Données sur l'intégrité des berges
#'
#' Intégration des données utilisées pour caractériser la composante valorisée d'intégrité des berges
#'
#' @keywords berge
#' @keywords composante valorisée
#'
#' @export
#'
#' @details Cette fonction télécharge et formatte les données
#'

cv_berge <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Prepare data
  # ------------------------------------
  #
  # All we will do for now for this dataset is include it as presence-absence
  # in the study grid., but for different categories of coastal segments
  #
  # Les données fournies caractérisent déjà l'état des verge en 3 catégories distinctes :
  #   - Active ou vive (VIVE) : Érosion apparente ou couvert végétal < 25%
  #   - Semi-végétalisée (SV) : Érosion apparente ou couvert végétal de 25 à 75%
  #   - Stable ou végétalisé : Aucun signe d’érosion pparent, et couvert végétal > 75%
  #                            ou présence d’une structure de protection
  #
  # En plus de ces catégories, nous différencions entre les berges naturelles ou artificialisées
  # pour notre évaluation, résultant ainsi en 6 catégories potentielles. Considérant la structure
  # des données, il en résultate 4 catégories puisque nous n'incluons pas les berges végétalisées,
  # qui ne sont pas à risque d'érosion.
  #
  # WARNING: there is still work to do here, at this phase this is exploratory
  # TODO: see if if makes sense to divide by erosion processes identified in dataset
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  load_format("data0017")
  data(grid1p)

  # ----------
  categories <- c(
    "naturelle_semi_vegetalisee", # Naturelle - Semi-végétalisée
    "naturelle_vive", # Naturelle - Vive
    "artificielle_semi_vegetalisee", # Artificielle - Semi-végétalisée
    "artificielle_vive" # Artificielle - Vive
  )

  # ----------
  uid <- data0017$Artificiel == "N" &
         data0017$Etat_Berge == "SV"
  data0017$categories[uid] <- categories[1]

  # ----------
  uid <- data0017$Artificiel == "N" &
         data0017$Etat_Berge == "VIVE"
  data0017$categories[uid] <- categories[2]

  # ----------
  uid <- data0017$Artificiel == "O" &
         data0017$Etat_Berge == "SV"
  data0017$categories[uid] <- categories[3]

  # ----------
  uid <- data0017$Artificiel == "O" &
         data0017$Etat_Berge == "VIVE"
  data0017$categories[uid] <- categories[4]

  # ----------
  uid <- !is.na(data0017$categories)
  data0017 = data0017[uid, ]

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Include to study grid
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  grd <- list()
  for(i in 1:length(categories)) {
    uid <- data0017$categories == categories[i]
    grd[[i]] <- st_intersects(data0017[uid, ], grid1p) %>%
               unlist() %>%
               unique()
  }

  # ----------
  berge <- grid1p
  for(i in 1:length(categories)) {
    berge[,categories[i]] <- 0
    berge[grd[[i]], categories[i]] <- 1
  }
  # ------------------------------------------------------------------------- #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Update metadata
  # ----------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  meta <- load_metadata("int_cv_berge")

  # -----
  meta$rawData <- c("0017")

  # -----
  meta$dataDescription$spatial$extent <- st_bbox(data0017)

  # -----
  meta$dataDescription$categories$accronyme <-  categories
  meta$dataDescription$categories$francais <-  c(
    "Naturelle - Semi-végétalisée (IE = 1)",
    "Naturelle - Vive (IE = 2)",
    "Artificielle - Semi-végétalisée (IE = 1)",
    "Artificielle - Vive (IE = 2)"
  )
  meta$dataDescription$categories$english <-  c(
    "Natural - Semi-vegetated (EI = 1)",
    "Natural - Active (EI = 2)",
    "Artificial - Semi-vegetated (EI = 1)",
    "Artificial - Active (EI = 2)"
  )

  meta$dataDescription$categories$type <-  c(
    "Naturelle",
    "Naturelle",
    "Artificielle",
    "Artificielle"
  )
  meta$dataDescription$categories$type_en <-  c(
    "Natural",
    "Natural",
    "Artificial",
    "Artificial"
  )
  meta$dataDescription$categories$source <-  rep(meta$rawData, length(categories))

  # --- For proper referencing in markdown syntax
  meta$dataDescription$categories$mdref <- modif_md(meta$dataDescription$categories$accronyme)

  meta$dataDescription$categories$description <-  c(
    "Berge naturelle présentant des signes d'érosion apparente ou un couvert végétal de 25 à 75%",
    "Berge naturelle présentant des signes d'érosion apparente ou un couvert végétal < 25%",
    "Berge artificielle présentant des signes d'érosion apparente ou un couvert végétal de 25 à 75%",
    "Berge artificielle présentant des signes d'érosion apparente ou un couvert végétal < 25%"
  )

  meta$dataDescription$categories$description <-  c(
    "Natural bank with apparent signs of erosion or vegetation cover of 25 to 75%",
    "Natural bank with apparent signs of erosion or vegetation cover of < 25%",
    "Artificial bank with apparent signs of erosion or vegetation cover of 25 to 75%",
    "Artificial bank with apparent signs of erosion or vegetation cover of < 25%"
  )

  meta$dataDescription$categories$zonesNA <- c("berge_fluvial", "berge_fluvial", "berge_fluvial", "berge_fluvial")


  # ---
  obs <- data0017 %>%
         mutate(length = as.numeric(st_length(.)) / 1000) %>%
         st_drop_geometry() %>%
         group_by(categories) %>%
         summarise(length = sum(length)) %>%
         left_join(as.data.frame(categories), ., by = 'categories')

  meta$dataDescription$categories$longueur <- obs$length
  # _____________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # -----
  write_yaml(meta, "./data/data-metadata/int_cv_berge.yml")

  # -----
  st_write(obj = berge,
           dsn = "./data/data-integrated/cv_berge.geojson",
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
