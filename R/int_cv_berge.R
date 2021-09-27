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
  # Dans le rapport, l'état de la berge est divisé en 8 catégories:
  #   - Naturelle - Semi-végétalisée (N-SV)
  #   - Naturelle - Vive (N-V)
  #   - Artificielle - Semi-végétalisée - Partiellement endommagée (A-SV-PE)
  #   - Artificielle - Semi-végétalisée - Très endommagée (A-SV-TE)
  #   - Artificielle - Semi-végétalisée - Complètement endommagée (A-SV-CE)
  #   - Artificielle - Vive - Partiellement endommagée (A-V-PE)
  #   - Artificielle - Vive - Très endommagée (A-V-TE)
  #   - Artificielle - Vive - Complètement endommagée (A-V-CE)
  #
  # WARNING: there is still work to do here, at this phase this is exploratory
  # TODO: see if if makes sense to divide by erosion processes identified in dataset
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  load_format("data0017")
  data(grid1p)

  # ----------
  categories <- c(
    "N_SV", # Naturelle - Semi-végétalisée (N_SV)
    "N_V", # Naturelle - Vive (N_V)
    "A_SV_PE", # Artificielle - Semi-végétalisée - Partiellement endommagée (A_SV_PE)
    "A_SV_TE", # Artificielle - Semi-végétalisée - Très endommagée (A_SV_TE)
    "A_SV_CE", # Artificielle - Semi-végétalisée - Complètement endommagée (A_SV-_E)
    "A_V_PE", # Artificielle - Vive - Partiellement endommagée (A_V_PE)
    "A_V_TE", # Artificielle - Vive - Très endommagée (A_V_TE)
    "A_V_CE" # Artificielle - Vive - Complètement endommagée (A_V_CE)
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
         data0017$Etat_Berge == "SV" &
         data0017$Etat_Artif == "PE"
  data0017$categories[uid] <- categories[3]

  # ----------
  uid <- data0017$Artificiel == "O" &
         data0017$Etat_Berge == "SV" &
         data0017$Etat_Artif == "TE"
  data0017$categories[uid] <- categories[4]

  # ----------
  uid <- data0017$Artificiel == "O" &
         data0017$Etat_Berge == "SV" &
         data0017$Etat_Artif == "CE"
  data0017$categories[uid] <- categories[5]

  # ----------
  uid <- data0017$Artificiel == "O" &
         data0017$Etat_Berge == "VIVE" &
         data0017$Etat_Artif == "PE"
  data0017$categories[uid] <- categories[6]

  # ----------
  uid <- data0017$Artificiel == "O" &
         data0017$Etat_Berge == "VIVE" &
         data0017$Etat_Artif == "TE"
  data0017$categories[uid] <- categories[7]

  # ----------
  uid <- data0017$Artificiel == "O" &
         data0017$Etat_Berge == "VIVE" &
         data0017$Etat_Artif == "CE"
  data0017$categories[uid] <- categories[8]

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
  meta$dataDescription$spatial$extent <- st_bbox(data0017) # extent of 0021 includes 0028

  # -----
  meta$dataDescription$categories$accronyme <-  categories
  meta$dataDescription$categories$francais <-  c(
    "Naturelle - Semi-végétalisée",
    "Naturelle - Vive",
    "Artificielle - Semi-végétalisée - Partiellement endommagée",
    "Artificielle - Semi-végétalisée - Très endommagée",
    "Artificielle - Semi-végétalisée - Complètement endommagée",
    "Artificielle - Vive - Partiellement endommagée",
    "Artificielle - Vive - Très endommagée",
    "Artificielle - Vive - Complètement endommagée"
  )
  meta$dataDescription$categories$source <-  rep(meta$rawData, length(categories))


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
