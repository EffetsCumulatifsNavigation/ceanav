#' Habitats: Integrated habitats dataset
#'
#' Intégration des données utilisées pour caractériser la composante valorisée d'habitats
#'
#' @keywords zostère
#' @keywords milieu humide
#' @keywords habitat
#' @keywords composante valorisée
#'
#' @export
#'
#' @details Cette fonction télécharge et formatte les données
#'

cv_habitats <- function() {
  # Load grid
  data(aoi_grid1000poly)

  # Identify grid cells with zostera
  zostereID <- st_intersects(zostere_inv, aoi) %>%
               unlist() %>%
               unique()

  # Add info to grid
  zostere <- aoi %>% mutate(zostere = 0)
  zostere$zostere[zostereID] <- 1

  # # Combine datasets
  # milieu_humide <- bind_rows(milieu_humide_lsp, milieu_humide_stl)
  #
  # # Load grid
  # data(aoi_grid1000poly)
  #
  # # Identify grid cells with zostera
  # uid <- st_intersects(milieu_humide, aoi) %>%
  #              unlist() %>%
  #              unique()
  #
  # # Add info to grid
  # milieu_humide <- aoi %>% mutate(milieu_humide = 0)
  # milieu_humide$milieu_humide[uid] <- 1

  # Format milieu humide db
  fmtMarais <- function () {

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Load data
    # ------------------------------------
    #
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    folder <- './analysis/data/cv/habitats/marais/'

    # dataID: 0005
    # Marais Saint-Laurent
    marais_stl <- st_read(paste0(folder, 'marais_saint-laurent/MaraisCotiers_Publ_janv2019/MaraisCotiers_Publ_janv2019.shp')) %>%
                         st_transform(32198)
    # ------------------------------------------------------------------------- #


    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Format data
    # ------------------------------------
    #
    # All we will do for now for this dataset is include it as presence-absence
    # in the study grid
    #
    # So I intersect the milieu humide db with the grid to identify which grid
    # cell intersect with the db
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Load grid
    data(aoi_grid1000poly)

    # Identify grid cells with zostera
    uid <- st_intersects(marais_stl, aoi) %>%
                 unlist() %>%
                 unique()

    # Add info to grid
    marais <- aoi %>% mutate(marais = 0)
    marais$marais[uid] <- 1
    # ------------------------------------------------------------------------- #


    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Export data
    # ------------------------------------
    #
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    save(marais, file = './data/cv_hab_marais.RData')
    # ------------------------------------------------------------------------- #

  }


  # Format milieu humide db
  fmtMilieu_sableux <- function () {

    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Load data
    # ------------------------------------
    #
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    folder <- './analysis/data/cv/habitats/milieu_sableux/'

    # dataID: 0005
    # Marais Saint-Laurent
    milieu_sableux_stl <- st_read(paste0(folder, 'milieu_sableux_saint-laurent/MilieuxSableux_Publ_janv2019/MilieuxSableux_Publ_janv2019.shp')) %>%
                          st_transform(32198)
    # ------------------------------------------------------------------------- #


    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Format data
    # ------------------------------------
    #
    # All we will do for now for this dataset is include it as presence-absence
    # in the study grid
    #
    # So I intersect the milieu humide db with the grid to identify which grid
    # cell intersect with the db
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Load grid
    data(aoi_grid1000poly)

    # Identify grid cells with zostera
    uid <- st_intersects(milieu_sableux_stl, aoi) %>%
                 unlist() %>%
                 unique()

    # Add info to grid
    milieu_sableux <- aoi %>% mutate(milieu_sableux = 0)
    milieu_sableux$milieu_sableux[uid] <- 1
    # ------------------------------------------------------------------------- #


    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    # Export data
    # ------------------------------------
    #
    # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
    save(milieu_sableux, file = './data/cv_hab_milieu_sableux.RData')
    # ------------------------------------------------------------------------- #

  }

  # Load grid
  data(aoi_grid1000poly)

  # Use grid as dataset
  cote <- aoi

  # For each coastal habitat type
  hab <- unique(cote_classification$SCAT_Class_EN)
  for(i in hab) {
    # Segments for habitat i
    habid <- cote_classification$SCAT_Class_EN == i

    # Identify grid cells with coast habitat types
    uid <- st_intersects(cote_classification[habid, ], aoi) %>%
                 unlist() %>%
                 unique()

    # Add info to grid
    dat <- numeric(nrow(cote))
    dat[uid] <- 1
    cote <- cbind(cote, dat) %>%
            rename(!!i:=dat)
  }

}
