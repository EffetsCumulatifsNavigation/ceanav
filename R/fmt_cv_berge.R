fmtBerge <- function () {

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  folder <- './analysis/data/cv/integrite_berge/berge_ul/'

  # dataID: 0014
  # Caractérisation berges fluviales
  ## Selected colnames
  nm <- c("Type_Berge","Etat_Berge","Artificiel","Type_Artif","Etat_Artif",
          "Process_dom_1", "Process_dom_2","Classe_site", "Shape")

  ## Lac St-Pierre
  berge_lsp <- st_read(paste0(folder, 'CaractBerges_TCRLSP_UL_Mars2020.gdb'),
                       layer = 'Sites_sensibles_TCRLSP_UL_Mars2020') %>%
               rename("Type_Berge" = "Type_berge",
                      "Etat_Berge" = "Etat_berge",
                      "Process_dom_1" = "process_dom_1",
                      "Process_dom_2" = "process_dom_2") %>%
               .[, nm] %>%
               st_transform(32198)

  ## Québec
  berge_qc <- st_read(paste0(folder, 'CaractBerges_TCRQC_UL_Mars2020.gdb'),
                      layer = 'Sites_sensibles_TCRQC_UL_Mars2020') %>%
              rename("Type_Berge" = "Type_Cote",
                     "Etat_Berge" = "Etat_Cote",
                     "Type_Artif" = "Type_artif",
                     "Shape" = "SHAPE") %>%
              .[, nm] %>%
              st_transform(32198)

  ## Haut-Saint-Laurent - Grand-Montréal
  berge_hslgm <- st_read(paste0(folder, 'CaractBerges_TCRHSLGM_UL_Mars2020.gdb'),
                         layer = 'Sites_sensibles_TCRHSLGM_UL_Mars2020') %>%
                 rename("Type_Berge" = "Type_berge",
                        "Etat_Berge" = "Etat_berge") %>%
                 .[, nm] %>%
                 st_transform(32198) %>%
                 st_cast("MULTILINESTRING")

  ## Estuaire fluvial
  berge_ef <- st_read(paste0(folder, 'CaractBerges_TCREF_UL_Mars2020.gdb'),
                      layer = 'Sites_sensibles_TCREF_UL_Mars2020') %>%
              rename("Type_Berge" = "Type_Cote",
                     "Etat_Berge" = "Etat_Cote",
                     "Shape" = "SHAPE") %>%
              .[, nm] %>%
              st_transform(32198)

  ## Single dataset
  berge <- bind_rows(berge_lsp, berge_qc, berge_hslgm, berge_ef)
  # ------------------------------------------------------------------------- #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Include to study grid
  # ------------------------------------
  #
  # All we will do for now for this dataset is include it as presence-absence
  # in the study grid
  #
  # So I intersect the zostere db with the grid to identify which grid cell
  # intersect with the db
  #
  # WARNING: there is still work to do here, at this phase this is exploratory
  # TODO: see if if makes sense to divide by erosion processes identified in dataset
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load grid
  data(aoi_grid1000poly)

  # Identify grid cells with zostera
  bergeID <- st_intersects(berge, aoi) %>%
             unlist() %>%
             unique()

  # Add info to grid
  berge <- aoi %>% mutate(berge = 0)
  berge$berge[bergeID] <- 1
  # ------------------------------------------------------------------------- #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  save(berge, file = './data/cv_berge.RData')
  # ------------------------------------------------------------------------- #
}
