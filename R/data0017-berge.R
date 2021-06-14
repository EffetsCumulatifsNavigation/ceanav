#' Data 0017 : Caractérisation des berges
#'
#' Caractérisation des berges de la partie fluviale du Saint-Laurent et analyse de l'évolution des facteurs hydro-climatiques influençant les aléas d'érosion et d'inondation
#'
#' @keywords berge
#' @keywords habitat
#' @keywords composante valorisée
#'
#' @source https://www.donneesquebec.ca/recherche/dataset/caracterisation-des-berges-et-analyse-de-l-evolution-des-facteurs-hydro-climatiques
#'
#' @export
#'
#' @details Cette fonction télécharge et formatte les données
#'

get_data0017 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0017-berge/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'berge_lsp.zip'))) {
    # URL
    berge_ul <- c('ftp://ftp.mddelcc.gouv.qc.ca/DONNEES_OUVERTES/Caract_berges_saint_laurent/FGDB/CaractBerges_TCRLSP_UL_Mars2020.zip',
                  'ftp://ftp.mddelcc.gouv.qc.ca/DONNEES_OUVERTES/Caract_berges_saint_laurent/FGDB/CaractBerges_TCRQC_UL_Mars2020.zip',
                  'ftp://ftp.mddelcc.gouv.qc.ca/DONNEES_OUVERTES/Caract_berges_saint_laurent/FGDB/CaractBerges_TCRHSLGM_UL_Mars2020.zip',
                  'ftp://ftp.mddelcc.gouv.qc.ca/DONNEES_OUVERTES/Caract_berges_saint_laurent/FGDB/CaractBerges_TCREF_UL_Mars2020.zip',
                  'https://www.donneesquebec.ca/recherche/dataset/8301c7ed-6b27-4319-9150-435246634b90/resource/a5d514c3-9356-40f5-9362-35967808be9b/download/caracterisationbergesulmars2020.pdf')

    # Download
    download.file(berge_ul[1], destfile = paste0(folder, 'berge_lsp.zip'))
    download.file(berge_ul[2], destfile = paste0(folder, 'berge_qc.zip'))
    download.file(berge_ul[3], destfile = paste0(folder, 'berge_hslgm.zip'))
    download.file(berge_ul[4], destfile = paste0(folder, 'berge_ef.zip'))
    download.file(berge_ul[5], destfile = paste0(folder, 'caracterisationbergesulmars2020.pdf'))

    # Unzip
    unzip(zipfile = paste0(folder, 'berge_lsp.zip'), exdir = folder)
    unzip(zipfile = paste0(folder, 'berge_qc.zip'), exdir = folder)
    unzip(zipfile = paste0(folder, 'berge_hslgm.zip'), exdir = folder)
    unzip(zipfile = paste0(folder, 'berge_ef.zip'), exdir = folder)
  }

  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import and format data
  # ----------------------------------------
  nm <- c("Type_Berge","Etat_Berge","Artificiel","Type_Artif","Etat_Artif",
          "Process_dom_1", "Process_dom_2","Classe_site", "Shape")

  ## Lac St-Pierre
  berge_lsp <- st_read(paste0(folder, 'CaractBerges_TCRLSP_UL_Mars2020.gdb'),
                       layer = 'Sites_sensibles_TCRLSP_UL_Mars2020') %>%
               rename("Type_Berge" = "Type_berge",
                      "Etat_Berge" = "Etat_berge",
                      "Process_dom_1" = "process_dom_1",
                      "Process_dom_2" = "process_dom_2") %>%
               .[, nm]

  ## Québec
  berge_qc <- st_read(paste0(folder, 'CaractBerges_TCRQC_UL_Mars2020.gdb'),
                      layer = 'Sites_sensibles_TCRQC_UL_Mars2020') %>%
              rename("Type_Berge" = "Type_Cote",
                     "Etat_Berge" = "Etat_Cote",
                     "Type_Artif" = "Type_artif",
                     "Shape" = "SHAPE") %>%
              .[, nm]

  ## Haut-Saint-Laurent - Grand-Montréal
  berge_hslgm <- st_read(paste0(folder, 'CaractBerges_TCRHSLGM_UL_Mars2020.gdb'),
                         layer = 'Sites_sensibles_TCRHSLGM_UL_Mars2020') %>%
                 rename("Type_Berge" = "Type_berge",
                        "Etat_Berge" = "Etat_berge") %>%
                 .[, nm] %>%
                 st_cast("MULTILINESTRING")

  ## Estuaire fluvial
  berge_ef <- st_read(paste0(folder, 'CaractBerges_TCREF_UL_Mars2020.gdb'),
                      layer = 'Sites_sensibles_TCREF_UL_Mars2020') %>%
              rename("Type_Berge" = "Type_Cote",
                     "Etat_Berge" = "Etat_Cote",
                     "Shape" = "SHAPE") %>%
              .[, nm]

  ## Single dataset
  data0017 <- bind_rows(berge_lsp, berge_qc, berge_hslgm, berge_ef)
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0017,
           dsn = "./data/data-format/data0017-berge.geojson",
           delete_dsn = TRUE)

  # RData
  save(data0017, file = "./data/data0017.RData")
  # _________________________________________________________________________ #
}
