# Function to download Zostere datasets
getBerge <- function() {
  output <- './analysis/data/cv/integrite_berge/'
  if (!file.exists(output)) dir.create(output)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Caractérisation berges fluviales
  # ------------------------------------
  # dataID:0014
  # ~~~~~~~~~~~~
  #
  # Caractérisation des berges de la partie fluviale du Saint-Laurent et analyse
  # de l'évolution des facteurs hydro-climatiques influençant les aléas
  # d'érosion et d'inondation
  #
  # https://www.donneesquebec.ca/recherche/dataset/caracterisation-des-berges-et-analyse-de-l-evolution-des-facteurs-hydro-climatiques
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Output folder
  folder <- paste0(output, 'berge_ul/')
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
}
