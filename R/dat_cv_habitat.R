# These functions download selected datasets from the internet
# Global function to download all habitat datasets
getHabitat <- function() {
  getZostere()
  getMilieu_humide()
  getMarais()
  getMilieu_sableux()
  getAlevinage()
  getFrayere()
  getEspece_statut()
}

# Function to download Zostere datasets
getZostere <- function() {
  output <- './analysis/data/cv/habitats/zostere/'
  if (!file.exists(output)) dir.create(output)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Inventaire zostère
  # ------------------------------------
  # dataID: 0001
  # ~~~~~~~~~~~~
  #
  # Inventaire de la zostère marine dans la Baie James, la Baie des Chaleurs,
  # l'estuaire et le golfe du Saint-Laurent
  #
  # https://ouvert.canada.ca/data/fr/dataset/c9ab948f-5009-4dbc-9129-2f6e373f17f6
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Output folder
  folder <- paste0(output, 'zostere_inventaire/')
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'Zostera_Zostere.zip'))) {
    # URL
    zostere_inv <- c('https://pacgis01.dfo-mpo.gc.ca/FGPPublic/Zostera_Zostere/Zostera_Zostere.zip',
                     'https://pacgis01.dfo-mpo.gc.ca/FGPPublic/Zostera_Zostere/DataDictionary_DictionnaireDonnees.csv')

    # Download
    download.file(zostere_inv[1], destfile = paste0(folder, 'Zostera_Zostere.zip'))
    download.file(zostere_inv[2], destfile = paste0(folder, 'DataDictionary_DictionnaireDonnees.csv'))

    # Unzip
    unzip(zipfile = paste0(folder, 'Zostera_Zostere.zip'), exdir = folder)
  }
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Zostère Pointe au père
  # ----------------------
  # dataID: 0002
  # ~~~~~~~~~~~~
  #
  # https://catalogue.ogsl.ca/fr/dataset/3d0c057b-ab39-4a7b-8809-4367d5028c11
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Output folder
  folder <- paste0(output, 'zostere_pointe_au_pere/')
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'herbier-zostere_continu_2017-18.zip'))) {
    # URL
    zostere_pap <- c('https://catalogue.ogsl.ca/data/zip-sud/3d0c057b-ab39-4a7b-8809-4367d5028c11/herbier-zostere_continu_2017-18.zip',
                     'https://catalogue.ogsl.ca/data/zip-sud/3d0c057b-ab39-4a7b-8809-4367d5028c11/zones_plantation_zostere_2017-18.zip',
                     'https://catalogue.ogsl.ca/data/zip-sud/3d0c057b-ab39-4a7b-8809-4367d5028c11/parcelles_suivi_2017-18.zip',
                     'https://catalogue.ogsl.ca/data/zip-sud/3d0c057b-ab39-4a7b-8809-4367d5028c11/depliant_zip_zostere.pdf',
                     'https://catalogue.ogsl.ca/data/zip-sud/3d0c057b-ab39-4a7b-8809-4367d5028c11/rapport_suivis_pointe-au-pere_2017-18.pdf',
                     'https://catalogue.ogsl.ca/data/zip-sud/3d0c057b-ab39-4a7b-8809-4367d5028c11/descriptionchamps_pointe-au-pere.xlsx')

    # Download
    download.file(zostere_pap[1], destfile = paste0(folder, 'herbier-zostere_continu_2017-18.zip'))
    download.file(zostere_pap[2], destfile = paste0(folder, 'zones_plantation_zostere_2017-2018.zip'))
    download.file(zostere_pap[3], destfile = paste0(folder, 'parcelles_suivi_2017-18.zip'))
    download.file(zostere_pap[4], destfile = paste0(folder, 'depliant_zip_zostere.pdf'))
    download.file(zostere_pap[5], destfile = paste0(folder, 'rapport_suivis_pointe-au-pere_2017-18.pdf'))
    download.file(zostere_pap[6], destfile = paste0(folder, 'descriptionchamps_pointe-au-pere.xlsx'))

    # Unzip
    unzip(zipfile = paste0(folder, 'herbier-zostere_continu_2017-18.zip'), exdir = folder)
    unzip(zipfile = paste0(folder, 'zones_plantation_zostere_2017-2018.zip'), exdir = folder)
    unzip(zipfile = paste0(folder, 'parcelles_suivi_2017-18.zip'), exdir = folder)
  }

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Zostère Mitis
  # ----------------------
  # dataID: 0003
  # ~~~~~~~~~~~~
  #
  # https://catalogue.ogsl.ca/fr/dataset/93fd20e8-c80b-4304-9eb4-80ac13a2d365
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Output folder
  folder <- paste0(output, 'zostere_mitis/')
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'herbier-zostere_continu_2017-18.zip'))) {
    # URL
    zostere_mitis <- c('https://catalogue.ogsl.ca/data/zip-sud/93fd20e8-c80b-4304-9eb4-80ac13a2d365/analyse_eau.zip',
                       'https://catalogue.ogsl.ca/data/zip-sud/93fd20e8-c80b-4304-9eb4-80ac13a2d365/zone_de_plantation.zip',
                       'https://catalogue.ogsl.ca/data/zip-sud/93fd20e8-c80b-4304-9eb4-80ac13a2d365/projet-zostere-profil-plage.xlsx',
                       'https://catalogue.ogsl.ca/data/zip-sud/93fd20e8-c80b-4304-9eb4-80ac13a2d365/description_champs_profil_plage.xlsx',
                       'https://zipsud.org/wp-content/uploads/2014/07/Caracterisation-Mitis-Zostere-28-05-14.pdf',
                       'https://zipsud.org/wp-content/uploads/2016/11/Rapport-de-restauration-Zosterere-ZIPSE-05-10-15.pdf',
                       'https://catalogue.ogsl.ca/data/zip-sud/93fd20e8-c80b-4304-9eb4-80ac13a2d365/cote_mitis2012.zip')

    # Download
    download.file(zostere_mitis[1], destfile = paste0(folder, 'analyse_eau.zip'))
    download.file(zostere_mitis[2], destfile = paste0(folder, 'zone_de_plantation.zip'))
    download.file(zostere_mitis[3], destfile = paste0(folder, 'projet-zostere-profil-plage.xlsx'))
    download.file(zostere_mitis[4], destfile = paste0(folder, 'description_champs_profil_plage.xlsx'))
    download.file(zostere_mitis[5], destfile = paste0(folder, 'Caracterisation-Mitis-Zostere-28-05-14.pdf'))
    download.file(zostere_mitis[6], destfile = paste0(folder, 'Rapport-de-restauration-Zosterere-ZIPSE-05-10-15.pdf'))
    download.file(zostere_mitis[7], destfile = paste0(folder, 'cote_mitis2012.zip'))

    # Unzip
    unzip(zipfile = paste0(folder, 'analyse_eau.zip'), exdir = folder)
    unzip(zipfile = paste0(folder, 'zone_de_plantation.zip'), exdir = folder)
    unzip(zipfile = paste0(folder, 'cote_mitis2012.zip'), exdir = folder)
  }
}



# Function to download milieux humides datasets
getMilieu_humide <- function() {
  output <- './analysis/data/cv/habitats/milieu_humide/'
  if (!file.exists(output)) dir.create(output)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Milieux humides lac Saint-Pierre
  # ------------------------------------
  # dataID: 0004
  # ~~~~~~~~~~~~
  #
  # Données de terrain pour la cartographie des milieux humides du lac
  # Saint-Pierre (fleuve Saint-Laurent), 2012
  #
  # https://ouvert.canada.ca/data/fr/dataset/9c52df44-7a34-4a73-a92a-5e3b20de6c73
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Output folder
  folder <- paste0(output, 'milieu_humide_lac_saint-pierre/')
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'MilieuxHumides-lacSaintPierre-inventaire-2012.csv'))) {
    # URL
    milieu_humide_lsp <- c('http://donnees.ec.gc.ca/data/sites/scientificknowledge/field-data-for-the-mapping-of-the-lake-st.-pierre-wetlands-st.-lawrence-river-2012/MilieuxHumides-lacSaintPierre-inventaire-2012.csv',
                           'http://donnees.ec.gc.ca/data/sites/scientificknowledge/field-data-for-the-mapping-of-the-lake-st.-pierre-wetlands-st.-lawrence-river-2012/MilieuxHumides-lacSaintPierre-sites-2012.csv')

    # Download
    download.file(milieu_humide_lsp[1], destfile = paste0(folder, 'MilieuxHumides-lacSaintPierre-inventaire-2012.csv'))
    download.file(milieu_humide_lsp[2], destfile = paste0(folder, 'MilieuxHumides-lacSaintPierre-sites-2012.csv'))
  }

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Milieux côtiers
  # ------------------------------------
  # dataID: 0005
  # ~~~~~~~~~~~~
  #
  # Atlas des milieux côtiers d’intérêt pour la conservation dans l’estuaire et
  # le golfe du Saint-Laurent
  #
  # https://catalogue.ogsl.ca/fr/dataset/0a232214-05cc-438a-b914-6a8b53ac184e
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Output folder
  folder <- paste0(output, 'milieu_humide_saint-laurent/')
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'atlas_estuairegolfe_rapport_final_fr.pdf'))) {
    # URL
    milieu_humide_stl <- c('https://catalogue.ogsl.ca/data/pasl/0a232214-05cc-438a-b914-6a8b53ac184e/atlas_estuairegolfe_rapport_final_fr.pdf',
                           'https://catalogue.ogsl.ca/data/pasl/0a232214-05cc-438a-b914-6a8b53ac184e/milieuxhum_publ_janv2019.zip')

    # Download
    download.file(milieu_humide_stl[1], destfile = paste0(folder, 'atlas_estuairegolfe_rapport_final_fr.pdf'))
    download.file(milieu_humide_stl[2], destfile = paste0(folder, 'milieuxhum_publ_janv2019.zip'))

    # Unzip
    unzip(zipfile = paste0(folder, 'milieuxhum_publ_janv2019.zip'), exdir = folder)
  }
}



# Function to download milieux humides datasets
getMarais <- function() {
  output <- './analysis/data/cv/habitats/marais/'
  if (!file.exists(output)) dir.create(output)


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Milieux côtiers
  # ------------------------------------
  # dataID: 0005
  # ~~~~~~~~~~~~
  #
  # Atlas des milieux côtiers d’intérêt pour la conservation dans l’estuaire et
  # le golfe du Saint-Laurent
  #
  # https://catalogue.ogsl.ca/fr/dataset/0a232214-05cc-438a-b914-6a8b53ac184e
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Output folder
  folder <- paste0(output, 'marais_saint-laurent/')
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'atlas_estuairegolfe_rapport_final_fr.pdf'))) {
    # URL
    marais_stl <- c('https://catalogue.ogsl.ca/data/pasl/0a232214-05cc-438a-b914-6a8b53ac184e/atlas_estuairegolfe_rapport_final_fr.pdf',
                    'https://catalogue.ogsl.ca/data/pasl/0a232214-05cc-438a-b914-6a8b53ac184e/maraiscotiers_publ_janv2019.zip')

    # Download
    download.file(marais_stl[1], destfile = paste0(folder, 'atlas_estuairegolfe_rapport_final_fr.pdf'))
    download.file(marais_stl[2], destfile = paste0(folder, 'maraiscotiers_publ_janv2019.zip'))

    # Unzip
    unzip(zipfile = paste0(folder, 'maraiscotiers_publ_janv2019.zip'), exdir = folder)
  }
}


# Function to download milieux humides datasets
getMilieu_sableux <- function() {
  output <- './analysis/data/cv/habitats/milieu_sableux/'
  if (!file.exists(output)) dir.create(output)


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Milieux côtiers
  # ------------------------------------
  # dataID: 0005
  # ~~~~~~~~~~~~
  #
  # Atlas des milieux côtiers d’intérêt pour la conservation dans l’estuaire et
  # le golfe du Saint-Laurent
  #
  # https://catalogue.ogsl.ca/fr/dataset/0a232214-05cc-438a-b914-6a8b53ac184e
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Output folder
  folder <- paste0(output, 'milieu_sableux_saint-laurent/')
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'atlas_estuairegolfe_rapport_final_fr.pdf'))) {
    # URL
    milieu_sableux_stl <- c('https://catalogue.ogsl.ca/data/pasl/0a232214-05cc-438a-b914-6a8b53ac184e/atlas_estuairegolfe_rapport_final_fr.pdf',
                            'https://catalogue.ogsl.ca/data/pasl/0a232214-05cc-438a-b914-6a8b53ac184e/milieuxsableux_publ_janv2019.zip')

    # Download
    download.file(milieu_sableux_stl[1], destfile = paste0(folder, 'atlas_estuairegolfe_rapport_final_fr.pdf'))
    download.file(milieu_sableux_stl[2], destfile = paste0(folder, 'milieuxsableux_publ_janv2019.zip'))

    # Unzip
    unzip(zipfile = paste0(folder, 'milieuxsableux_publ_janv2019.zip'), exdir = folder)
  }
}


# Function to download milieux humides datasets
getAlevinage <- function() {
  output <- './analysis/data/cv/habitats/alevinage/'

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Sites d'alevinage
  # ------------------------------------
  # dataID: 0006
  # ~~~~~~~~~~~~
  #
  # WARNING: Données provenant du MFFP, non disponible en ligne
  # WARNING: Données ne peuvent être partagées
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Output folder
  folder <- paste0(output, 'alevinage_mffp/')

  # Unzip file
  unzip(zipfile = paste0(folder, 'DonneesMFFP_PourPASL.gdb.zip'), exdir = folder)
}


# Function to download milieux humides datasets
getFrayere <- function() {
  output <- './analysis/data/cv/habitats/frayere/'

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Sites d'alevinage
  # ------------------------------------
  # dataID: 0006
  # ~~~~~~~~~~~~
  #
  # WARNING: Données provenant du MFFP, non disponible en ligne
  # WARNING: Données ne peuvent être partagées
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Output folder
  folder <- paste0(output, 'frayere_mffp/')

  # Unzip file
  unzip(zipfile = paste0(folder, 'DonneesMFFP_PourPASL.gdb.zip'), exdir = folder)
}


# Function to download milieux humides datasets
getEspece_statut <- function() {
  output <- './analysis/data/cv/habitats/espece_statut/'

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Sites d'alevinage
  # ------------------------------------
  # dataID: 0006
  # ~~~~~~~~~~~~
  #
  # WARNING: Données provenant du MFFP, non disponible en ligne
  # WARNING: Données ne peuvent être partagées
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Output folder
  folder <- paste0(output, 'espece_statut_mffp/')

  # Unzip file
  unzip(zipfile = paste0(folder, 'DonneesMFFP_PourPASL.gdb.zip'), exdir = folder)
}


# Function to download milieux humides datasets
getShoreline <- function() {
  output <- './analysis/data/cv/habitats/cote/'
  if (!file.exists(output)) dir.create(output)


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Milieux côtiers
  # ------------------------------------
  # dataID: 0007
  # ~~~~~~~~~~~~
  #
  # Classification des Rivages du Québec - Fleuve Saint-Laurent
  #
  # https://ouvert.canada.ca/data/fr/dataset/ba580518-59e8-4d1c-b3ef-41d2658e6965
  #
  # Sergy, G. (2008). The Shoreline Classification Scheme for SCAT and Oil
  #   Spill Response in Canada. Proceedings of the 31stArctic and Marine
  #   Oil Spill Program Technical Seminar.Environment Canada, Ottawa, ON,
  #   Pp. 811-819.
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Output folder
  folder <- paste0(output, 'cote_classification/')
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'How_To_Guide_-_Navigating_Through_Open_Data_Portals_-_for_ECCC_Shoreline_Data.pdf'))) {
    # URL
    cote_classification <- c('http://data.ec.gc.ca/data/sites/emergencies/shoreline-segmentation-with-shoreline-cleanup-assessment-technique-scat-classification/quebec-saint-lawrence-river-shoreline-classification/How_To_Guide_-_Navigating_Through_Open_Data_Portals_-_for_ECCC_Shoreline_Data.pdf',
                             'http://data.ec.gc.ca/data/sites/emergencies/shoreline-segmentation-with-shoreline-cleanup-assessment-technique-scat-classification/quebec-saint-lawrence-river-shoreline-classification/ShorelineClassification_QC_OpenDataCatalogue.gdb.zip')

    # Download
    download.file(cote_classification[1], destfile = paste0(folder, 'How_To_Guide_-_Navigating_Through_Open_Data_Portals_-_for_ECCC_Shoreline_Data.pdf'))
    download.file(cote_classification[2], destfile = paste0(folder, 'ShorelineClassification_QC_OpenDataCatalogue.gdb.zip'))

    # Unzip
    unzip(zipfile = paste0(folder, 'ShorelineClassification_QC_OpenDataCatalogue.gdb.zip'), exdir = folder)
  }
}
