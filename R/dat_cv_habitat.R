

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

getZone_inondable <- function() {
  output <- './analysis/data/cv/habitats/zone_inondable/'
  if (!file.exists(output)) dir.create(output)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Zones inondables MRC
  # ------------------------------------
  # dataID: 0008
  # ~~~~~~~~~~~~
  #
  # Grille de présence de zone inondable identifiée par les MRC
  #
  # WARNING:
  # > Cette grille est une représentation spatiale des secteurs où une
  #   cartographie a été produite par les municipalités régionales de comté
  #   (MRC) ou les villes à compétence de MRC. Elle indique qu’une cartographie
  #   des zones inondables a été intégrée dans le schéma d’aménagement et de
  #   développement (SAD) ou dans un règlement de contrôle intérimaire (RCI)
  #   en vigueur dans la MRC.
  #
  # > Cette information est fournie à titre indicatif et n’a aucune valeur
  #   légale. L'utilisateur est invité à communiquer avec les municipalités ou
  #   les MRC afin de connaitre les limites exactes des zones inondables
  #   cartographiées et la réglementation en vigueur applicable sur leur
  #   territoire.
  #
  # https://www.donneesquebec.ca/recherche/fr/dataset/0d9de0d6-9873-4a8c-adc7-0e94d51b3fa0
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Output folder
  folder <- paste0(output, 'zone_inondable_mrc/')
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'grillepresencezoneinondable.geojson'))) {
    # URL
    zone_inondable_mrc <- c('https://donneesouvertes.affmunqc.net/zones_inondables/grillepresencezoneinondable.json')

    # Download
    download.file(zone_inondable_mrc[1], destfile = paste0(folder, 'grillepresencezoneinondable.geojson'))
  }


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Zones inondables BDZI
  # ------------------------------------
  # dataID: 0009
  # ~~~~~~~~~~~~
  #
  # Base de données des zones à risque d'inondation (BDZI)
  #
  # https://www.donneesquebec.ca/recherche/fr/dataset/3ac8ddff-fe0a-4a7a-8393-d5938e8f35e5
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Output folder
  folder <- paste0(output, 'zone_inondable_bdzi/')
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'Struc_physique_20BDZI_V3.0.pdf'))) {
    # URL
    zone_inondable_bdzi <- c('ftp://ftp.mddelcc.gouv.qc.ca/DONNEES_OUVERTES/Base_donnees_zones_inondables/Struc_physique_%20BDZI_V3.0.pdf',
                             'ftp://ftp.mddelcc.gouv.qc.ca/DONNEES_OUVERTES/Base_donnees_zones_inondables/BDZI.gdb.zip')

    # Download
    # Default R options limit download time to 60 seconds. Modify for larger files
    oldopt <- options()$timeout
    options(timeout=500)
    download.file(zone_inondable_bdzi[1], destfile = paste0(folder, 'Struc_physique_20BDZI_V3.0.pdf'))
    download.file(zone_inondable_bdzi[2], destfile = paste0(folder, 'BDZI.gdb.zip'))
    options(timeout=oldopt)

    # Unzip
    unzip(zipfile = paste0(folder, 'BDZI.gdb.zip'), exdir = folder)
  }
}

getBenthic <- function() {
  output <- './analysis/data/cv/habitats/benthique/'
  if (!file.exists(output)) dir.create(output)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Substrats benthiques
  # ------------------------------------
  # dataID: 0010
  # ~~~~~~~~~~~~
  #
  # Substrats benthiques du fjord du Saguenay, l'estuaire maritime et du
  # golfe du Saint-Laurent
  #
  # https://ouvert.canada.ca/data/fr/dataset/8c269a91-d3a2-4f49-943d-6b2401c42cba
  #
  # Loring, D. H., and D. J. G. Nota. 1973. Morphology and sediments of the Gulf
  # of St. Lawrence. Bull. Fish. Res. Bd. Can. 182. 147 p. + 7 charts.
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Output folder
  folder <- paste0(output, 'benthique_ln/')
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'DataDictionary_DictionnaireDonnees.csv'))) {
    # URL
    benthique_ln <- c('https://pacgis01.dfo-mpo.gc.ca/FGPPublic/Seafloor_SubstratBenthique/DataDictionary_DictionnaireDonnees.csv',
                      'https://pacgis01.dfo-mpo.gc.ca/FGPPublic/Seafloor_SubstratBenthique/Seafloor_SubstratBenthique.zip')

    # Download
    download.file(benthique_ln[1], destfile = paste0(folder, 'DataDictionary_DictionnaireDonnees.csv'))
    download.file(benthique_ln[2], destfile = paste0(folder, 'Seafloor_SubstratBenthique.zip'))

    # Unzip
    unzip(zipfile = paste0(folder, 'Seafloor_SubstratBenthique.zip'), exdir = folder)
  }


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Megahabitats
  # ------------------------------------
  # dataID: 0011
  # ~~~~~~~~~~~~
  #
  #
  #
  # https://open.canada.ca/data/en/dataset/3e89da6c-fd76-4e53-af62-dfec141ffda5
  #
  # Dutil, J.-D., Proulx, S., Chouinard, P.-M., and Borcard. D. 2011. A
  # hierarchical classification of the seabed based on physiographic and
  # oceanographic features in the St. Lawrence. Can. Tech. Rep. Fish. Aquat.
  # Sci. 2916: vii + 72 p.
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Output folder
  folder <- paste0(output, 'benthique_mega/')
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'DataDictionary_DictionnaireDonnees.csv'))) {
    # URL
    benthique_mega <- c('https://pacgis01.dfo-mpo.gc.ca/FGPPublic/Megahabitats_DB/DataDictionary_DictionnaireDonnees.csv',
                        'https://pacgis01.dfo-mpo.gc.ca/FGPPublic/Megahabitats_DB/Megahabitats_DB.zip')

    # Download
    download.file(benthique_mega[1], destfile = paste0(folder, 'DataDictionary_DictionnaireDonnees.csv'))
    download.file(benthique_mega[2], destfile = paste0(folder, 'Megahabitats_DB.zip'))

    # Unzip
    unzip(zipfile = paste0(folder, 'Megahabitats_DB.zip'), exdir = folder)
  }
}
