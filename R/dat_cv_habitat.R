
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
