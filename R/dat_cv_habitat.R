# These functions download selected datasets from the internet
# Global function to download all habitat datasets
getHabitat <- function() {
  getZostere()
  getMilieu_humide()
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
}
