explo <- function(dat) {
  library(leaflet)
  leaflet(dat) %>%
       setView(lng = -63, lat = 48, zoom =4) %>%
       addTiles(group = 'Default') %>%
       addPolygons(color = '#16090b')
}

habitat <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Libraries
  # ---------
  #
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  library(sf)
  library(tidyverse)
  library(magrittr)
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Zostère
  # ---------
  #
  # Inventaire de la zostère marine dans la Baie James, la Baie des Chaleurs,
  # l'estuaire et le golfe du Saint-Laurent
  #
  # https://ouvert.canada.ca/data/fr/dataset/c9ab948f-5009-4dbc-9129-2f6e373f17f6
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # URL
  zostere <- 'https://pacgis01.dfo-mpo.gc.ca/FGPPublic/Zostera_Zostere/Zostera_Zostere.zip'

  # Download
  output <- './analysis/data/cv/habitats/zostere/'
  dir.create(output)
  download.file(zostere, destfile = paste0(output, 'Zostere.zip'))

  # Unzip
  unzip(zipfile = paste0(output, 'Zostere.zip'), exdir = output)

  # Import
  zostere <- st_read(paste0(output, 'Zostera_Zostere.shp')) %>%
             st_transform(32198)
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Zostère Pointe au père
  # ----------------------
  #
  #
  #
  # https://catalogue.ogsl.ca/fr/dataset/3d0c057b-ab39-4a7b-8809-4367d5028c11
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # URL
  zostere <- c('https://catalogue.ogsl.ca/data/zip-sud/3d0c057b-ab39-4a7b-8809-4367d5028c11/herbier-zostere_continu_2017-18.zip',
               'https://catalogue.ogsl.ca/data/zip-sud/3d0c057b-ab39-4a7b-8809-4367d5028c11/zones_plantation_zostere_2017-18.zip',
               'https://catalogue.ogsl.ca/data/zip-sud/3d0c057b-ab39-4a7b-8809-4367d5028c11/parcelles_suivi_2017-18.zip',
               'https://catalogue.ogsl.ca/data/zip-sud/3d0c057b-ab39-4a7b-8809-4367d5028c11/depliant_zip_zostere.pdf',
               'https://catalogue.ogsl.ca/data/zip-sud/3d0c057b-ab39-4a7b-8809-4367d5028c11/rapport_suivis_pointe-au-pere_2017-18.pdf',
               'https://catalogue.ogsl.ca/data/zip-sud/3d0c057b-ab39-4a7b-8809-4367d5028c11/descriptionchamps_pointe-au-pere.xlsx')

  # Download
  output <- './analysis/data/cv/habitats/zostere_pap/'
  dir.create(output)
  download.file(zostere[1], destfile = paste0(output, 'herbier-zostere_continu_2017-18.zip'))
  download.file(zostere[2], destfile = paste0(output, 'zones_plantation_zostere_2017-2018.zip'))
  download.file(zostere[3], destfile = paste0(output, 'parcelles_suivi_2017-18.zip'))
  download.file(zostere[4], destfile = paste0(output, 'depliant_zip_zostere.pdf'))
  download.file(zostere[5], destfile = paste0(output, 'rapport_suivis_pointe-au-pere_2017-18.pdf'))
  download.file(zostere[6], destfile = paste0(output, 'descriptionchamps_pointe-au-pere.xlsx'))

  # Unzip
  unzip(zipfile = paste0(output, 'herbier-zostere_continu_2017-18.zip'), exdir = output)
  unzip(zipfile = paste0(output, 'zones_plantation_zostere_2017-2018.zip'), exdir = output)
  unzip(zipfile = paste0(output, 'parcelles_suivi_2017-18.zip'), exdir = output)

  # Import
  zostere <- st_read(paste0(output, 'herbier-zostere_continu.shp')) %>%
             st_transform(32198)

  zostere2 <- st_read(paste0(output, 'zones_plantation_zostere.shp')) %>%
             st_transform(32198)
}
