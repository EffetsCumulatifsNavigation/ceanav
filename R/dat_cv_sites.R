# These functions download selected datasets from various sources

# Global function to download all "sites importants" datasets
getSite <- function() {
  getPeche()
  getSaumon()
}



getPeche <- function() {
  output <- './analysis/data/cv/site_important/peche_commerciale/'
  if (!file.exists(output)) dir.create(output)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Commercial fisheries - Mi'gmaq and Maliseet
  # ------------------------------------
  # dataID: 0016
  # ~~~~~~~~~~~~
  #
  # Mi'gmaq and Maliseet commercial fisheries of the communities of
  # Gesgapegiag, Gespeg and Viger
  #
  # https://catalogue.ogsl.ca/en/dataset/bbe35312-987a-42d7-ae2f-41c1a0715dc3
  #
  # Racine, M.-J. and Arsenault, L.M. 2017. Commercial Fisheries Data for the
  # communities of Gesgapegiag, Gespeg and Viger. MMAFMA. Data published on St.
  # Lawrence Global Observatory-SLGO. [https://slgo.ca]. Access date:
  # [YYYY-MM-DD].
  #
  # Report:
  # Use report source identification + " Published on St. Lawrence Global
  # Observatory-SLGO. [https://slgo.ca]. Access date: [YYYY-MM-DD]."
  #
  # Atlas:
  # Arsenault, L.M. Racine, M.-J. and Lambert Koizumi, C. (2017) Atlas of Marine
  # St. Lawrence Mi’gmaq and Maliseet Sites and Their Uses by the Gesgapegiag,
  # Gespeg and Viger Communities. Mi’gmaq Maliseet Aboriginal Fisheries Management
  # Association (MMAFMA), 46 p.
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Output folder
  folder <- paste0(output, 'peche_commerciale_aghamm/')
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'atlas-halieutique_fr.pdf'))) {
    # URL
    peche_aghamm <- c('https://catalogue.ogsl.ca/data/aghamm/bbe35312-987a-42d7-ae2f-41c1a0715dc3/atlas-halieutique_fr.pdf',
                      'https://catalogue.ogsl.ca/data/aghamm/bbe35312-987a-42d7-ae2f-41c1a0715dc3/atlas-halieutique_en.pdf',
                      'https://catalogue.ogsl.ca/data/aghamm/bbe35312-987a-42d7-ae2f-41c1a0715dc3/peches_commerciales.gdb.zip',
                      'https://catalogue.ogsl.ca/data/aghamm/bbe35312-987a-42d7-ae2f-41c1a0715dc3/heatmap.tif',
                      'https://catalogue.ogsl.ca/data/aghamm/bbe35312-987a-42d7-ae2f-41c1a0715dc3/descriptionchamps-peches_commerciales.xlsx')

    # Download
    download.file(peche_aghamm[1], destfile = paste0(folder, 'atlas-halieutique_fr.pdf'))
    download.file(peche_aghamm[2], destfile = paste0(folder, 'atlas-halieutique_en.pdf'))
    download.file(peche_aghamm[3], destfile = paste0(folder, 'peches_commerciales.gdb.zip'))
    download.file(peche_aghamm[4], destfile = paste0(folder, 'heatmap.tif'))
    download.file(peche_aghamm[5], destfile = paste0(folder, 'descriptionchamps-peches_commerciales.xlsx'))

    # Unzip
    unzip(zipfile = paste0(folder, 'peches_commerciales.gdb.zip'), exdir = folder)
  }
  # _________________________________________________________________________ #
}

getSaumon <- function() {
  output <- './analysis/data/cv/site_important/saumon_atlantique/'
  if (!file.exists(output)) dir.create(output)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Saumon Atlantique
  # ------------------------------------
  # dataID: 0017
  # ~~~~~~~~~~~~
  #
  # Important sites for Atlantic salmon for the communities of
  # Gesgapegiag, Gespeg and Viger
  #
  # Les données de saumon Atlantique, pour nos besoins, correspondraient à
  # l’embouchure des Rivières Rimouski et Métis puisque le bassin versant
  # n’est pas inclu dans notre aire d’étude. Je pourrai être en mesure de
  # cibler les embouchures moi-même et j’utiliserai  le rapport sur l’Atlas
  # comme citation:
  # https://aghamm.maps.arcgis.com/apps/Cascade/index.html?appid=73f20ada030f424d89200b8c5473f849
  #
  # Une citation de prise de bar rayé dans le secteur de la  la Matane.
  # Tu pourrais  mettre l’estuaire de cette rivière comme importante étant une
  # rivière à saumon également:
  # https://aghamm.maps.arcgis.com/apps/Cascade/index.html?appid=bfa444495fae4aa4bf8760100b8b6bf2
  #
  # Atlas:
  # Arsenault, L.M. Racine, M.-J. and Lambert Koizumi, C. (2017) Atlas of Marine
  # St. Lawrence Mi’gmaq and Maliseet Sites and Their Uses by the Gesgapegiag,
  # Gespeg and Viger Communities. Mi’gmaq Maliseet Aboriginal Fisheries Management
  # Association (MMAFMA), 46 p.
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Output folder
  folder <- paste0(output, 'saumon_atlantique_aghamm/')
  if (!file.exists(folder)) dir.create(folder)

  # Coordonnées de l'embouchure des rivières
  df <- data.frame(riviere = c('rimouski','metis','matane'),
                   longitude = c(-68.54104, -68.13395, -67.53268),
                   latitude = c(48.44548, 48.63192, 48.85499))

  # Crééer géometries pour l'embouchures des trois rivières d'intérêt
  saumon_aghamm <- st_as_sf(df, coords = c('longitude','latitude'), crs = 4326) %>%
                   st_transform(32198)

  # Export data
  st_write(saumon_aghamm, paste0(folder, 'saumon_atlantique_aghamm.geojson'))
}
