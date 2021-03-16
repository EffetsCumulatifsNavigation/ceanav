

getPeche <- function() {
  output <- './analysis/data/cv/site_important/peche_commerciale/'
  if (!file.exists(output)) dir.create(output)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Commercial fisheries - Mi'gmaq and Maliseet
  # ------------------------------------
  # dataID: 0015
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
