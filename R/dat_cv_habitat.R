
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


fmtBenthic <- function() {

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  folder <- './analysis/data/cv/habitats/benthique/'

  # dataID: 0011
  # Mégahabitats
  benthique_mega <- st_read(paste0(folder, 'benthique_mega/Megahabitats_DB.shp')) %>%
                    st_transform(32198)

  # dataID: 0010
  # Géologie Loring et Nota
  # WARNING: Non considéré, inclu dans mégahabitats benthiques dataID: 0011
  # benthique_ln <- st_read(paste0(folder, 'benthique_ln/Seafloor_SubstratBenthique.shp')) %>%
  #                 st_transform(32198)
  # ------------------------------------------------------------------------- #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Format data
  # ------------------------------------
  #
  # All we will do for now for this dataset is include it as presence-absence
  # in the study grid
  #
  # So I intersect the zostere db with the grid to identify which grid cell
  # intersect with the db
  #
  # See report for a description of the different habitat types
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load grid
  data(grid1p)

  # Use grid as dataset
  benthique <- grid1p

  # For each coastal habitat type
  hab <- unique(benthique_mega$Megahabita) %>%
         .[!is.na(.)]

  for(i in hab) {
    # Segments for habitat i
    habid <- benthique_mega$Megahabita == i

    # Identify grid cells with coast habitat types
    uid <- st_intersects(benthique_mega[habid, ], grid1p) %>%
           unlist() %>%
           unique()

    # Add info to grid
    dat <- numeric(nrow(benthique))
    dat[uid] <- 1
    benthique <- cbind(benthique, dat) %>%
                 rename(!!i:=dat)
  }
  # ------------------------------------------------------------------------- #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  save(benthique, file = './data/cv_hab_benthique.RData')
  # ------------------------------------------------------------------------- #

}
