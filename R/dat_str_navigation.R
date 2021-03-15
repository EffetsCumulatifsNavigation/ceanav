
getNavigation <- function() {
  # Output folder
  output <- './analysis/data/stresseurs/navigation'
  if (!file.exists(output)) dir.create(output)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Navigation AIS
  # ------------------------------------
  # dataID: 0015
  # ~~~~~~~~~~~~
  #
  # Données AIS - à décrire
  #
  # WARNING: Données provenant du MFFP, non disponible en ligne
  # WARNING: Données ne peuvent être partagées
  # WARNING: Données brutes doivent être supprimées suite au projet
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Output folder
  folder <- paste0(output, 'navigation_ais/')

  # Unzip data -----
  ## 2017
  unzip(zipfile = './analysis/data/stresseurs/navigation/navigation_ais/2017-AIS.zip',
        exdir = './analysis/data/stresseurs/navigation/navigation_ais/2017-AIS')

  ## 2018
  unzip(zipfile = './analysis/data/stresseurs/navigation/navigation_ais/2018-AIS.zip',
        exdir = './analysis/data/stresseurs/navigation/navigation_ais/2018-AIS')

  ## 2019
  unzip(zipfile = './analysis/data/stresseurs/navigation/navigation_ais/2019-AIS.zip',
        exdir = './analysis/data/stresseurs/navigation/navigation_ais/2019-AIS')
}
