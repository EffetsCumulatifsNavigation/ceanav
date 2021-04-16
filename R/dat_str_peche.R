
getPecheCommerciale <- function() {
  # Output folder
  output <- './analysis/data/stresseurs/peches/'
  if (!file.exists(output)) dir.create(output)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Pêches commerciales ZIFF
  # ------------------------------------
  # dataID: 0029
  # ~~~~~~~~~~~~
  #
  # Données de pêche commerciales
  #
  # The data comes from eDrivers, using the eDrivers package
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Output folder
  folder <- paste0(output, 'peches_ziff/')
  if (!file.exists(folder)) dir.create(folder)

  # Load data from eDrivers
  library(eDrivers)
  fetchDrivers(drivers = c('FisheriesDD','FisheriesDNH','FisheriesDNL','FisheriesPLB','FisheriesPHB'),
               output = folder)
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
}
