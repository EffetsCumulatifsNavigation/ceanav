getMammiferesMarins <- function() {
  output <- './analysis/data/cv/mammiferes_marins/'
  if (!file.exists(output)) dir.create(output)

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Mammifères marins
  # ------------------------------------
  # dataID:
  # ~~~~~~~~~~~~
  #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Output folder
  folder <- paste0(output, 'mammiferes_marins_romm_wwf/')
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'CetaceanHeatmapRasters/'))) {
    # Unzip
    unzip(zipfile = paste0(folder, 'CetaceanHeatmapRasters.zip'), exdir = folder)
  }
  # _________________________________________________________________________ #
}
