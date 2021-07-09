#' Data 0000 : Zone d'intérêt et grilles d'étude
#'
#' Délimitation de la zone d'intérêt et grilles utilisées pour l'étude
#'
#' @keywords zone d'intérêt
#' @keywords grille d'étude
#'
#' @export
#'
#' @details Cette fonction télécharge les données
#'

get_aoi <- function () {
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
# Notes
# ---------------------------------------
#
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
# This script is built to get the data for the study area, which is hosted
# in a different GH repo:
#   https://github.com/EffetsCumulatifsNavigation/ZoneEtude
#
# I may at some point migrate the code for the study area directly in this
# research compendium.
#
# For now I will keep it seperate because I am likely to share the data for
# the study area with outside collaborators.
# _____________________________________________________________________________ #


# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
# Study area
# ---------------------------------------
#
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
# Output folder
output <- "data0000-zone_grille/"
folder <- paste0("./data/data-raw/", output)
if (!file.exists(folder)) dir.create(folder)

# URL
aoi <- 'https://github.com/EffetsCumulatifsNavigation/ZoneEtude/raw/main/Data/StudyArea/StudyArea.geojson'

# Download
download.file(aoi, destfile = paste0(folder, 'StudyArea.geojson'))

# Import
aoi <- st_read(paste0(folder, 'StudyArea.geojson'))

# Export as package data
save(aoi, file = './data/aoi.RData')
# _____________________________________________________________________________ #



# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
# Study grids
# ---------------------------------------
#
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #

# URLs
grid1p <- 'https://github.com/EffetsCumulatifsNavigation/ZoneEtude/raw/main/Data/StudyGrid/Grid_Poly1000.geojson'
grid2p <- 'https://github.com/EffetsCumulatifsNavigation/ZoneEtude/raw/main/Data/StudyGrid/Grid_Poly2000.geojson'
grid1r <- 'https://github.com/EffetsCumulatifsNavigation/ZoneEtude/raw/main/Data/StudyGrid/Grid_Raster1000.gpkg'
grid2r <- 'https://github.com/EffetsCumulatifsNavigation/ZoneEtude/raw/main/Data/StudyGrid/Grid_Raster2000.gpkg'

# Download
download.file(grid1p, destfile = paste0(folder, 'Grid_Poly1000.geojson'))
download.file(grid2p, destfile = paste0(folder, 'Grid_Poly2000.geojson'))
download.file(grid1r, destfile = paste0(folder, 'Grid_Raster1000.gpkg'))
download.file(grid2r, destfile = paste0(folder, 'Grid_Raster2000.gpkg'))

# Import
grid1p <- st_read(paste0(folder, 'Grid_Poly1000.geojson'))
grid2p <- st_read(paste0(folder, 'Grid_Poly2000.geojson'))
# grid1r <- read_stars(paste0(folder, 'Grid_Raster1000.gpkg'))
# grid2r <- read_stars(paste0(folder, 'Grid_Raster2000.gpkg'))

# Remove unnecessary column
grid1p <- select(grid1p, -val_ras)
grid2p <- select(grid2p, -val_ras)

# Export as package data for lazy load
save(grid1p, file = './data/grid1p.RData')
save(grid2p, file = './data/grid2p.RData')
# save(grid1r, file = './data/grid1r.RData')
# save(grid2r, file = './data/grid2r.RData')
# _____________________________________________________________________________ #


# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
# Simple EGSL outline from eDrivers
# ---------------------------------------
#
# Import only for convenience, directly as package data
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
# URL
egsl <- 'https://github.com/eDrivers/eDriversGrids/raw/master/Data/egslSimple.RData'

# Download
download.file(egsl, destfile = paste0(folder,'egsl.RData'))

# Load
load(paste0(folder,'egsl.RData'))
egsl <- egslSimple

# Export as package data for lazy load
save(egsl, file = './data/egsl.RData')
# _____________________________________________________________________________ #
}
