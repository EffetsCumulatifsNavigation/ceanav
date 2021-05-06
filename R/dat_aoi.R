getAOI <- function () {
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
# URL
studyarea <- 'https://github.com/EffetsCumulatifsNavigation/ZoneEtude/raw/main/Data/StudyArea/StudyArea.geojson'

# Download
download.file(studyarea, destfile = './analysis/data/aoi/StudyArea.geojson')

# Import
studyarea <- st_read('./analysis/data/aoi/StudyArea.geojson')

# Export as package data
save(studyarea, file = './data/aoi_studyarea.RData')
# _____________________________________________________________________________ #



# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
# Study grids
# ---------------------------------------
#
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
# URLs
grid1000poly <- 'https://github.com/EffetsCumulatifsNavigation/ZoneEtude/raw/main/Data/StudyGrid/Grid_Poly1000.geojson'
grid2000poly <- 'https://github.com/EffetsCumulatifsNavigation/ZoneEtude/raw/main/Data/StudyGrid/Grid_Poly2000.geojson'
grid1000ras <- 'https://github.com/EffetsCumulatifsNavigation/ZoneEtude/raw/main/Data/StudyGrid/Grid_Raster1000.gpkg'
grid2000ras <- 'https://github.com/EffetsCumulatifsNavigation/ZoneEtude/raw/main/Data/StudyGrid/Grid_Raster2000.gpkg'

# Download
download.file(grid1000poly, destfile = './analysis/data/aoi/Grid_Poly1000.geojson')
download.file(grid2000poly, destfile = './analysis/data/aoi/Grid_Poly2000.geojson')
download.file(grid1000ras, destfile = './analysis/data/aoi/Grid_Raster1000.gpkg')
download.file(grid2000ras, destfile = './analysis/data/aoi/Grid_Raster2000.gpkg')

# Import
aoi <- st_read('./analysis/data/aoi/Grid_Poly1000.geojson')
aoi2 <- st_read('./analysis/data/aoi/Grid_Poly2000.geojson')
# grid1000ras <- read_stars('./analysis/data/aoi/Grid_Raster1000.gpkg')
# grid2000ras <- read_stars('./analysis/data/aoi/Grid_Raster2000.gpkg')

# Remove unnecessary column
aoi <- select(aoi, -val_ras)
aoi2 <- select(aoi2, -val_ras)

# Export as package data
save(aoi, file = './data/aoi_grid1000poly.RData')
save(aoi2, file = './data/aoi_grid2000poly.RData')
# save(grid1000ras, file = './data/grid1000ras.RData')
# save(grid2000ras, file = './data/grid2000ras.RData')
# _____________________________________________________________________________ #


# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
# Simple EGSL outline from eDrivers
# ---------------------------------------
#
# Import only for convenience, directly as package data
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
# URL
egslSimple <- 'https://github.com/eDrivers/eDriversGrids/raw/master/Data/egslSimple.RData'

# Download
download.file(egslSimple, destfile = './data/aoi_egsl.RData')
# _____________________________________________________________________________ #
}
