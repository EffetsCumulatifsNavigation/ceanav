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
# Libraries
# ---------------------------------------
#
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
library(sf)
library(stars)
library(tidyverse)
library(magrittr)
# _____________________________________________________________________________ #


# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
# Study area
# ---------------------------------------
#
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
# URL
studyarea <- 'https://github.com/EffetsCumulatifsNavigation/ZoneEtude/raw/main/Data/StudyArea/StudyArea.geojson'

# Download
download.file(studyarea, destfile = './analysis/output/aoi/StudyArea.geojson')

# Import
studyarea <- st_read('./analysis/output/aoi/StudyArea.geojson')

# Export as package data
save(studyarea, file = './data/studyarea.RData')
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
download.file(grid1000poly, destfile = './analysis/output/aoi/Grid_Poly1000.geojson')
download.file(grid2000poly, destfile = './analysis/output/aoi/Grid_Poly2000.geojson')
download.file(grid1000ras, destfile = './analysis/output/aoi/Grid_Raster1000.gpkg')
download.file(grid2000ras, destfile = './analysis/output/aoi/Grid_Raster2000.gpkg')

# Import
grid1000poly <- st_read('./analysis/output/aoi/Grid_Poly1000.geojson')
grid2000poly <- st_read('./analysis/output/aoi/Grid_Poly2000.geojson')
# grid1000ras <- read_stars('./analysis/output/aoi/Grid_Raster1000.gpkg')
# grid2000ras <- read_stars('./analysis/output/aoi/Grid_Raster2000.gpkg')

# Export as package data
save(grid1000poly, file = './data/grid1000poly.RData')
save(grid2000poly, file = './data/grid2000poly.RData')
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
download.file(egslSimple, destfile = './data/egslSimple.RData')
# _____________________________________________________________________________ #
}
