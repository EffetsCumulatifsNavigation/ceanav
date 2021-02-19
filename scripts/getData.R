# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
# Notes
# ---------------------------------------
#
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
# This script is built to get the data from various sources on the internet and
# locally
#
# The package is not meant for publication, and this script is also not meant
# to be reprpducible
#
# The goal is to facilitate the analyses and remove the need to load the data
# from various repos all the time
#
# The actual repos should be fully reproducible, and the final workflow should
# not call data from the package
#
# This is mostly for exploration purposes and ease of work in the future
#
# Use to see list of data available in the package:
# data(package = "ceanavR")
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
download.file(studyarea, destfile = './data/StudyArea.geojson')

# Import
studyarea <- st_read('./data/StudyArea.geojson')

# Export
save(studyarea, file = './data/studyarea.RData')

# Delete
file.remove('./data/StudyArea.geojson')
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
download.file(grid1000poly, destfile = './data/Grid_Poly1000.geojson')
download.file(grid2000poly, destfile = './data/Grid_Poly2000.geojson')
download.file(grid1000ras, destfile = './data/Grid_Raster1000.gpkg')
download.file(grid2000ras, destfile = './data/Grid_Raster2000.gpkg')

# Import
grid1000poly <- st_read('./data/Grid_Poly1000.geojson')
grid2000poly <- st_read('./data/Grid_Poly2000.geojson')
grid1000ras <- read_stars('./data/Grid_Raster1000.gpkg')
grid2000ras <- read_stars('./data/Grid_Raster2000.gpkg')

# Export
save(grid1000poly, file = './data/grid1000poly.RData')
save(grid2000poly, file = './data/grid2000poly.RData')
save(grid1000ras, file = './data/grid1000ras.RData')
save(grid2000ras, file = './data/grid2000ras.RData')

# Delete
file.remove('./data/Grid_Poly1000.geojson')
file.remove('./data/Grid_Poly2000.geojson')
file.remove('./data/Grid_Raster1000.gpkg')
file.remove('./data/Grid_Raster2000.gpkg')
# _____________________________________________________________________________ #


# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
# Simple EGSL outline from eDrivers
# ---------------------------------------
#
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
# URL
egslSimple <- 'https://github.com/eDrivers/eDriversGrids/raw/master/Data/egslSimple.RData'

# Download
download.file(egslSimple, destfile = './data/egslSimple.RData')
# _____________________________________________________________________________ #
