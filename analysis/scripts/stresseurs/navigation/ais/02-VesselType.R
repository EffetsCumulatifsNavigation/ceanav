library(magrittr)
library(tidyverse)

# Vessel attributes dataset
df <- read.csv('Data/RawData/STATIC_DATA_2015_to_2019_forULAVAL.csv')

# Navigation data (2019)
folder <- dir('Data/RawData/CSV_MS_QC_2019', full.names = TRUE)
nf <- length(folder)

# Export unique vessels
y <- list()
for(i in 1:nf) y[[i]] <- read.csv(folder[i])$MMSI %>% unique()
y <- unlist(y) %>% unique()

# Select vessels observed un 2019
df <- df[df$MMSI %in% y, ]

# Unique vessel types
table(df$NTYPE)
table(df$SPECIFIC_VESS_TYPE)

# Create shapefile
library(sf)
