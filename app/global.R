library(ceanav)
library(leaflet)
library(mapedit)
library(leafem)

# List of available data
dat <- data(package = 'ceanav')$results[,'Item'] %>%
       gsub(" \\(([^\\)]+)\\)", "", .)

# Colors
cols <- c("#c7cbce", "#687677", "#222d3d", "#25364a", "#172434",
          "#ad6a11", "#e6a331", "#e4be29", "#f2ea8b")
