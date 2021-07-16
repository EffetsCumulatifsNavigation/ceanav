#' Function to evaluate fishing intensity
#'
#' The function evaluates fishing intensity over a regular grid
#'
#' @param fishData fishing data as sf object
#' @param areaGrid regular grid as sf object
#' @param type type(s) of fishing intensity metric to evaluate: 1: Fishing effort density, no normalization; 2: Fishing effort density; 3: Fishing biomass yield density; 4: Fishing relative biomass yield density

#' @keywords fishing intensity
#'
#' @export
#'
#' @details This function uses fisheries data to evaluate the intensity of fishing over a regular grid
#'

fishingMetrics <- function(fishData, areaGrid, type = 3) {
  # ~~~~~       INITIAL MEASUREMENTS        ~~~~~ #

  # Calculate total area of all points transformed in polygons and bind to polygons
    fishData$AreaTotKM2 <- as.numeric(sf::st_area(fishData) / 1000000)

  # Calculate total biomass and bind to polygons
    fishData$BiomassTotKg <- sum(fishData$catch)

  # Intersect points as polygons to study grid as polygons
    fishData <- suppressWarnings(sf::st_intersection(areaGrid, fishData))

  # Calculate area of intersected geometries and bind to spatial polygons data frame
    fishData$AreaKM2 <- as.numeric(sf::st_area(fishData) / 1000000)

  # Calculate proportion of intersected geometries from total area and bind to spatial polygons data frame
    fishData$PropAreaTot <- fishData$AreaKM2 / fishData$AreaTotKM2

  # Calculate relative biomass as a proportion of fishing area and bind to polygons data frame
    fishData$PropBiomassKg <- fishData$catch * fishData$PropAreaTot

  # Calculate biomass as a proportion of fishing area and bind to polygons data frame
    fishData$RelPropBiomassKg <- fishData$PropBiomassKg / fishData$BiomassTotKg


                # ~~~~~       FISHING METRICS        ~~~~~ #

  if (1 %in% type) {
    # Metric 1: Fishing effort density, no normalization
    areaGrid <- as.character(fishData$ID) %>%
                table() %>%
                as.data.frame(stringsAsFactors = F) %>%
                rename(ID = 1, FishEffortDens = 2) %>%
                left_join(areaGrid, ., by = 'ID') %>%
                mutate(FishEffortDens = ifelse(is.na(FishEffortDens), 0, FishEffortDens))
  }

  if (2 %in% type) {
    # Metric 2: Fishing effort density
    areaGrid <- fishData %>%
                sf::st_set_geometry(NULL) %>%
                dplyr::group_by(ID) %>%
                dplyr::summarize(FishEffortDensProp = sum(PropAreaTot)) %>%
                rename(ID = 1, FishEffortDensProp = 2) %>%
                left_join(areaGrid, ., by = 'ID') %>%
                mutate(FishEffortDensProp = ifelse(is.na(FishEffortDensProp), 0, FishEffortDensProp))
  }

  if (3 %in% type) {
    # Metric 3: Fishing biomass yield density
    areaGrid <- fishData %>%
                sf::st_set_geometry(NULL) %>%
                dplyr::group_by(ID) %>%
                dplyr::summarize(FishBiomassKg = sum(PropBiomassKg)) %>%
                rename(ID = 1, PropBiomassKg = 2) %>%
                left_join(areaGrid, ., by = 'ID') %>%
                mutate(PropBiomassKg = ifelse(is.na(PropBiomassKg), 0, PropBiomassKg))
  }

  if (4 %in% type) {
    # Metric 4: Fishing relative biomass yield density
    egslGrid <- fishData %>%
                sf::st_set_geometry(NULL) %>%
                dplyr::group_by(ID) %>%
                dplyr::summarize(RelFishBiomassKg = sum(RelPropBiomassKg)) %>%
                rename(ID = 1, RelPropBiomassKg = 2) %>%
                left_join(areaGrid, ., by = 'ID') %>%
                mutate(RelPropBiomassKg = ifelse(is.na(RelPropBiomassKg), 0, RelPropBiomassKg))
  }

  # Remove geometry column
      st_geometry(areaGrid) <- NULL

  return(areaGrid)
}
