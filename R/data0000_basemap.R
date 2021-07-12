#' Download and format basemap data used for figures
#'
#' Accesses and formats spatial data that I use frequently for static mapping
#'
#'
#' @export

get_basemap <- function() {
  # Quebec
  canada <- getData('GADM', country = 'CAN', level = 1, path = 'data/data-basemap/')
  canada <- st_as_sf(canada)
  quebec <- canada[canada$NAME_1 %in% c('Québec', 'Nova Scotia','New Brunswick', 'Newfoundland and Labrador', 'Prince Edward Island'),]
  quebec <- suppressWarnings(st_simplify(quebec, dTolerance = 100, preserveTopology = F))
  st_write(obj = quebec,
           dsn = "./data/data-basemap/quebec.geojson",
           delete_dsn = TRUE)

  # Canada
  canada <- getData('GADM', country = 'CAN', level = 0, path = 'data/data-basemap/')
  canada <- st_as_sf(canada)
  canada <- suppressWarnings(st_simplify(canada, dTolerance = 150, preserveTopology = F))
  st_write(obj = canada,
           dsn = "./data/data-basemap/canada.geojson",
           delete_dsn = TRUE)
  # USA
  # Load needed data
  usa <- getData('GADM', country = 'USA', level = 0, path = 'data/data-basemap/')
  usa <- st_as_sf(usa)
  usa <- suppressWarnings(st_simplify(usa, dTolerance = 150, preserveTopology = F))
  st_write(obj = usa,
           dsn = "./data/data-basemap/usa.geojson",
           delete_dsn = TRUE)

  # Delete loaded data
  file.remove("data/data-basemap/gadm36_CAN_1_sp.rds")
  file.remove("data/data-basemap/gadm36_CAN_0_sp.rds")
  file.remove("data/data-basemap/gadm36_USA_0_sp.rds")

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Simple EGSL outline from eDrivers
  # ---------------------------------------
  #
  # Import only for convenience, directly as package data
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # URL
  egsl <- 'https://github.com/eDrivers/eDriversGrids/raw/master/Data/RawData/egsl.zip'

  # Download
  download.file(egsl, destfile = "./data/data-basemap/egsl.zip")

  # Unzip
  unzip("./data/data-basemap/egsl.zip", exdir = "./data/data-basemap/egsl/")

  # Load
  library(sp)
  egsl <- st_read("./data/data-basemap/egsl/egsl.shp") %>%
          as("Spatial")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                               SIMPLIFIED CONTOUR
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Selected plygons in egsl (remove small islands that add many vertex to the dataset)
  j <- c(1, 9127, 8999, 9038, 9005, 8998, 131, 18, 17)
  p <- list()
  for(i in 1:length(j)) {
    p[[i]] <- Polygon(egsl@polygons[[1]]@Polygons[[j[i]]]@coords)
  }
  ps <- Polygons(p, 1)
  sps <- SpatialPolygons(list(ps))
  proj4string(sps) <- proj4string(egsl)

  # Transform to sf object and simplify
  egsl <- sps %>%
          SpatialPolygonsDataFrame(. , data.frame(ID = 1)) %>%
          st_as_sf() %>%
          st_transform(crs = 32198) %>%
          st_simplify(preserveTopology = T, dTolerance = 100)

  # -----
  st_write(obj = egsl,
           dsn = "./data/data-basemap/egsl.geojson",
           delete_dsn = TRUE)

  # -----
  file.remove("data/data-basemap/egsl.zip")
  file.remove("data/data-basemap/egsl/egsl.dbf")
  file.remove("data/data-basemap/egsl/egsl.prj")
  file.remove("data/data-basemap/egsl/egsl.sbn")
  file.remove("data/data-basemap/egsl/egsl.sbx")
  file.remove("data/data-basemap/egsl/egsl.shp")
  file.remove("data/data-basemap/egsl/egsl.shx")
  file.remove("data/data-basemap/egsl/")
  # _____________________________________________________________________________ #
}
