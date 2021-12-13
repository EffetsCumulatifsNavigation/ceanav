#' Download and format basemap data used for figures
#'
#' Accesses and formats spatial data that I use frequently for static mapping
#'
#'
#' @export

get_basemap <- function() {
  global_parameters()
  # Quebec
  canada <- getData('GADM', country = 'CAN', level = 1, path = 'data/data-basemap/')
  canada <- st_as_sf(canada)
  quebec <- canada[canada$NAME_1 %in% c('Québec', 'Nova Scotia','New Brunswick', 'Newfoundland and Labrador', 'Prince Edward Island'),]
  quebec <- suppressWarnings(st_simplify(quebec, dTolerance = 100, preserveTopology = F)) %>%
            st_transform(crs = global_param$crs)
  st_write(obj = quebec,
           dsn = "./data/data-basemap/quebec.geojson",
           delete_dsn = TRUE)

  # Canada
  canada <- getData('GADM', country = 'CAN', level = 0, path = 'data/data-basemap/')
  canada <- st_as_sf(canada)
  canada <- suppressWarnings(st_simplify(canada, dTolerance = 150, preserveTopology = F)) %>%
            st_transform(crs = global_param$crs)
  st_write(obj = canada,
           dsn = "./data/data-basemap/canada.geojson",
           delete_dsn = TRUE)
  # USA
  # Load needed data
  usa <- getData('GADM', country = 'USA', level = 0, path = 'data/data-basemap/')
  usa <- st_as_sf(usa)
  usa <- suppressWarnings(st_simplify(usa, dTolerance = 150, preserveTopology = F)) %>%
            st_transform(crs = global_param$crs)
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



  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                                   CITIES
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cities <- rbind(
    c("Montréal", 45.50254228022055, -73.60212361152767, 0, 10000, -2500),
    c("Sorel-Tracy", 46.04602094647503-.05, -73.11066440843165, 0, 5000, -2500),
    c("Trois-Rivière", 46.369134392835505+.01, -72.57437227244509-.01, 1, -5000, 5000),
    c("Portneuf", 46.69773995828071+.02, -71.89258221436206-.02, 1, -5000, 5000),
    c("Québec", 46.81389793427603+.02, -71.21222137489029-.05, 1, -5000, 5000),
    c("La Pocatière", 47.36885583905954, -70.02321997363107+.04, 0, 5000, -2500),
    c("Baie-Saint-Paul", 47.440943292425075+.03, -70.51229725440152-.04, 1, -5000, 5000),
    c("Tadoussac", 48.1549463212358+.04, -69.7254261992016+.02,.5, -5000, 17500),
    c("Rivière-du-Loup", 47.84514499357216-.01, -69.5537057317311+.07, 0, 5000, -2500),
    c("Rimouski", 48.44414698303665-.03, -68.54511969494227+.05, 0, 5000, -2500),
    c("Forestville", 48.7417453695865+.02, -69.08201781318876-.05, 1, -5000, 5000),
    c("Baie-Comeau", 49.222495074896855+.02, -68.1603788485369-.04, 1, -5000, 5000),
    c("Matane", 48.847708031386716-.04, -67.52783889874046+.03, 0, 5000, -2500),
    c("Saint-Fulgence", 48.452471677247125+.04, -70.90072260561196, 1, -5000, 5000),
    c("La Baie", 48.34102261306717-.03, -70.88195643377449-.05, 1, -5000, 5000)
  ) %>%
  data.frame() %>%
  setNames(c("city","latitude","longitude","adjX","offX","offY")) %>%
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude),
         adjX = as.numeric(adjX),
         offX = as.numeric(offX),
         offY = as.numeric(offY)) %>%
  st_as_sf(coords = c("longitude","latitude"), crs = 4326) %>%
  st_transform(crs = global_parameters()$crs) %>%
  cbind(st_coordinates(.)) %>%
  st_write(dsn = "./data/data-basemap/cities.geojson", delete_dsn = TRUE)




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
