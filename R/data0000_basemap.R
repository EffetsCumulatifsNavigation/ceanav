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
  quebec <- suppressWarnings(st_simplify(quebec, dTolerance = .01, preserveTopology = F))
  st_write(obj = quebec,
           dsn = "./data/data-basemap/quebec.geojson",
           delete_dsn = TRUE)

  # Canada
  canada <- getData('GADM', country = 'CAN', level = 0, path = 'data/data-basemap/')
  canada <- st_as_sf(canada)
  canada <- suppressWarnings(st_simplify(canada, dTolerance = .01, preserveTopology = F))
  st_write(obj = canada,
           dsn = "./data/data-basemap/canada.geojson",
           delete_dsn = TRUE)
  # USA
  # Load needed data
  usa <- getData('GADM', country = 'USA', level = 0, path = 'data/data-basemap/')
  usa <- st_as_sf(usa)
  usa <- suppressWarnings(st_simplify(usa, dTolerance = .05, preserveTopology = F))
  st_write(obj = usa,
           dsn = "./data/data-basemap/usa.geojson",
           delete_dsn = TRUE)

  # Delete loaded data
  file.remove("data/data-basemap/gadm36_CAN_1_sp.rds")
  file.remove("data/data-basemap/gadm36_CAN_0_sp.rds")
  file.remove("data/data-basemap/gadm36_USA_0_sp.rds")
}
