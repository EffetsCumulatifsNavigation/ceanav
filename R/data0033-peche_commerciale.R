#' Data 0033 : Pêche commerciale
#'
#' A compilation of landing data from Zonal Interchange File Format (ZIFF) data between 2000 and 2020
#'
#' @keywords pêche commerciale
#' @keywords stresseurs
#'
#' @source Fisheries and Oceans Canada (2021). Departement of Fisheries and Oceans Canada’s Fisheries and Oceans Canada Zonal Interchange File Format (ZIFF) data. A compilation of landing data from logbook data between 2000 and 2020. [Data accessed: 2021-06-11]".
#'
#' @export
#'
#' @details Cette fonction formatte les données
#'

get_data0033 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # WARNING:
  message("Les données brutes sont soumis à une entente de partage de données")

  # Output folder
  output <- "data0033-peche_commerciale/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # WARNING: Data transfered physically, no cloud access currently
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import and format data
  # ----------------------------------------
  # WARNING: This dataset is very big, hence I only select data that intersect
  #          our area of interest.
  data(grid1p)
  grid1p <- st_transform(grid1p, crs = 4326)

  # Function to import ZIFF data
  import_ziff <- function(filename) {
    # Import
    read.csv(paste0(folder, filename)) %>%

    # Remove NAs
    filter(!is.na(latit_GIS) & !is.na(longit_GIS)) %>% # Remove NAs

    # Create spatial object
    st_as_sf(coords = c("longit_GIS","latit_GIS"),
             crs = 4326) %>%

    # Select points in study area
    .[grid1p, ]
  }

  # Import in list
  d <- list()
  d[[1]] <- import_ziff("Version_totale_20002004.csv")
  d[[2]] <- import_ziff("Version_totale_20052009.csv")
  d[[3]] <- import_ziff("Version_totale_20102014.csv")
  d[[4]] <- import_ziff("Version_totale_20152019.csv")
  d[[5]] <- import_ziff("Version_totale_20202024.csv")

  # Single dataset
  data0033 <- bind_rows(d)

  # Transform projection
  data0033 <- st_transform(data0033, crs = global_parameters()$crs)
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0033,
           dsn = "./data/data-format/data0033-peche_commerciale.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #
}
