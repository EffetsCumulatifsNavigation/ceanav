#' Data 0082 : Aires protégées publiques et privées
#'
#' Couche de données intégrées qui inclue les aires protégées inscrites au Registre des Aires protégées du Québec, les territoires d’importance, le Réseau de Milieux Naturels protégés (RMN) en plus des habitats fauniques et des écosystèmes forestiers exceptionnels qui ne sont pas inscrits au registre. Les multiparties ont été séparées en partie et les coordonnées des centroides de chaque polygone ont été ajoutées pour servir de localisation gps. Cette couche a été intégrée le 2022/10/13.
#'
#' @export
#'
#' @details Cette fonction formatte les données
#'

get_data0082 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0082-aires_protegees_publiques_privees/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # WARNING: Data transfered physically, no cloud access currently
  # Unzip
  unzip(zipfile = paste0(folder, 'Zones_Conservation.gdb.zip'), exdir = folder)
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import and format data
  # ----------------------------------------
  dat <- sf::st_read(
    paste0(folder, "Zones_Conservation.gdb"), 
    layer = "Ap_plus",
    quiet = TRUE
  )

  # WARNING: There ios something weird with the structure of certain layers,
  # which are LINESTRING, but also MULTISURFACE and CURVEPOLYGON. I cannot find
  # a clean way to deal with this, so I will use a brute force approach.
  # Just keep in mind that if another set of rawdata comes along, this code
  # will likely not be reproducible

  # --------------------------------------------------
  # Function to format individual layers
  geom_format <- function(dat) {
    # Only geometries
    geom <- st_geometry(dat) %>%
            st_zm() %>%
            st_sfc() %>%
            st_as_text() # Transforms geometry as character string

    # Remove MULTISURFACE and CURVEPOLYGON
    geom <- gsub("MULTISURFACE \\(CURVEPOLYGON \\(COMPOUNDCURVE \\(CIRCULARSTRING", "LINESTRING", geom)
    geom <- gsub("MULTISURFACE \\(CURVEPOLYGON \\(", "", geom)
    geom <- gsub("CIRCULARSTRING \\(", "LINESTRING \\(", geom)
    geom <- gsub("MULTIPOLYGON \\(\\(\\(", "LINESTRING \\(", geom)
    geom <- gsub("COMPOUNDCURVE \\(", "", geom)
    geom <- gsub("\\), LINESTRING \\(", ",", geom)
    geom <- gsub("\\)\\)\\)\\)", "\\)", geom)
    geom <- gsub("\\)\\)\\)", "\\)", geom)
    geom <- gsub("\\)\\)", "\\)", geom)
    # geom <- gsub("\\(\\(", "\\(", geom)
    # geom <- gsub("\\), \\(", ",", geom)

    # Recreate polygons
    dat <- st_as_sfc(geom) %>%
           st_sf(st_drop_geometry(dat), crs = st_crs(dat)) %>%
           st_cast("POLYGON")

    # Return
    dat
  }

  # --------------------------------------------------
  data0082 <- geom_format(dat) |>
              st_transform(crs = global_parameters()$crs)
  # _________________________________________________________________________ #    

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0082,
           dsn = "./data/data-format/data0082-aires_protegees_publiques_privees.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #
}
