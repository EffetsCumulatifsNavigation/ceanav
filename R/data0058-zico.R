#' Data 0058 :
#'
#' Les zones importantes pour la conservation des oiseaux (ZICO) sont des sites qui abritent des groupes d’oiseaux bien précis : espèces menacées, vaste population d’oiseaux ou espèces ayant une aire de répartition réduite. De tailles variées, les ZICO peuvent couvrir de minuscules parcelles de territoire comme de vastes étendues de terre, des rivières et des plans d’eau. Elles peuvent être constituées de terres publiques ou privées et peuvent chevaucher en tout ou en partie des aires protégées.
#'
#' @keywords oiseaux
#' @keywords zico
#'
#' @source https://www.ibacanada.com/explore_how.jsp?lang=fr
#'
#' @export
#'
#' @details This function loads and formats the data
#'

get_data0058 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  #
  # WARNING: For some reason, I was unable to import the KMZ file available from
  # https://www.ibacanada.com/explore_how.jsp?lang=fr in R, even after I uncompressed it to a KML.
  #
  # I therefore imported the file in Google Earth Pro, then exported it as a KML file.
  # The KML file imports successfully in R using sf.
  #
  # There might be a misunderstanding on my end as to how to deal with KMZ files in R. Still,
  # this roundabout strategy works and will be sufficient for our needs. I therefore did not dig
  # deeper into the problem.
  # ----------------------------------------
  # Output folder
  output <- "data0058-zico/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  data0058 <- st_read(paste0(folder, 'CanIBA.kml'), quiet = TRUE) %>%
              st_cast("MULTIPOLYGON") %>%
              st_zm() %>%
              st_transform(crs = global_parameters()$crs)
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0058,
           dsn = "./data/data-format/data0058-zico.geojson",
           delete_dsn = TRUE,
           quiet = TRUE)
  # _________________________________________________________________________ #

}
