#' Data 0015 : Ancrages
#'
#' Distribution des sites d'ancrages de SIGTM-INNAV dans l'Est du Canada
#'
#' @keywords ancrage
#' @keywords stresseurs
#'
#' @source INNAV
#'
#' @export
#'
#' @details Cette fonction formatte les données
#'

get_data0015 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # WARNING: Data cannot be shared
  #
  # For use in CEA for St Lawrence pilot site.
  #
  # Used here with permission from:
  # Innav Helpdesk
  # Fisheries and Oceans Canada
  # Canadian Coast Guard
  # 101 Champlain Boulevard
  # Quebec (Quebec)
  # Canada
  # G1K 7Y7
  #
  # E-mail: CenHelpDesk@innav.gc.ca
  #
  # ------------------------------------
  # Metadata
  # ------------------------------------
  #
  # SO-19139 Metadata
  # Metadata Information:
  # Metadata language:
  # Metadata character set: utf8
  #
  # Last update: 2020-04-28
  #
  # Scope of the data described by the metadata: dataset
  # Scope name: dataset
  #
  # Name of the metadata standard used: ISO 19139 Geographic Information - Metadata - Implementation Specification
  # Version of the metadata standard: 2007
  #
  # Resource Identification Information:
  # Citation:
  # Title: INNAV_Marine_Navigation_Objects_ANCHORAGES_2019_EAST
  #
  # Presentation format: mapDigital
  #
  # Descriptive keywords - :
  # Keywords: Downloadable Data
  #
  # Descriptive keywords - :
  # Keywords: ULaval, Quebec
  #
  # Descriptive keywords - :
  # Keywords: ULaval, Quebec
  #
  # Abstract:
  #  Eastern Canada anchorage locations from INNAV.For use in CEA for St Lawrence pilot site.
  #
  # Purpose:
  #  Eastern Canada anchorage locations from INNAV
  #
  # Dataset language: eng
  # Dataset character set: utf8
  #
  # Resource constraints:
  # Constraints:
  # Limitations of use: For use in CEA for St Lawrence pilot site.Used here with permission from:Innav HelpdeskFisheries and Oceans CanadaCanadian Coast Guard101 Champlain BoulevardQuebec (Quebec)CanadaG1K 7Y7E-mail: CenHelpDesk@innav.gc.ca
  #
  # Spatial representation type: vector
  #
  # Processing environment: Esri ArcGIS 12.2.1.12813
  #
  # Credits:
  #  Fisheries and Oceans Canada
  #
  # Spatial Representation - Vector:
  # Level of topology for this dataset: geometryOnly
  # Geometric objects:
  # Object type: point
  #
  # Reference System Information:
  # Reference system identifier:
  # Value: 3857
  #
  # Code space: EPSG
  # Version: 8.8(9.3.1.2)
  #
  # Distribution Information:
  # Format:
  # Format name: File Geodatabase Feature Class
  # Format version:
  # ----------------------------------------------------------------------------

  # Output folder
  output <- "data0015-ancrage/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # WARNING: Data transfered physically, no cloud access currently

  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  data0015 <- st_read(paste0(folder, 'INNAV_Marine_Navigation_Objects_ANCHORAGES_2019_EAST.geojson'))

  # Transform projection
  data0015 <- st_transform(data0015, crs = global_parameters()$crs)
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0015,
           dsn = "./data/data-format/data0015-ancrage.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #
}
