#' Load formatted data available in `./data/data-format/`
#'
#' The function imports data available in `./data/data-format/` regardless of its format.
#'
#' @param data_id `character` id of data to import in R session with format `dataXXXX`
#'
#' @keywords metadata
#' @keywords contact
#'
#' @export
#'
#' @details This function is used to avoid putting all formatted dataset in lazy load, yet still make it easy for them to be loaded. The user simply provides the id of the dataset and the function imports it in the R session
#'
#' @examples
#' ceanav_load('data0001')

ceanav_load <- function(data_id) {

  # List files
  # WARNING: This might not be the best way. Perhaps I should create a table of data automatically.
  files <- dir('./data/data-format/', full.names = TRUE)

  # Identify dataset to load
  uid <- str_detect(files, data_id)

  # Identify extensions
  ext <- last(str_split(files[uid], "\\.")[[1]])

  # Load according to extension type
  ## ---------------------------------------------
  ## GEOJSON
  if (ext == "geojson") {
    assign(x = data_id,
           value = st_read(files[uid], quiet = TRUE),
           envir = globalenv())
  }

  ## ---------------------------------------------
  ## GeoTIFF
  if (ext == "tif") {
    assign(x = data_id,
           value = read_stars(files[uid], quiet = TRUE),
           envir = globalenv())
  }

  ## ---------------------------------------------
  ## CSV
  if (ext == "csv") {
    assign(x = data_id,
           value = read.csv(files[uid]),
           envir = globalenv())
  }

}
