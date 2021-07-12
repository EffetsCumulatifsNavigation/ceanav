#' Load basemap data
#'
#' @param data_name `character` name of basemap data to load in R session
#'
#' @export

basemap <- function(data_name) {
  files <- dir('./data/data-basemap', full.names = TRUE)

  # Identify dataset to load
  uid <- str_detect(files, data_name)

  # Identify extensions
  ext <- last(str_split(files[uid], "\\.")[[1]])

  # Load according to extension type
  ## ---------------------------------------------
  ## GEOJSON
  if (ext == "geojson") {
    assign(x = data_name,
           value = st_read(files[uid], quiet = TRUE),
           envir = globalenv())
  }
}
