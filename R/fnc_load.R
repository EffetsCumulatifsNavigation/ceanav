#' Load data, metadata and contact information
#'
#' The function imports data available in `./data/data-format/` regardless of its format.
#'
#' @param data_id `character` id of data to import in R session with format `dataXXXX` for `ceanav_load_data()`, `metadata_dataXXXX` for `ceanav_load_metadata()`, and `contact_dataXXXX` for `ceanav_load_contact()`. `ceanav_load_all()` loads data, metadata and contact for `data_id`
#'
#' @keywords metadata
#' @keywords contact
#' @keywords data
#'
#' @export
#'
#' @details This function is used to avoid putting all formatted dataset in lazy load, yet still make it easy for them to be loaded. The user simply provides the id of the dataset and the function imports it in the R session, along with its metadata and contact information if desired
#'
#' @examples
#' ceanav_load("data0001")

ceanav_load_data <- function(data_id) {
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

# =================================================================
#' @name ceanav_load_metadata
#' @export
ceanav_load_metadata <- function(data_id) {
  ## ---------------------------------------------
  ## YAML (metatada)
  path <- paste0("./data/data-metadata/", data_id, ".yml")
  assign(x = paste0("metadata_", data_id),
         value = yaml::read_yaml(path),
         envir = globalenv())
}

# =================================================================
#' @name ceanav_load_contact
#' @export
ceanav_load_contact <- function(data_id) {
  ## ---------------------------------------------
  ## YAML (contact)
  ceanav_load_metadata(data_id)
  uid <- get(paste0("metadata_", data_id))$data_description$contact_id
  path <- paste0("./data/data-contact/pr", uid, ".yml")

  # -----
  dat <- list()
  for(i in 1:length(path)) {
    dat[[i]] <- yaml::read_yaml(path[i])
  }

  # ------
  assign(x = paste0("contact_", data_id),
         value = dat,
         envir = globalenv())
}

# =================================================================
#' @name ceanav_load_all
#' @export
ceanav_load_all <- function(data_id) {
  ceanav_load_data(data_id)
  ceanav_load_metadata(data_id)
  ceanav_load_contact(data_id)
}
