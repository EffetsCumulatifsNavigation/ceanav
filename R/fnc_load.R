#' Load data, metadata and contact information
#'
#' The function imports data available in `./data/data-format/` regardless of its format.
#'
#' @param data_id `character` id of data to import in R session with format `dataXXXX` for `load_format()`, `metadata_dataXXXX` for `load_metadata()`, and `contact_dataXXXX` for `load_contact()`.
#' @param data_name `character` name of integrated data to load in R session with format, one of `navigation`, `peche`, `deversement`, `dragage`, `ancrage`, `rejet`, `epaves` for stressors, and one of `habitat`, `site`, `berge`, `mammifere_marin`, `quality` for valued components.
#' @param basemap_name `character` name of basemap data to load, one of `egsl`, `quebec`, `canada`, `usa`

#' @keywords metadata
#' @keywords contact
#' @keywords data
#'
#' @rdname load
#'
#' @export
#'
#' @details This function is used to avoid putting all formatted dataset in lazy load, yet still make it easy for them to be loaded. The user simply provides the id of the dataset and the function imports it in the R session, along with its metadata and contact information if desired
#'
#' @examples
#' load_format("data0001")

load_format <- function(data_id) {
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
#' @rdname load
#' @export
load_metadata <- function(data_id) {
  ## ---------------------------------------------
  ## YAML (metatada)
  path <- paste0("./data/data-metadata/", data_id, ".yml")
  assign(x = paste0("metadata_", data_id),
         value = read_yaml(path),
         envir = globalenv())
}

# =================================================================
#' @rdname load
#' @export
load_contact <- function(data_id) {
  ## ---------------------------------------------
  ## YAML (contact)
  load_metadata(data_id)
  uid <- get(paste0("metadata_", data_id))$data_description$contact_id
  path <- paste0("./data/data-contact/pr", uid, ".yml")

  # -----
  dat <- list()
  for(i in 1:length(path)) {
    dat[[i]] <- read_yaml(path[i])
  }

  # ------
  assign(x = paste0("contact_", data_id),
         value = dat,
         envir = globalenv())
}

# =================================================================
#' @rdname load
#' @export
load_integrated <- function(data_name) {
  # Possible values
  st <- c("navigation", "peche_commerciale", "deversement", "dragage", "ancrage", "rejet", "epaves", "port")
  cv <- c("habitat", "site", "berge", "mammiferes_marins", "quality")
  nm <- paste(c(st, cv), collapse = ", ")
  if (!data_name %in% c(st,cv)) stop(glue("Les données identifiées ne sont pas disponibles. Utilisez plutôt un des identifiants suivants: {nm}"))

  # List files
  # WARNING: This might not be the best way. Perhaps I should create a table of data automatically.
  files <- dir('./data/data-integrated/', full.names = TRUE)

  # Identify dataset to load
  uid <- str_detect(files, data_name)

  # Identify extensions
  # ext <- last(str_split(files[uid], "\\.")[[1]])

  # Load according to extension type
  ## ---------------------------------------------
  ## GEOJSON
  assign(x = data_name,
         value = st_read(files[uid], quiet = TRUE),
         envir = globalenv())
}

# =================================================================
#' @rdname load
#' @export
basemap <- function(basemap_name) {
  files <- dir('./data/data-basemap', full.names = TRUE)

  # Identify dataset to load
  uid <- str_detect(files, basemap_name)

  # Identify extensions
  ext <- last(str_split(files[uid], "\\.")[[1]])

  # Load according to extension type
  ## ---------------------------------------------
  ## GEOJSON
  if (ext == "geojson") {
    assign(x = basemap_name,
           value = st_read(files[uid], quiet = TRUE),
           envir = globalenv())
  }
}
