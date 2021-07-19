#' Update metadata and contacts
#'
#' The function updates the existing metadata file based on individual metadata files in `data-metadata`
#'
#' @param type type of metadata, one of `raw` or `integrated`
#' @keywords metadata
#' @keywords contact
#'
#' @rdname update
#'
#' @export
#'
#' @details These functions take '.yml' files in specified folder to generate an individual table containing information from all `.yml` files
#'
#'
#' @examples
#' # Metadata
#' update_metadata("raw")
#'
#' # Contacts
#' update_contact()

update_metadata <- function(type) {
  # Folder
  folder <- './data/data-metadata/'

  # Identify files and paths of files in `folder`
  paths <- dir(folder, full.names = TRUE, pattern = '.yml')
  uid <- stringr::str_detect(paths, "int")

  if (type == "raw") {
    paths <- paths[!uid]
  } else if (type == "integrated") {
    paths <- paths[uid]
  } else {
    stop("Wrong metadata type. See documentation for more information.")
  }

  # Names of files
  dat <- list()
  for(i in 1:length(paths)) {
    dat[[i]] <- read_yaml(paths[i]) %>%
                as.data.frame()
  }

  # Single data.frame
  dat <- bind_rows(dat)

  # Export
  metadata <- dat

  if (type == "raw") {
    write.csv(metadata, file = './data/data-metadata/metadata.csv', row.names = FALSE)
    save(metadata, file = './data/data_metadata.RData')
  }

  if (type == "integrated") {
    write.csv(metadata, file = './data/data-metadata/metadata_integrated.csv', row.names = FALSE)
    save(metadata, file = './data/integrated_metadata.RData')
  }

}

# =================================================================
#' @rdname update
#' @export
update_contact <- function() {
  # Folder
  folder <- './data/data-contact/'

  # Identify files and paths of files in `folder`
  paths <- dir(folder, full.names = TRUE, pattern = '.yml')

  # Names of files
  dat <- list()
  for(i in 1:length(paths)) {
    dat[[i]] <- read_yaml(paths[i]) %>%
                as.data.frame()
  }

  # Single data.frame
  dat <- bind_rows(dat)

  # Export
  contact <- dat
  write.csv(contact, file = './data/data-contact/contact.csv', row.names = FALSE)
  save(contact, file = './data/data_contact.RData')
}
