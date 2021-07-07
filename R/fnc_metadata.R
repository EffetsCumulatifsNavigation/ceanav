#' Update metadata and contacts
#'
#' The function updates the existing metadata file based on individual metadata files in `data-metadata`
#'
#' @param type type of metadate, one of `metadata` or `contact`
#' @keywords metadata
#' @keywords contact
#'
#' @export
#'
#' @details These functions take '.yml' files in specified folder to generate an individual table containing information from all `.yml` files
#'
#'
#' @examples
#' # Metadata
#' ceanav_metadata('metadata')
#'
#' # Contacts
#' ceanav_metadata('contact')

ceanav_update_metadata <- function(type) {

  # Folder
  if (type == "metadata") folder <- './data/data-metadata/'
  if (type == "contact") folder <- './data/data-contact/'

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
  if (type == "metadata") {
    metadata <- dat
    write.csv(metadata, file = './data/data-metadata/metadata.csv', row.names = FALSE)
    save(metadata, file = './data/data_metadata.RData')
  }

  if (type == "contact") {
    contact <- dat
    write.csv(contact, file = './data/data-contact/contact.csv', row.names = FALSE)
    save(contact, file = './data/data_contact.RData')
  }
}
