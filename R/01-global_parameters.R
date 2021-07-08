#' Loads global parameters
#'
#' The function loads the global parameters for this project
#'
#' @export
#'
#' @examples
#' # Metadata
#' global_parameters()

global_parameters <- function() {
  ## ---------------------------------------------
  ## Global parameters stored as YAML
  global_param <<- read_yaml("./data/data-param/global_parameters.yml")
}
