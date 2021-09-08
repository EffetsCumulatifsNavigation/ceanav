#' Cumulative footprint
#'
#' Function to evaluate cumulative footprint of stressors and valued components
#'
#' @param dat data.frame with stressors intensity or valued components
#' @param normaliser logical, whether to normalize footprint
#'
#' @keywords cumulative footprint
#'
#' @export
#'
#' @details 
#'

cumulativeFootprint <- function(dat, normaliser = FALSE) {

  # -----
  nc <- ncol(dat)

  # -----
  if (!normaliser & nc > 1) {
    dat <- rowSums(dat, na.rm = TRUE)
  } else if (normaliser & nc > 1) {
    dat <- rowSums(dat, na.rm = TRUE) / nc
  } else {
    dat <- as.numeric(dat[,1])
  }

  # -----
  return(dat)
}
