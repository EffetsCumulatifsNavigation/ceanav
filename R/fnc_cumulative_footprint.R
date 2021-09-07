#' Cumulative footprint
#'
#' Function to evaluate cumulative footprint of stressors
#'
#' @param dat data.frame with stressors intensity
#' @param normaliser logical, whether to normalize footprint
#'
#' @keywords cumulative footprint
#'
#' @export
#'
#' @details This function uses fisheries data to evaluate the intensity of fishing over a regular grid. If `normaliser` is TRUE, then the cumulative footprint is divided by the number of cumulated stressors.
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
