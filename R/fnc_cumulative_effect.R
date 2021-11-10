#' Cumulative effects
#'
#' Function to evaluate cumulative effects of stressors on valued components
#'
#' @param stress matrix with stressors intensity
#' @param valued matrix with presence or amount of valued components
#' @param vulnerability matrix with vulnerability of valued components to stressors. Names used in stress and valued matrices must be the same as that found in vulnerability matrix
#'
#' @keywords cumulative effects
#'
#' @export
#'
#' @details
#'

cumulativeEffects <- function(stress, valued, vulnerability) {

  stress <- matrix(nrow = 100, ncol = 10, dimnanes = list(c(), paste0("stress", 1:10)))
  valued <- matrix(nrow = 100, ncol = 10, dimnanes = list(c(), paste0("valued", 1:10)))

  # -----
  st <- colnames(stress)
  cv <- colnames(valued)

  # -----
  nst <- ncol(stress)
  ncv <- ncol(valued)

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
