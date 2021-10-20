#' Lissage Gaussien de 0.2o
#'
#' @param dat observation points as sf object
#'
#' @keywords lissage gaussien
#' @keywords kernel
#'
#' @export
#'

lissage <- function(dat) {
  dat <- dat

  # -----
  coords <- st_centroid(dat) %>%
            st_coordinates() %>%
            as.data.frame() %>%
            rename(x = X, y = Y)

  # -----
  dat <- st_drop_geometry(dat) %>%
         cbind(coords) %>%
         select(x, y, normalisation)

  # -----
  dat <- btb::kernelSmoothing(dfObservations = dat,
                              sEPSG = "32198",
                              iCellSize = 1000,
                              iBandwidth = 22200,
                              # iBandwidth = 5000,
                              vQuantiles = NULL,
                              dfCentroids = round(coords,0))

  # -----
  dat <- dat$normalisation

  # -----
  dat
}
