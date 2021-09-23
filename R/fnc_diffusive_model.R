#' Diffusive model
#'
#' Function to evaluate the influence area of a stressor using a passive diffusive model
#'
#' @param dat sf object with points characterizing stressor intensity
#' @param field character, name of field containing stresseor intensity
#' @param threshold numeric, minimum threshold in percent of the global maximum at which to stop the diffusive model
#' @param globalmaximum global maximum value of the stressor in the study area
#' @param decay percent by which the value of the stressor is reduced when diffusing to the adjacent cells
#' @param distance numeric, distance beyond which the stressor is considered to have no influence. Units are in the units of the spatial object provided.
#' @param increment numeric, distance between successive steps in the model. Units are in the units of the spatial object provided.
#'
#' @keywords cumulative footprint
#'
#' @export
#'
#' @details
#'

diffusive <- function(dat, field, threshold, globalmaximum, decay, distance, increment) {
  # -----
  res <- list()

  # -----
  val <- list()
  for(i in 1:nrow(dat)) {
    val[[i]] <- seq(from = dat[i, field, drop = TRUE],
                    to = 6 * (threshold/100),
                    by = -(decay/100))

    # Succesive buffers
    buf <- seq(from = increment,
               to = increment*length(val[[i]]),
               by = increment)

    # Solid buffers
    temp <- select(dat, geometry)
    circles <- list()
    for(j in 1:length(buf)) circles[[j]] <- st_buffer(temp[i,], buf[j])

    # Rings
    rings <- list()
    rings[[1]] <- circles[[1]]
    for(j in 2:length(circles)) {
      rings[[j]] <- st_difference(circles[[j]], circles[[(j-1)]])
    }

    # Add values
    res[[i]] <- bind_rows(rings) %>%
                mutate(intensity = val[[i]])
  }

  # -----
  res
}
