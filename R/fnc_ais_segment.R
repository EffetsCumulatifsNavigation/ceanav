#' Create navigation tracks from AIS data.
#'
#' A function to create navigation tracks from AIS data based on maximum distance and maximum time interval between observations.
#'
#' @param ais csv of AIS positional data
#' @param time_interval maximum time interval allowed between points, in hours
#' @param distance maximum distance allowed between points, in meters
#' @keywords AIS
#' @keywords navigation
#'
#' @export
#'
#' @details This function takes basic `.csv` files containing AIS observation data and creates line segments divided by individual boats (`MMSI` field) and by a user defined maximum time internal and distance between observations.
#'
#'
#' @examples
#' # Example 1:
#' ais <- ais_segment(df)

ais_segment <- function(df, time_interval = (300/60), distance = (50 * 1.61 * 1000)) {
  # =~-~=~-~=~-~=~-~=~-~=~-~= #
  # CSV AS SF POINTS DATA
  # =~-~=~-~=~-~=~-~=~-~=~-~= #
  ais <- df %>%
         st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
         st_transform(32198)


  # =~-~=~-~=~-~=~-~=~-~=~-~= #
  # TIME INTERALS
  # =~-~=~-~=~-~=~-~=~-~=~-~= #
  # Create `POSIXct` column from `Date_UTC`
  # The AIS format for date and time: `20051224_163958`
  # The format has to be transformed to the form: `2005-12-24 16:39:58`
  yr <- substr(ais$Date_UTC, 1, 4)
  mt <- substr(ais$Date_UTC, 5, 6)
  dy <- substr(ais$Date_UTC, 7, 8)
  hr <- substr(ais$Date_UTC, 10, 11)
  mn <- substr(ais$Date_UTC, 12, 13)
  sc <- substr(ais$Date_UTC, 14, 15)
  POSIXct <- paste0(yr, '-', mt, '-', dy, ' ', hr, ':', mn, ':', sc)

  # Add time to AIS data
  ais$POSIXct <- as.POSIXct(POSIXct,  format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")

  # Order data by ship and date/time
  ais <- arrange(ais, MMSI, POSIXct)

  # Time interval
  ais$time_int <- c(0,difftime(ais$POSIXct[-1], ais$POSIXct[-nrow(ais)], units = "hours"))


  # =~-~=~-~=~-~=~-~=~-~=~-~= #
  # DISTANCE
  # =~-~=~-~=~-~=~-~=~-~=~-~= #
  # Distance between points
  ais$distance_int <- c(0,st_distance(ais[-1, ], ais[-nrow(ais), ], by_element = TRUE))



  # =~-~=~-~=~-~=~-~=~-~=~-~= #
  # SEGMENTS
  # =~-~=~-~=~-~=~-~=~-~=~-~= #
  # Identify individual tracks according to `time_interval` and `distance` selected
  ais <- ais %>%
         mutate(time_group = ifelse(ais$time_int > time_interval, TRUE, FALSE),
                distance_group = ifelse(ais$distance_int > distance, TRUE, FALSE),
                group = time_group + distance_group) %>%
         mutate(group = cumsum(group)+1)

  # Create line segments
  ais %>%
  group_by(MMSI, group) %>%
  arrange(POSIXct) %>%
  summarize(n = n(), .groups = "drop", do_union = FALSE,
            Date_UTC_start = min(POSIXct),
            Date_UTC_end = max(POSIXct)) %>% # do_union = FALSE is important to preserve order
  filter(n > 2) %>%
  st_cast("LINESTRING")
}
