#' Data 0085 : Seasonal distribution and concentration of four baleen whale species in the St. Lawrence Estuary
#'
#' The Estuary and the Gulf of St. Lawrence are feeding grounds for several North Atlantic whales, including minke, humpback, endangered blue whales and fin whales which are designated by COSEWIC as Special Concern. Observations of these species collected during aerial and boat surveys conducted between 1995 to 2017 were used to produce maps presenting raw sightings, relative densities from kernel analyses and predicted relative probability of occurrence from spatial distribution modelling.
#'
#' @export
#'
#' @details Cette fonction formatte les données
#'

get_data0085 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0085-large_whales_dfo/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)
  # WARNING: Data transfered physically, no cloud access currently
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import and format data
  # ----------------------------------------
  dat <- dir(folder, pattern = ".tif$", full.names = TRUE) |>
         lapply(stars::read_stars)
  dat <- lapply(dat, stars::st_warp, dest = dat[[1]])
  dat[[4]] <- stars::st_warp(dat[[4]], dest = dat[[1]])
  nm <- lapply(dat, names) |> unlist()
  
  data0085 <- c(
    dat[[1]],
    dat[[2]],
    dat[[3]],
    dat[[4]],
    along = "z"
  )
  data0085 <- st_set_dimensions(data0085, 3, values = nm)
  
  message("Id 0085: Dataset is not transformed to project coordinate system since it is a raster and the transformation makes it curvilinear. Make sure that the integration script transforms the data to avoid errors.")
  # _________________________________________________________________________ #    

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  write_stars(data0085,
              dsn = "./data/data-format/data0085-large_whales_dfo.tif",
              driver = "GTiff", delete_dsn = TRUE)
  # _________________________________________________________________________ #
}
