#' Data 0032 : Kahnawà:ke - Local input on valued components
#'
#' Kahnawà:ke – Local input on valued components identified through the Cumulative Effects of Marine Shipping initiative for the St. Lawrence River
#'
#' @keywords kahnawà:ke
#' @keywords site archéologique
#' @keywords site site culturel
#' @keywords chasse
#' @keywords pêche
#' @keywords végétation
#' @keywords site d'importance
#' @keywords composante valorisée
#'
#' @source Mohawk Council of Kahnawà:ke (2021) Kahnawà:ke – Local input on valued components identified through the Cumulative Effects of Marine Shipping initiative for the St. Lawrence River
#'
#' @export
#'
#' @details Cette fonction formatte les données
#'

get_data0032 <- function() {

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # WARNING:
  message("Les données brutes sont soumis à une entente de partage de données et doivent être détruites au terme du projet. L'équipe de recherche s'engage à envoyer une attestation de destruction de données. ")

  # DISCLAIMER:
  # The Mohawk Council of Kahnawà:ke ("MCK") has gathered some information from
  # community members concerning the value and use of the St. Lawrence River in
  # the vicinity of Kahnawà:ke. These comments have been organized according to
  # the valued components identified for the fluvial section of the river through
  # the Cumulative Effects of Marine Shipping by the Transport Canada team in
  # collaboration with project partners. Therefore, the information provided is
  # based on a limited consultation of community members, with the intent of
  # responding to the specific parameters of this Transport Canada study only
  # and should not be interpreted as a comprehensive study of Mohawk traditional
  # land use or rights throughout the study area. The information provided should
  # also not be used to replace engagement, including the gathering of traditional
  # land use information within the context of any other future study or project.
  # In addition, our participation in this study is without prejudice to the
  # exercise of our jurisdictional rights to our traditional territory to which
  # our stewardship rights and responsibilities apply and that are based on the
  # application of a broader ecosystem-based approach. Therefore, the identification
  # of specific areas of concern or value, including the identification of specific
  # fish species or archaeological sites of interest is to be understood within the
  # context of these broader rights and responsibilities.
  message("The Mohawk Council of Kahnawà:ke (MCK) has gathered some information from community members concerning the value and use of the St. Lawrence River in the vicinity of Kahnawà:ke. These comments have been organized according to the valued components identified for the fluvial section of the river through the Cumulative Effects of Marine Shipping by the Transport Canada team in collaboration with project partners. Therefore, the information provided is based on a limited consultation of community members, with the intent of responding to the specific parameters of this Transport Canada study only and should not be interpreted as a comprehensive study of Mohawk traditional land use or rights throughout the study area. The information provided should also not be used to replace engagement, including the gathering of traditional land use information within the context of any other future study or project. In addition, our participation in this study is without prejudice to the exercise of our jurisdictional rights to our traditional territory to which our stewardship rights and responsibilities apply and that are based on the application of a broader ecosystem-based approach. Therefore, the identification of specific areas of concern or value, including the identification of specific fish species or archaeological sites of interest is to be understood within the context of these broader rights and responsibilities.")

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0032-kahnawake/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # WARNING: Data transfered physically, no cloud access currently
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import and format data
  # ----------------------------------------
  # Import data
  data0032 <- st_read(paste0(folder, "Kahnawake Cumulative Effects of Marine Shipping/Kahnawake Cumulative Effects of Marine Shipping.shp"), quiet = TRUE) %>%
              select(-uuid, -id) %>%
              rename(description = descriptio)

  # Add certain names
  data0032$description[c(3,9,11,12,18)] <- "Waterfowl hunting"
  data0032$category[c(4,17,19,25)] <- "Near Shore Fishing"

  # Transform projection
  data0032 <- st_transform(data0032, crs = global_parameters()$crs)
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0032,
           dsn = "./data/data-format/data0032-kahnawake.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #
}
