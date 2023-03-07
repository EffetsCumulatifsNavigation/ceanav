#' Figures données intégrées
#'
#' Fonctions pour générer des figures pour les données intégrées
#'
#' @keywords figure
#'
#' @rdname fig_integrated
#'
#' @export
#'
#' @examples
#' # Figure for specific dataset
#' fig_integrated(data_id = "mammiferes_marins")
#'
#' # Render all figures for available data
#' fig_integrated_all()
#'
#' # Update figures based on available data
#' fig_integrated_update()


fig_integrated <- function(data_id, lang = "fr") {
  # Integrated data
  load_integrated(data_id)
  dat <- get(data_id)
  
  # Global parameters 
  global_parameters()

  # Metadata
  meta_id <- data_id
  st <- c("ancrage","deversement","dragage","naufrage","navigation","peche_commerciale","rejet","pollution_maritime")
  cv <- c("berge","habitat","mammiferes_marins","site")
  if (meta_id %in% st) meta_id <- glue("int_st_{meta_id}")
  if (meta_id %in% cv) meta_id <- glue("int_cv_{meta_id}")
  meta <- load_metadata(meta_id)

  # Titles
  nm <- meta$dataDescription$categories$accronyme
  ref <- meta$dataDescription$categories$source
  na <- meta$dataDescription$categories$zonesNA
  if (lang == "fr") {
    main <- meta$title
    tp <- meta$dataDescription$categories$type
    subt <- meta$dataDescription$categories$francais  
    if (meta$dataDescription$units == "in_categories") {
      un <- meta$dataDescription$categories$units
    } else {
      un <- rep(meta$dataDescription$units, length(nm))      
    }
  } else if (lang == "en") {
    main <- meta$title_en
    tp <- meta$dataDescription$categories$type_en
    subt <- meta$dataDescription$categories$english
    if (meta$dataDescription$units_en == "in_categories") {
      un <- meta$dataDescription$categories$units_en
    } else {
      un <- rep(meta$dataDescription$units_en, length(nm))      
    }
  }

  for(i in 1:length(nm)) {
    if (lang == "fr") {
      output <- glue('./figures/figures-integrated/{data_id}-{nm[i]}.png')
    } else if (lang == "en") {
      output <- glue('./figures_en/figures-integrated/{data_id}-{nm[i]}.png')
    }
    # pdf(glue('./figures/figures-format/{data_id}.pdf'), width = 7, height = 5, pointsize = 12)
    png(output, res = global_param$figures$resolution, width = global_param$figures$width, height = global_param$figures$height, units = "mm", pointsize = global_param$figures$pointsize)
    plot_ceanav(
      dat[, nm[i]],
      main = main,
      type = tp[i],
      subtitle = subt[i],
      unit_data = un[i],
      references = ref[i],
      zones_NA = na[i],
      lang = lang
    )
    dev.off()
  }
}


#' @rdname fig_integrated
#' @aliases fig_integrated_all
#' @export
fig_integrated_all <- function(lang = "fr") {
  dataname <- dir("./data/data-integrated/") %>%
              gsub("cv_","",.) %>%
              gsub("st_","",.) %>%
              gsub(".geojson","",.)
  dataname <- dataname[dataname != "vulnerability.csv"]
  for(i in dataname) fig_integrated(data_id = i, lang = lang)
}
