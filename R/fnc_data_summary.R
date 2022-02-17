#' Tableau récapitulatif des données
#'
#' Fonction utilisée pour générer un tableau récapitulatif des données présentées dans le rapport.
#'
#' @keywords tableau récapitulatif
#'
#' @export
#'
#' @details Cette fonction génère un tableau récapitulatif des données considérées pour l'évaluation
#'
#' @examples
#' fnc_data_summary()

  data_summary <- function() {  
  # Metadata files 
  files <- dir("./data/data-metadata", pattern = "data0", full.names = TRUE)
  nF <- length(files)
  
  # -----
  meta <- data.frame(id = character(nF), 
                     name = character(nF),
                     pr = character(nF),
                     temp = character(nF),
                     disp = character(nF),
                     src = character(nF))

  # -----
  for(i in 1:length(files)) {
    dat <<- read_yaml(files[i])
    meta$id[i] <- dat$data_description$id
    meta$name[i] <- rep_hyperlien(dat$data_description$name, dat$data_description$url)
    meta$pr[i] <- paste(dat$data_description$contact_id, collapse = ", ")
    meta$disp[i] <- dat$data_description$availability
    meta$src[i] <- paste(glue("@{dat$data_description$citekey}"), collapse = "; ")

    # Temporal
    tm1 <- dat$data_description$temporal_start
    tm2 <- dat$data_description$temporal_end
    if (!is.null(tm1)) {
      meta$temp[i] <- ifelse(tm1 == tm2, tm1, glue("{tm1} - {tm2}"))
    } else {
      meta$temp[i] <- NA
    }
  }

  # Integrated 
  files <- c(dir("./data/data-metadata", pattern = "int_cv", full.names = TRUE), 
             dir("./data/data-metadata", pattern = "int_st", full.names = TRUE))
  # -----
  l <- list()
  for(i in 1:length(files)) {
    dat <<- read_yaml(files[i])
    l[[i]] <- data.frame(id = dat$rawData, ana = "X")
  }
  
  # -----
  l <- bind_rows(l) %>%
       unique()
       
  # -----
  meta <- left_join(meta, l, by = "id") %>%
          select(id,name,src,ana,temp, pr,disp)
  
  # -----
  write.csv(meta, "./data/data-metadata/data_summary.csv", row.names = FALSE)
}
