#' Tableau récapitulatif des données
#'
#' Fonction utilisée pour générer un tableau récapitulatif des données présentées dans le rapport.
#'
#' @keywords tableau récapitulatif
#'
#' @export
#'
#' @details Cette fonction permet de générer le tableau récapitulatif des données utilisées pour le rapport
#'
#' @examples
#' rep_annexe_data_summary()

rep_annexe_data_summary <- function() {  
  # Metadata files 
  files <- dir("./data/data-metadata", pattern = "data0", full.names = TRUE)
  nF <- length(files)
  
  # -----
  meta <- data.frame(id = character(nF), 
                     name = character(nF),
                     pr = character(nF),
                     disp = character(nF),
                     src = character(nF))

  # -----
  for(i in 1:length(files)) {
    dat <<- read_yaml(files[i])
    meta$id[i] <- dat$data_description$id
    meta$name[i] <- rep_hyperlien(dat$data_description$name, dat$data_description$url)
    meta$pr[i] <- paste(dat$data_description$contact_id, collapse = ", ")
    meta$disp[i] <- dat$data_description$availability
    # meta$src[i] <- paste(dat$data_description$source, collapse = ", ")
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
          select(id,name,ana,pr,disp,src)
  
  # -----
  write.csv(meta, "./data/data-metadata/data_summary.csv", row.names = FALSE)
}
