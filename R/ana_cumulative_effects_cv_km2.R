#' Effets cumulatifs par km2
#'
#' Évaluation des effets cumulatifs par km^2 des stresseurs environnementaux sur les composantes valorisées
#'
#' @keywords effets cumulatifs
#' @keywords stresseurs
#' @keywords composantes valorisées
#'
#' @export
#'
#' @details Cette fonction effectue une partie des analyses du projet d'évaluation des effets cumulatifs
#'

ana_cumulative_effects_cv_km2 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Notes
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # -----
  load_output("composantes_valorisees_format")
  load_output("stresseurs_format")
  cv <- st_drop_geometry(composantes_valorisees_format)
  st <- st_drop_geometry(stresseurs_format)

  # Adjust marine mammals data, currently not in presence-absence
  uid <- str_detect(colnames(cv), "mammiferes_marins")
  cv[, uid] <- apply(cv[, uid], 2, function(x) ifelse(x > 0, 1, NA))

  # -----
  cekm <- matrix(ncol = ncol(st)+3, nrow = ncol(cv), data = 0,
                 dimnames = list(c(), c("cv","area","cea",colnames(st)))) %>%
          data.frame()

  # -----
  cekm$cv <- colnames(cv)

  # -----
  cekm$area <- colSums(cv, na.rm = TRUE)

  # -----
  # Now get metrics for each valued component
  folder <- "data/data-output/cea_composante_valorisee/"
  files <- dir(folder)
  cvNames <- gsub("cea_", "", files) %>% gsub(".csv","",.)
  for(i in 1:length(files)) {
    # ---
    uid <- cekm$cv == cvNames[i]

    # ---
    dat <- read.csv(glue("{folder}{files[i]}")) %>%
           colSums(na.rm = TRUE) / cekm$area[uid]

    # ---
    cekm[uid, names(dat)] <- round(dat, 6)

    # ---
    cekm$cea[uid] <- round(sum(dat), 6)
  }

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  write.csv(cekm,
            file = "./data/data-output/cumulative_effects_cv_km2.csv",
            row.names = FALSE)
  # ------------------------------------------------------------------------- #}


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Clean global environment
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  clean()
  # ------------------------------------------------------------------------- #}
}
