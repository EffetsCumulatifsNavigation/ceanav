#' Effets cumulatifs par km2 sur les régions administratives du Saint-Laurent
#'
#' Évaluation des effets cumulatifs par km^2 des stresseurs environnementaux sur les composantes valorisées dans les régions administratives du Saint-Laurent
#'
#' @keywords effets cumulatifs
#' @keywords stresseurs
#' @keywords composantes valorisées
#'
#' @export
#'
#' @details Cette fonction effectue une partie des analyses du projet d'évaluation des effets cumulatifs
#'

ana_cumulative_effects_region_km2 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Notes
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # -----
  load_format("data0065")
  data(grid1p)

  # -----
  # Identify which regions intersect with which grid cells
  uid <- list()
  for(i in 1:nrow(data0065)) {
    uid[[i]] <- st_intersects(data0065[i,], grid1p) %>%
    unlist()
  }

  # -----
  # Identify which regions intersect with any grid cell
  iid <- logical(length(uid))
  for(i in 1:nrow(data0065)) iid[i] <- length(uid[[i]]) > 10

  # -----
  # Keep only regions with data
  data0065 <- data0065[iid, ]
  uid <- uid[iid]

  load_output("composantes_valorisees_format")
  load_output("stresseurs_format")
  cv <- st_drop_geometry(composantes_valorisees_format)
  st <- st_drop_geometry(stresseurs_format)

  # -----
  cekm <- matrix(ncol = ncol(st)+3, nrow = nrow(data0065), data = 0,
                 dimnames = list(c(), c("region","area","cea",colnames(st)))) %>%
          data.frame()

  # -----
  cekm$region <- data0065$RES_NM_REG

  # -----
  for(i in 1:length(uid)) cekm$area[i] <- length(uid[[i]])

  # -----
  # Now get metrics for each stressor
  folder <- "data/data-output/cea_stresseur/"
  files <- dir(folder)
  stNames <- gsub("cea_", "", files) %>% gsub(".csv","",.)
  for(i in 1:length(files)) {
    # ---
    dat <- read.csv(glue("{folder}{files[i]}"))

    # ---
    # Iterate over regions
    # TODO: Remove na.rm = TRUE
    for(j in 1:nrow(cekm)) cekm[j, stNames[i]] <- sum(dat[uid[[j]], ], na.rm = TRUE) / cekm$area[j]
  }

  # ---
  for(i in 1:nrow(cekm)) cekm$cea[i] <- round(sum(cekm[i, stNames]), 6)

  # # -----
  # # Now get metrics for each valued component
  # folder <- "data/data-output/cea_composante_valorisee/"
  # files <- dir(folder)
  # cvNames <- gsub("cea_", "", files) %>% gsub(".csv","",.)
  # for(i in 1:length(files)) {
  #   # ---
  #   uid <- cekm$cv == cvNames[i]
  #
  #   # ---
  #   dat <- read.csv(glue("{folder}{files[i]}")) %>%
  #          colSums(na.rm = TRUE) / cekm$area[uid]
  #
  #   # ---
  #   cekm[uid, names(dat)] <- round(dat, 6)
  #
  #   # ---
  #   cekm$cea[uid] <- round(sum(dat), 6)
  # }

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  write.csv(cekm,
            file = "./data/data-output/cumulative_effects_region_km2.csv",
            row.names = FALSE)
  # ------------------------------------------------------------------------- #}


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Clean global environment
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  clean()
  # ------------------------------------------------------------------------- #}
}
