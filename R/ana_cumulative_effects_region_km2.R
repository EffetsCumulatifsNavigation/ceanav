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

  # Add dfs for individuals cvs
  cekm_berge <- cekm_habitat <- cekm_mammiferes_marins <- cekm_site <- cekm
  cv_meta <- read.csv("./data/data-metadata/metadata_composantes_valorisees.csv")
  berge <- paste0("berge_",cv_meta$accronyme[cv_meta$comp_val == "berge"])
  habitat <- paste0("habitat_",cv_meta$accronyme[cv_meta$comp_val == "habitat"])
  mammiferes_marins <- paste0("mammiferes_marins_",cv_meta$accronyme[cv_meta$comp_val == "mammiferes_marins"])
  site <- paste0("site_",cv_meta$accronyme[cv_meta$comp_val == "site"])

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
    for(j in 1:nrow(cekm)) {
      cekm[j, stNames[i]] <- sum(dat[uid[[j]], ], na.rm = TRUE) / cekm$area[j]
      cekm_berge[j, stNames[i]] <- sum(dat[uid[[j]], berge], na.rm = TRUE) / cekm$area[j]
      cekm_habitat[j, stNames[i]] <- sum(dat[uid[[j]], habitat], na.rm = TRUE) / cekm$area[j]
      cekm_mammiferes_marins[j, stNames[i]] <- sum(dat[uid[[j]], mammiferes_marins], na.rm = TRUE) / cekm$area[j]
      cekm_site[j, stNames[i]] <- sum(dat[uid[[j]], site], na.rm = TRUE) / cekm$area[j]
    }
  }

  # ---
  for(i in 1:nrow(cekm)) {
    cekm$cea[i] <- round(sum(cekm[i, stNames]), 6)
    cekm_berge$cea[i] <- round(sum(cekm_berge[i, stNames]), 6)
    cekm_habitat$cea[i] <- round(sum(cekm_habitat[i, stNames]), 6)
    cekm_mammiferes_marins$cea[i] <- round(sum(cekm_mammiferes_marins[i, stNames]), 6)
    cekm_site$cea[i] <- round(sum(cekm_site[i, stNames]), 6)
  }



  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  write.csv(cekm,
            file = "./data/data-output/cumulative_effects_region_km2.csv",
            row.names = FALSE)

  write.csv(cekm_berge,
            file = "./data/data-output/cumulative_effects_region_km2_berge.csv",
            row.names = FALSE)

  write.csv(cekm_habitat,
            file = "./data/data-output/cumulative_effects_region_km_habitat.csv",
            row.names = FALSE)

  write.csv(cekm_mammiferes_marins,
            file = "./data/data-output/cumulative_effects_region_km2_mammiferes_marins.csv",
            row.names = FALSE)

  write.csv(cekm_site,
            file = "./data/data-output/cumulative_effects_region_km2_site.csv",
            row.names = FALSE)
  # ------------------------------------------------------------------------- #}


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Clean global environment
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  clean()
  # ------------------------------------------------------------------------- #}
}
