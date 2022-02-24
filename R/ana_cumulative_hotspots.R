#' Hotspots des stressurs environnementaux
#'
#' Évaluation des hotspots cumulés des stresseurs environnementaux considérés
#'
#' @keywords empreinte cumulée
#' @keywords stresseurs
#'
#' @export
#'
#' @details Cette fonction effectue une partie des analyses du projet d'évaluation des effets cumulatifs
#'

ana_cumulative_hotspots <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Notes
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #

  # -----
  data(grid1p)

  # -----
  load_output("stresseurs_format")
  dr <- st_drop_geometry(stresseurs_format)

  # Empty matrix to store results
  hot <- matrix(ncol = ncol(dr),
                nrow = nrow(dr),
                data = NA,
                dimnames = list(c(), colnames(dr)))

  # Evaluate hotspots
  for(i in 1:ncol(hot)) {
    # ID = 0
    id0 <- dr[, i] > 0
    # 80th quantile threshold
    th <- quantile(dr[id0, i], .8, na.rm = T)
    # Values over hotspot threshold
    hot[, i] <- dr[, i] > th
  }

  # Hotspots
  cumulative_hotspots <- rowSums(hot, na.rm = TRUE)
  cumulative_hotspots[cumulative_hotspots == 0] <- NA

  # Spatial dataset 
  cumulative_hotspots <- cbind(grid1p, cumulative_hotspots)
  # ------------------------------------------------------------------------- #}
  

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # -----
  st_write(obj = cumulative_hotspots,
           dsn = "./data/data-output/cumulative_hotspots.geojson",
           delete_dsn = TRUE,
           quiet = TRUE)
  # ------------------------------------------------------------------------- #}

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Clean global environment
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  clean()
  # ------------------------------------------------------------------------- #}
}
