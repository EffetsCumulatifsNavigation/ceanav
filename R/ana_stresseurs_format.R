#' Transformation des données sur les stresseurs environnementaux
#'
#' Transformation des données sur les stresseurs environnementaux
#'
#' @keywords transformation
#' @keywords stresseurs
#'
#' @export
#'
#' @details Cette fonction effectue transforme les données sur les stresseurs environnementaux pour les préparer pour l'évaluation des effets cumualatifs
#'

ana_stresseurs_format <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Notes
  # ------------------------------------
  #
  # Pour combiner les données de stresseurs et obtenir une évaluation des effets
  # cumulatifs relatives, il est nécessaire de normaliser leur intensité entre
  # 0 et 1 pour s'assurer que les données soient comparables d'une stresseur
  # à l'autre malgré des unités brutes différentes.
  #
  # On débute par une tranformation logarithmique pour les données dont la
  # distribution n'est pas normale, puis on normalise les données entre 0 et 1
  # en utilisant le 99e percentile comme valeur maximale pour la normalisation.
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #

  # -----
  load_output("stresseurs_raw")
  data(grid1p)

  # ------------------------------------------------
  # Log transformation

  # -----
  # Cette étape nécessite une visualisation initiale des données afin d'identifier
  # quelles nécessitent une transformation logarithmique
  # hist_all <- function(x) {
  #   x <- st_drop_geometry(x)
  #   j <- ceiling(sqrt(ncol(x)))
  #   par(mfrow = c(j,j))
  #   for(i in 1:ncol(x)) {
  #     hist(x[,i], main = colnames(x)[i])
  #   }
  # }
  # hist_all(stresseurs_raw)

  # -----
  dr <- st_drop_geometry(stresseurs_raw) %>%
        apply(2, function(x) log(x + 1))


  # ------------------------------------------------
  # Normalisation

  # -----
  # Scale drivers between 0 and 1 using the 99th quantile
  quantNorm <- function(x) {
    id <- x != 0
    x <- x / quantile(x[id], probs = .99, na.rm = T)
    x[x > 1] <- 1
    x[x < 0] <- 0
    x
  }

  # -----
  dr <- apply(dr, 2, quantNorm)

  # -----
  stresseurs_format <- cbind(grid1p, dr)
  # hist_all(stresseurs_format)



  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # -----
  st_write(obj = stresseurs_format,
           dsn = "./data/data-output/stresseurs_format.geojson",
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
