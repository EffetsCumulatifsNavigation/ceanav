#' Effets cumulatifs
#'
#' Évaluation des effets cumulatives des stresseurs environnementaux sur les  composantes valorisées
#'
#' @keywords effets cumulatifs
#' @keywords stresseurs
#' @keywords composantes valorisées
#'
#' @export
#'
#' @details Cette fonction effectue une partie des analyses du projet d'évaluation des effets cumulatifs
#'

ana_cumulative_effects <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Notes
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #

  # -----
  data(grid1p)

  # Load data
  load_output("stresseurs_format")
  load_output("composantes_valorisees_format")
  load_integrated("vulnerability")

  # Transform sf to data.frame
  stresseurs_format <- st_drop_geometry(stresseurs_format)
  composantes_valorisees_format <- st_drop_geometry(composantes_valorisees_format)

  # NA to 0
  repNA <- function(x) ifelse(is.na(x), 0, x)
  stresseurs_format <- apply(stresseurs_format, 2 , repNA)
  composantes_valorisees_format <- apply(composantes_valorisees_format, 2 , repNA)

  # names
  st <- colnames(stresseurs_format)
  stV <- colnames(vulnerability)
  cv <- colnames(composantes_valorisees_format)
  cvV <- rownames(vulnerability)

  # Check that all stressors and valued components are in the tables used
  cond <- all(stV %in% st) &
          all(cvV %in% cv) &
          all(st %in% stV) &
          all(cv %in% cvV)

  # Stop if all variables names are not in all datasets
  if (!cond) {
    stop("Les noms de lignes et de colonnes dans le fichier de vulnerabilité doivent être dans les données de stresseurs et de composantes valorisées, et les stresseurs et composantes valorisées doivent toutes être dans la matrice de vulnérabilité")
  }

  # Make sure that stressors and VCs are in the same order in spatial data and vulnerability data
  vulnerability <- vulnerability[, colnames(stresseurs_format)]
  vulnerability <- vulnerability[colnames(composantes_valorisees_format), ]

  # Evaluate effects (longer, this one exports all individual effects for later use)
  ce <- cumulativeEffects(stress = stresseurs_format,
                    valued = composantes_valorisees_format,
                    vulnerability = vulnerability)

  # Effets sur les berges
  vc <- str_detect(colnames(composantes_valorisees_format), "berge")
  berge <- cumulativeEffects(stress = stresseurs_format,
                             valued = composantes_valorisees_format[, vc],
                             vulnerability = vulnerability[vc, ],
                             individual_cea = FALSE)

  # Effets sur les habitats
  vc <- str_detect(colnames(composantes_valorisees_format), "habitat")
  hab <- cumulativeEffects(stress = stresseurs_format,
                             valued = composantes_valorisees_format[, vc],
                             vulnerability = vulnerability[vc, ],
                             individual_cea = FALSE)


  # Effets sur les mammifères marins
  vc <- str_detect(colnames(composantes_valorisees_format), "mammiferes_marins")
  mm <- cumulativeEffects(stress = stresseurs_format,
                             valued = composantes_valorisees_format[, vc],
                             vulnerability = vulnerability[vc, ],
                             individual_cea = FALSE)

  # Effets sur les sites d'intérêt
  load_integrated("site")
  vc <- colnames(composantes_valorisees_format) %in% paste0("site_",colnames(site))
  site <- cumulativeEffects(stress = stresseurs_format,
                             valued = composantes_valorisees_format[, vc],
                             vulnerability = vulnerability[vc, ],
                             individual_cea = FALSE)

  # Single data
  ce$cumulative_effects_berge <- berge$cumulative_effects
  ce$cumulative_effects_habitat <- hab$cumulative_effects
  ce$cumulative_effects_mammiferes_marins <- mm$cumulative_effects
  ce$cumulative_effects_site <- site$cumulative_effects
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(ce,
           dsn = "data/data-output/cumulative_effects.geojson",
           quiet = TRUE,
           delete_dsn = TRUE)
  # _________________________________________________________________________ #



  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Clean global environment
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  clean()
  # ------------------------------------------------------------------------- #}
}
