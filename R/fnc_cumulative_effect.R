#' Cumulative effects
#'
#' Function to evaluate cumulative effects of stressors on valued components
#'
#' @param stress matrix with stressors intensity
#' @param valued matrix with presence or amount of valued components
#' @param vulnerability matrix with vulnerability of valued components to stressors. Names used in stress and valued matrices must be the same as that found in vulnerability matrix
#'
#' @keywords cumulative effects
#'
#' @export
#'
#' @details
#'

cumulativeEffects <- function(stress, valued, vulnerability) {

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


  # TODO: TO PUT IN FUNCTION params
  stress <- stresseurs_format
  valued <- composantes_valorisees_format
  vulnerability <- vulnerability


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # Evaluate individual effects
  cumulative_effects <- list()
  for(i in 1:nrow(stress)) {
    # Individual risk
    ## This provides an evaluation of "risk", i.e. the effect IF the VC is present (=1)
    ## Essentially evaluates the Si * ui,j portion of the Halpern equation
    risk <- sweep(vulnerability, MARGIN=2, stress[i, ], `*`)

    # Individual effects
    cumulative_effects[[i]] <- sapply(risk, '*', valued[i, ])

    # Cumulative risk (IF YOU WANT CUMULATIVE EFFECTS TO SINGLE VS RIGHT NOW RUN THIS)
    ## Evaluation of the cumulative "risk", i.e. the sum of all risks for each VC
    # cumulative_effect <- rowSums(effect)
  }

  # Names of stressors and vcs
  # Duplicate, but we want to be certain that they remain the same between objects
  st <- colnames(cumulative_effects[[1]])
  vc <- rownames(cumulative_effects[[1]])

  # Export information on effects predicted by individual stressors
  folder <- "data/data-output/cea_stresseur/"
  if (!file.exists(folder)) dir.create(folder)

  # Iterate over stressors
  for(i in 1:length(st)) {
    temp <- cumulative_effects

    # Iterate over grid cells
    for(j in 1:length(temp)) {
      temp[[j]] <- temp[[j]][, st[i]] %>%
                   t() %>%
                   data.frame()
    }

    # Single data.frame
    temp <- bind_rows(temp)

    # Export
    write.csv(temp, glue("{folder}cea_{st[i]}.csv"), row.names = FALSE)
  }

  # Export information on effects predicted on individual valued components
  folder <- "data/data-output/cea_composante_valorisee/"
  if (!file.exists(folder)) dir.create(folder)

  # Iterate over stressors
  for(i in 1:length(cv)) {
    temp <- cumulative_effects

    # Iterate over grid cells
    for(j in 1:length(temp)) {
      temp[[j]] <- temp[[j]][cv[i], ] %>%
                   t() %>%
                   data.frame()
    }

    # Single data.frame
    temp <- bind_rows(temp)

    # Export
    write.csv(temp, glue("{folder}cea_{cv[i]}.csv"), row.names = FALSE)
  }

  # Single cumulative effects assessment
  # TODO: evaluate whether I should normalize results
  # TODO: remove na.rm from equation, there should not be any and if there are is a sign that there is a problem to resolve, so I want to keep it
  data(grid1p)
  ce <- lapply(cumulative_effects, sum, na.rm = TRUE) %>%
               unlist()
  grid1p$cumulative_effects <- ce
  st_write(grid1p, file = "data/data-output/cumulative_effects.geojson", quiet = TRUE)
}
