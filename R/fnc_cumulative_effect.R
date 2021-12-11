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

cumulativeEffects <- function(stress, valued, vulnerability, individual_cea = TRUE) {

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

  if (individual_cea) {
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

    # Iterate over valued components
    for(i in 1:length(vc)) {
      temp <- cumulative_effects

      # Iterate over grid cells
      for(j in 1:length(temp)) {
        temp[[j]] <- temp[[j]][vc[i], ] %>%
                     t() %>%
                     data.frame()
      }

      # Single data.frame
      temp <- bind_rows(temp)

      # Export
      write.csv(temp, glue("{folder}cea_{vc[i]}.csv"), row.names = FALSE)
    }
  }

  # Single cumulative effects assessment
  # TODO: evaluate whether I should normalize results
  # TODO: remove na.rm from equation, there should not be any and if there are is a sign that there is a problem to resolve, so I want to keep it
  data(grid1p)
  ce <- lapply(cumulative_effects, sum) %>%
               unlist()
  grid1p$cumulative_effects <- ce

  return(grid1p)
}
