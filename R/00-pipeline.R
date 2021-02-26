

pipeline <- function(pipeData = FALSE,
                     pipeAnalysis = FALSE) {

  if (pipeData) {
    # Study area
    getAOI()

    # Stressors
    ## Navigation
    nav_ais()
  }
}
