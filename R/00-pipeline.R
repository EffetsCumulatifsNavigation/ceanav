

pipeline <- function(pipeline_data = FALSE,
                     pipeline_format = FALSE,
                     pipeline_analysis = FALSE,
                     pipeline_figure = FALSE) {

  if (pipeline_data) {
    # -----------------------------------
    # Study area
    getAOI()

    # -----------------------------------
    # Composantes valorisées
    # Habitats
    getHabitat()

    # -----------------------------------
    # Stressors
    ## Anchorages
    getAncrage()

    ## Déversements accidentels
    getDeversement()
  }

  if (pipeline_format)
    # -----------------------------------
    # Composantes valorisées
    # Habitats
    fmtHabitat()

    # -----------------------------------
    # Stressors
    ## Anchorages
    fmtAncrage()

    ## Déversements accidentels
    fmtDeversement()

}
