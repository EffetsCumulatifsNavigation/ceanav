

pipeline <- function(pipeline_data = FALSE,
                     pipeline_format = FALSE,
                     pipeline_analysis = FALSE,
                     pipeline_figure = FALSE) {

  if (pipeline_data) {
    # -----------------------------------
    # Study area
    getAOI() # Study area and grids

    # -----------------------------------
    # Composantes valorisées
    getHabitat() # Habitats
    getBerge() # Intégrité berges

    # -----------------------------------
    # Stressors
    getAncrage() ## Anchorages
    getDeversement() ## Déversements accidentels
    getNavigation() ## Navigation
  }

  if (pipeline_format) {
    # -----------------------------------
    # Composantes valorisées
    fmtHabitat() # Habitats
    fmtBerge() # Intégrité berges


    # -----------------------------------
    # Stressors
    fmtAncrage() ## Anchorages
    fmtDeversement() ## Déversements accidentels
    fmtNavigation() ## Navigation

  }
}
