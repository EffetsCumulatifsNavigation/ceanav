

pipeline <- function(pipeline_data = FALSE,
                     pipeline_format = FALSE,
                     pipeline_analysis = FALSE,
                     pipeline_figure = FALSE,
                     pipeline_report = TRUE) {

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~
  if (pipeline_data) {
    # -----------------------------------
    # Study area
    getAOI() # Study area and grids

    # -----------------------------------
    # Composantes valorisées
    getHabitat() # Habitats
    getBerge() # Intégrité berges
    getSite() # Sites d'importance
    getMammiferesMarins() # Distribution de mammifères marins

    # -----------------------------------
    # Stressors
    getAncrage() ## Anchorages
    getDeversement() ## Déversements accidentels
    getNavigation() ## Navigation
    getPecheCommerciale() ## Pêches commerciales
  }

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~
  if (pipeline_format) {
    # -----------------------------------
    # Composantes valorisées
    fmtHabitat() # Habitats
    fmtBerge() # Intégrité berges
    fmtSite() # Sites d'importance
    fmtMammiferesMarins() # Distribution de mammifères marins



    # -----------------------------------
    # Stressors
    fmtAncrage() ## Anchorages
    fmtDeversement() ## Déversements accidentels
    fmtNavigation() ## Navigation
    fmtPecheCommerciale() ## Pêches commerciales
  }

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~
  if (pipeline_report) {
    suppressWarnings({
      setwd('./report/')
      bookdown::render_book(input = "index.Rmd",
                            output_format = "bookdown::gitbook",
                            config_file = "_bookdown.yml")
      setwd('../')
    })
  }
}
