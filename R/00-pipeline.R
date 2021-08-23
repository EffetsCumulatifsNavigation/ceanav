#' Pipeline d'analyse
#'
#' Pipeline d'analyse du projet d'évaluation des effets cumulatifs de la navigation dans le Saguenay et le Saint-Laurent
#'
#' @keywords pipeline
#'
#' @export
#'
#' @details Cette fonction exécute l'ensemble des fonctions pour effectuer l'analyse des effets cumulatifs
#'

pipeline <- function(
  pipeline_metadata = FALSE,
  pipeline_data = FALSE,
  pipeline_integration = FALSE,
  pipeline_analysis = FALSE,
  pipeline_figures = FALSE,
  pipeline_annexes = FALSE,
  pipeline_report = TRUE
) {


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~
  if (pipeline_metadata) {
    # Metadata
    update_metadata("raw")
    # update_metadata("integrated")

    # Contacts
    update_contact()
  }


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~
  if (pipeline_data) {
    # -----------------------------------
    # Study area and basemaps
    get_aoi()
    get_basemap()

    # -----------------------------------
    # Individual datasets
    get_data0001()
    get_data0002()
    get_data0003()
    get_data0004()
    get_data0005()
    get_data0006()
    get_data0007()
    get_data0008()
    get_data0009()
    get_data0010()
    get_data0011()
    get_data0012()
    get_data0013()
    get_data0014()
    get_data0015()
    get_data0016()
    get_data0017()
    get_data0018()
    get_data0019()
    get_data0020()
    get_data0021()
    get_data0022()
    get_data0023()
    get_data0024()
    get_data0025()
    get_data0026()
    get_data0027()
    get_data0028()
    get_data0029()
    get_data0030()
    get_data0031()
    get_data0032()
    get_data0033()
    get_data0034()
    get_data0035()
    get_data0036()
    get_data0037()
    get_data0038()
    get_data0039()
    get_data0040()
    get_data0041()
    get_data0042()
    get_data0043()
    get_data0044()
    get_data0045()
    get_data0046()
  }


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~
  if (pipeline_integration) {
    # Composantes valorisées
    cv_berge()
    cv_habitat()
    cv_mammiferes_marins()
    # cv_qualite()
    cv_site()

    # Stresseurs
    st_ancrage()
    st_deversement()
    # st_dragage()
    # st_naufrages()
    st_navigation()
    st_peche_commerciale()
    # st_rejets()
  }


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~
  if (pipeline_figures) {
    # Données formatées (annexe)
    rep_annexe_data_figures_all()

    # Données intégrées (portrait)
    rep_portrait_data_figures_int_all()
    # TODO: Différencier binaire de continu
    # TODO: Retirer trait de côte
    # TODO: insert sur la portion fluviale pour meilleure visibilité
    # TODO: valeurs de 0 / absentes transparentes

  }


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~
  if (pipeline_annexes) {
    # Générer fiches descriptives
    rep_annexe_data_description_all(
      output_folder = "./report/contenu/annexes/",
      suffix =  "annexe3-"
    )
  }


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~
  if (pipeline_report) {
    suppressWarnings({
      setwd('./report/')

      # HTML format
      bookdown::render_book(input = "index.Rmd",
                            output_format = "bookdown::gitbook",
                            config_file = "_bookdown.yml")

      # # PDF format
      # bookdown::render_book(input = "index.Rmd",
      #                       output_format = "bookdown::pdf_book",
      #                       config_file = "_bookdown.yml")

      setwd('../')
    })
  }
}
