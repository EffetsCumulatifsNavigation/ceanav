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
  pipeline_vulnerability = FALSE,
  pipeline_analysis = FALSE,
  pipeline_figures = FALSE,
  pipeline_fiches = FALSE,
  pipeline_report = TRUE,
  pipeline_pkgsite = FALSE
) {


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~
  if (pipeline_metadata) {
    # Metadata
    # update_metadata("raw")
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
    get_data0047()
    get_data0048()
    get_data0049()
    get_data0050()
    get_data0051()
    get_data0052()
    get_data0053()
    get_data0054()
    get_data0055()
    get_data0056()
    get_data0057()
    get_data0058()
    get_data0059()
    get_data0060()
    get_data0061()
    get_data0062()
    get_data0063()
    get_data0064()
    get_data0065()
    get_data0066()
    get_data0067()
    get_data0068()
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
    st_dragage()
    st_naufrage()
    st_navigation()
    st_peche_commerciale()
    # st_rejets()

    # This one MUST be run last, as it is an integrated of integrated stressors
    st_pollution_maritime()

    # Metadata for stressors and valued components
    metadata_st_cv()
  }

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~
  if (pipeline_vulnerability) {
    # Each valued component
    vuln_berge()
    vuln_habitat()
    vuln_mammiferes_marins()
    vuln_faune_flore()
    vuln_site()

    # Integrate all
    vulnerability_matrix()
  }

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~
  if (pipeline_analysis) {
    # Stresseurs et composantes valorisées
    ana_stresseurs_raw()
    ana_composantes_valorisees_raw()

    # Transformer stresseurs et composantes valorisées
    ana_stresseurs_format()
    ana_composantes_valorisees_format()

    # Stresseurs et composantes valorisées cumulés
    ana_cumulative_stresseurs()
    ana_cumulative_composantes_valorisees()

    # Cumulative exposure
    ana_cumulative_exposure()

    # Cumulative effects
    ana_cumulative_effects()

    # Cumulative effects per km2 for valued components
    ana_cumulative_effects_cv_km2()

    # Cumulative effects per km2 for administrative regions
    ana_cumulative_effects_region_km2()
  }


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~
  if (pipeline_figures) {
    # Zone d'étude
    fig_aoi()

    # Données formatées (annexe)
    # fig_format_all()

    # Données intégrées (portrait)
    fig_integrated_all()

    # Analyses
    fig_cumulative_stresseurs()
    fig_cumulative_composantes_valorisees()
    fig_cumulative_exposure()
    fig_cumulative_effects()
    fig_regional_contribution()
    fig_metanetwork()
    fig_region_cea_km2()
  }


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~
  if (pipeline_fiches) {
    # # -----
    # # Données formatées (annexe)
    # rep_annexe_data_description_all(
    #   output_folder = "./report/contenu/annexes/",
    #   suffix =  "annexe4-"
    # )
    #
    # -----
    # Données intégrées (portrait)
    # Stresseurs
    folder <- "report/contenu/2-portrait/1-stresseurs/"
    rep_portrait_data_description(data_id = "ancrage", output_folder = folder)
    rep_portrait_data_description(data_id = "deversement", output_folder = folder)
    rep_portrait_data_description(data_id = "dragage", output_folder = folder)
    rep_portrait_data_description(data_id = "naufrage", output_folder = folder)
    rep_portrait_data_description(data_id = "navigation", output_folder = folder)
    rep_portrait_data_description(data_id = "peche_commerciale", output_folder = folder)
    rep_portrait_data_description(data_id = "pollution_maritime", output_folder = folder)

    # Composantes valorisées
    folder <- "report/contenu/2-portrait/2-composantes_valorisees/"
    rep_portrait_data_description(data_id = "berge", output_folder = folder)

    # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    # NOTE: 2021-12-15
    #       Removed, manual restructuring of this report section
    #       Eventually fix the code to generate individual RMDs
    #
    # rep_portrait_data_description(data_id = "habitat", output_folder = folder)
    # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    rep_portrait_data_description(data_id = "mammiferes_marins", output_folder = folder)
    rep_portrait_data_description(data_id = "site", output_folder = folder)
  }


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~
  if (pipeline_report) {
    file.copy("./figures/", "./report/", recursive = TRUE)
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

    # WARNING: Temporary pipeline to export report only to another repo.
    # TODO: This is not reproducible and should be removed from the pipeline as soon as this
    #       repository can be made available publicly
    unlink("../Rapport/docs/", recursive = TRUE)
    file.copy("./report/docs", "../Rapport/", recursive = TRUE)
    file.copy("./report/figures", "../Rapport/", recursive = TRUE)
  }

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~
  if (pipeline_pkgsite) {
    pkgdown::build_site()
  }
}
