#' Vulnérabilité des berges à l'érosion causée par les stresseurs environnementaux
#'
#' Création de la matrice de vulnérabilité des berges à l'érosion pour l'évaluation des effets cumulatifs
#'
#' @keywords vulnérabilité
#' @keywords intégrité des berges
#' @keywords composante valorisée
#'
#' @export
#'
#' @details Cette fonction génère la matrice de vulnérabilité pour les habitats
#'

vuln_berge <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Matrix
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Stressors for this project
  st <- read.csv("./data/data-metadata/metadata_stresseurs.csv")

  # Habitats for this project
  berge <- read.csv("./data/data-metadata/metadata_composantes_valorisees.csv") %>%
           filter(comp_val == "berge")
  # ------------------------------------------------------------------------- #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Vulnerability matrix
  # ----------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  vulnerability_berge <- matrix(data = 0,
                               nrow = nrow(st),
                               ncol = nrow(berge),
                               dimnames = list(st$accronyme,
                                               berge$accronyme))
  # -----
  str <- st$accronyme[st$stresseur == "navigation" | st$stresseur == "dragage"]

  # -----
  vulnerability_berge[str, "naturelle_vive"] <- 1
  vulnerability_berge[str, "artificielle_vive"] <- .9
  vulnerability_berge[str, "naturelle_semi_vegetalisee"] <- .8
  vulnerability_berge[str, "artificielle_semi_vegetalisee"] <- .7
  # _____________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # -----
  write.csv(vulnerability_berge,
            "./data/data-vulnerability/vulnerability_berge.csv",
            row.names = TRUE)
  # ------------------------------------------------------------------------- #}

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Clean global environment
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  clean()
  # ------------------------------------------------------------------------- #}
}
