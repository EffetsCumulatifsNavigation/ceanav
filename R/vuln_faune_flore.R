#' Vulnérabilité des sites d'importance fauniques et floristiques aux stresseurs environnementaux
#'
#' Création de la matrice de vulnérabilité pour les sites d'importance fauniques et floristiques considérés pour l'évaluation des effets cumulatifs
#'
#' @keywords vulnérabilité
#' @keywords habitat
#' @keywords composante valorisée
#'
#' @export
#'
#' @details Cette fonction génère la matrice de vulnérabilité pour les habitats
#'

vuln_faune_flore <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Data import
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Vulnerability matrix
  load_format("data0063")

  # Habitats for this project
  hab <- read.csv("./data/data-metadata/metadata_composantes_valorisees.csv") %>%
         filter(comp_val == "habitat")

  # Stressors
  st <- unique(data0063$categories)

  # -----
  cv <- unique(data0063$cv)

  # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # Select only relevant habitats for this process -- contraire du script pour habitats
  # TODO: perhaps differentiate habitat types directly in report to have two different ones:
    # 1. Habitats
    # 2. Sites of importance for flora and fauna
  # TODO: remove this code once the process of dividing habitats and sites has been completed
  rm <- c("site_alevinage","frayere","oiseaux","faune_susceptible","faune_vulnerable",
          "faune_menacee","flore_susceptible","flore_vulnerable","flore_menacee",
          "lep_menacee", "lep_voie_disparition")
  hab <- hab[hab$accronyme %in% rm, ]
  # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # ------------------------------------------------------------------------- #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Vulnerability scores
  # ------------------------------------
  # NOTE:
  # Toues les critères de vulnérabilité ne sont pas utilisés afin de calculer la vulnérabilité des sites d'importance fauniques et floristiques. En tant que tel, les valeurs de vulnérabilité relatives ne peuvent être directement comparées entre elles. Les sites d'importance pour les espèces à statut ont été évaluées à partir des mêmes critères; il en est de même pour les sites d'alevinage et les frayères, qui sont des sites d'importance pour le cycle de vie des poissons. Les sites d'importance pour les oiseaux marins sont quant à eux seuls dans leurs évaluation. Puisque ces trois groupes ne sont pas évalués à partir de même critères, les valeurs de vulnérabilité relative ont été normalisées entre 0 et 1 en considérant la valeur maximale de vulnérabilité au sein de chaque groupe. Les trois groupes ont subséquemment été combinés en une seule matrice de vulnérabilité.
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Oiseaux
  uid <- data0063$cv == "oiseau"
  oiseau <- data0063[uid, c("categories","cv","frequence","population")]
  oiseau$vulnerabilite <- rowSums(oiseau[, c("frequence","population")])

  # Cycles de vie poisson
  uid <- data0063$cv %in% c("frayere","alevinage")
  poisson <- data0063[uid, c("categories","cv","frequence","reproduction","population")]
  poisson$vulnerabilite <- rowSums(poisson[, c("frequence","reproduction","population")])


  # Espèces à statut
  uid <- data0063$cv %in% c("susceptible","vulnerable","menace")
  statut <- data0063[uid, c("categories","cv","frequence","population","statut")]
  statut$vulnerabilite <- rowSums(statut[, c("frequence","population","statut")])

  # Normlisation par groupe
  oiseau$vulnerabilite <- oiseau$vulnerabilite / max(oiseau$vulnerabilite)
  poisson$vulnerabilite <- poisson$vulnerabilite / max(poisson$vulnerabilite)
  statut$vulnerabilite <- statut$vulnerabilite / max(statut$vulnerabilite)

  # Long to wide
  oiseau <- pivot_wider(oiseau, id_cols = categories, names_from = cv, values_from = vulnerabilite)
  poisson <- pivot_wider(poisson, id_cols = categories, names_from = cv, values_from = vulnerabilite)
  statut <- pivot_wider(statut, id_cols = categories, names_from = cv, values_from = vulnerabilite)

  # Single table
  vuln <- left_join(oiseau, poisson, by = c("categories")) %>%
          left_join(statut, by = "categories")
  # ------------------------------------------------------------------------- #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Vulnerability matrix
  # ----------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  vulnerability_faune_flore <- matrix(data = NA, nrow = nrow(vuln), ncol = length(hab$accronyme),
                                  dimnames = list(c(vuln$categories),c(hab$accronyme)))


  # Fill matrix
  vulnerability_faune_flore[,'site_alevinage'] <- vuln$alevinage
  vulnerability_faune_flore[,'frayere'] <- vuln$frayere
  vulnerability_faune_flore[,'oiseaux'] <- vuln$oiseau
  vulnerability_faune_flore[,'faune_susceptible'] <- vuln$susceptible
  vulnerability_faune_flore[,'faune_vulnerable'] <- vuln$vulnerable
  vulnerability_faune_flore[,'faune_menacee'] <- vuln$menace
  vulnerability_faune_flore[,'flore_susceptible'] <- vuln$susceptible
  vulnerability_faune_flore[,'flore_vulnerable'] <- vuln$vulnerable
  vulnerability_faune_flore[,'flore_menacee'] <- vuln$menace


  # _____________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Adjust vulnerability scores
  # ----------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Diminution de 10% pour les déversements de types autres
  vulnerability_faune_flore["autres", ] <- vulnerability_faune_flore["autres", ]*.9

  # Round everything
  vulnerability_faune_flore <- round(vulnerability_faune_flore, 2)
  # _____________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Update metadata
  # ----------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #


  # _____________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # -----
  write.csv(vulnerability_faune_flore,
            "./data/data-vulnerability/vulnerability_faune_flore.csv",
            row.names = TRUE)
  # ------------------------------------------------------------------------- #}

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Clean global environment
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  clean()
  # ------------------------------------------------------------------------- #}
}
