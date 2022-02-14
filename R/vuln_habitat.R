#' Vulnérabilité des habitats aux stresseurs environnementaux
#'
#' Création de la matrice de vulnérabilité pour les habtiats considérés pour l'évaluation des effets cumulatifs
#'
#' @keywords vulnérabilité
#' @keywords habitat
#' @keywords composante valorisée
#'
#' @export
#'
#' @details Cette fonction génère la matrice de vulnérabilité pour les habitats
#'

vuln_habitat <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Data import
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Vulnerability matrix
  load_format("data0061")
  rownames(data0061) <- data0061$human_activity

  # Stressors for this project
  st <- read.csv("./data/data-metadata/metadata_stresseurs.csv")

  # Habitats for this project
  hab <- read.csv("./data/data-metadata/metadata_composantes_valorisees.csv") %>%
         filter(comp_val == "habitat")

  # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # Select only relevant habitats for this process
  # TODO: perhaps differentiate habitat types directly in report to have two different ones:
    # 1. Habitats
    # 2. Sites of importance for flora and fauna
  # TODO: remove this code once the process of dividing habitats and sites has been completed
  rm <- c("site_alevinage","frayere","oiseaux","faune_susceptible","faune_vulnerable",
          "faune_menacee","flore_susceptible","flore_vulnerable","flore_menacee")
  hab <- hab[!hab$accronyme %in% rm, ]
  # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  # Kappel
  meta <- load_metadata("data0061")
  hab_kappel <- meta$data_description$habitats$accronyme
  str_kappel <- meta$data_description$stresseurs$nom
  # ------------------------------------------------------------------------- #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Stressor equivalencies
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  st_eq <- data.frame(st_tc = st$accronyme, st_kp = "")
  equiv <- function(tc, kp) {
    st_eq$st_kp[st_eq$st_tc == tc] <- kp
    st_eq
  }
  # TODO: Verify ancrage vulnerability, not sure this makes sense
  st_eq <- equiv("ancrage","Direct human impact: trampling")
  st_eq <- equiv("hydrocarbures", "Ocean dumping: toxic materials")
  st_eq <- equiv("autres", "Ocean dumping: toxic materials")
  st_eq <- equiv("inconnus", "Ocean dumping: toxic materials")
  st_eq <- equiv("dragage", "Dredging")
  st_eq <- equiv("depot", "Sediment input: increase")
  st_eq <- equiv("dragage_prevu", "Dredging")
  st_eq <- equiv("CARGO", "Shipping (commercial, cruise, ferry)")
  st_eq <- equiv("CONTAINER", "Shipping (commercial, cruise, ferry)")
  st_eq <- equiv("DRY.BULK", "Shipping (commercial, cruise, ferry)")
  st_eq <- equiv("FERRY.RO.RO", "Shipping (commercial, cruise, ferry)")
  st_eq <- equiv("GOVERNMENT.RESEARCH", "Shipping (commercial, cruise, ferry); Military activity; Scientific research: collecting;Scientific research: experiments/surveys")
  st_eq <- equiv("Observation", "Tourism: whale watching")
  st_eq <- equiv("PASSENGER", "Shipping (commercial, cruise, ferry)")
  st_eq <- equiv("PLEASURE.VESSELS", "Shipping (commercial, cruise, ferry)")
  st_eq <- equiv("SPECIAL.SHIPS", "Shipping (commercial, cruise, ferry)")
  st_eq <- equiv("TANKER", "Shipping (commercial, cruise, ferry)")
  st_eq <- equiv("TUGS.PORT", "Shipping (commercial, cruise, ferry)")
  st_eq <- equiv("navigation_portuaire", "Shipping (commercial, cruise, ferry)")
  st_eq <- equiv("DD", "Fishing: demersal habitat-modifying")
  st_eq <- equiv("DNL", "Fishing: demersal non-habitat-modifying low bycatch")
  st_eq <- equiv("DNH", "Fishing: demersal non-habitat-modifying high bycatch")
  st_eq <- equiv("PLB", "Fishing: pelagic low bycatch")
  st_eq <- equiv("PHB", "Fishing: pelagic high bycatch")
  st_eq <- equiv("pollution_maritime", "Ocean pollution (from ships, ports, etc.)")
  st_eq <- equiv("naufrage", "Ocean dumping: shipwrecks")
  st_eq <- equiv("PASSENGER.FERRY.RO.RO", "Shipping (commercial, cruise, ferry)")

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Stressor equivalencies
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  hb_eq <- data.frame(hb_tc = hab$accronyme, hb_kp = "")
  equiv <- function(tc, kp) {
    hb_eq$hb_kp[hb_eq$hb_tc == tc] <- kp
    hb_eq
  }
  hb_eq <- equiv("zostere", "eelgrass")
  hb_eq <- equiv("zone_inondable", "rocky_intertidal;tidal_flats;saltmarsh")
  hb_eq <- equiv("eau_peu_profonde", "saltmarsh")
  hb_eq <- equiv("marais", "saltmarsh")
  hb_eq <- equiv("marecage", "saltmarsh")
  hb_eq <- equiv("milieu_humide", "saltmarsh")
  hb_eq <- equiv("gisement_coquilliers", "nearshore_soft_bottom")
  hb_eq <- equiv("meuble_sans_falaise", "tidal_flats")
  hb_eq <- equiv("rocheuse_sans_falaise", "rocky_intertidal")
  hb_eq <- equiv("rocheuse_sans_escarpement", "rocky_intertidal")
  hb_eq <- equiv("terrasse_fluviale", "saltmarsh")
  hb_eq <- equiv("terrasse_plage", "beach")
  # TODO: hb_eq <- equiv("herbier_aquatique", "Algal zone")
  # TODO: hb_eq <- equiv("marais_maritime","Salt marsh")
  # TODO: hb_eq <- equiv("marecage_intertidal","Salt marsh")
  # TODO: hb_eq <- equiv("bas_estran_rocheux","Rocky intertidal")
  # TODO: hb_eq <- equiv("lagune","Nearshore soft bottom")
  # TODO: hb_eq <- equiv("dela","Salt marsh;Nearshore soft bottom")
  # TODO: hb_eq <- equiv("plage","Beach")
  # TODO: hb_eq <- equiv("bas_estran_meuble","Nearshore soft bottom")
  # TODO: hb_eq <- equiv("haut_estran_rocheux","Nearshore hard bottom")
  # TODO: hb_eq <- equiv("infralittoral","Nearshore hard bottom;Nearshore soft bottom")
  # ------------------------------------------------------------------------- #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Vulnerability matrix
  # ----------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  vulnerability_habitat <- matrix(data = NA, nrow = nrow(st_eq), ncol = nrow(hb_eq),
                                  dimnames = list(c(),c()))

  for(i in 1:length(hb_eq$hb_kp)) {
    # Habitats
    h <- str_split(hb_eq$hb_kp[i], ";") %>% unlist()

    # Stressors
    for(j in 1:length(st_eq$st_kp)) {
      s <- str_split(st_eq$st_kp[j], ";") %>% unlist()
      vulnerability_habitat[j,i] <- mean(as.matrix(data0061[s, h]), na.rm = TRUE)
    }
  }

  # Change names
  colnames(vulnerability_habitat) <- hab$accronyme
  rownames(vulnerability_habitat) <- st$accronyme
  # _____________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Adjust vulnerability scores
  # ----------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Diminution de 5% pour les marais
  vulnerability_habitat[,"marais"] <- vulnerability_habitat[,"marais"]*.95

  # Diminution de 10% pour les marécages
  vulnerability_habitat[,"marecage"] <- vulnerability_habitat[,"marecage"]*.9

  # Diminution de 10% pour les déversements de types autres
  vulnerability_habitat["autres", ] <- vulnerability_habitat["autres", ]*.9

  # Observation terrasse_plage = NA for now. Change to 0
  vulnerability_habitat["Observation","terrasse_plage"] <- 0

  # Normalize between 0 and 1
  norm <- function(x, dat) round(x / max(dat, na.rm = TRUE), 4)
  vulnerability_habitat <- apply(vulnerability_habitat, 2, norm, vulnerability_habitat)

  # Round everything
  vulnerability_habitat <- round(vulnerability_habitat, 2)
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
  write.csv(vulnerability_habitat,
            "./data/data-vulnerability/vulnerability_habitat.csv",
            row.names = TRUE)
  # ------------------------------------------------------------------------- #}

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Clean global environment
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  clean()
  # ------------------------------------------------------------------------- #}
}
