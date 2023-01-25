#' Habitats: Données sur les habitats
#'
#' Intégration des données utilisées pour caractériser la composante valorisée d'intégrité des habitats
#'
#' @keywords habitat
#' @keywords composante valorisée
#'
#' @export
#'
#' @details Cette fonction télécharge et formatte les données
#'

cv_habitat <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Notes
  # ------------------------------------
  #
  # Plusieurs catégories d'habitats composent la composante valorisée d'habitats.
  # Les types d'habitats ont été sélectionnés selon leur importance pour l'écologie
  # du milieu d'étude. Par exemple, les zostères sont des milieux qui sont importants
  # pour les cycles vitaux d'un nombre importants d'espèces, en plus d'influencer
  # le stockage de carbone et la dynamique hydrosédimentaire locale.
  #
  # Il est clair que d'autres habitats pourraient être ajoutés à la liste actuelle.
  # La liste des habitats considérés actuellement est la suivante:
  #
  #   - Zostères : 0001, 0002, 0003
  #   - Zones inondables : 0013, 0014
  #   - Milieux humides : 0004, 0005, 0006, 0042 - Modifié pour 0053
  #   - Marais côtiers : 0007 - Modifié pour 0053
  #   - Sites d'alevinage : 0009
  #   - Frayères : 0010
  #   - Zones herbacées : 0029
  #   - Espèces à statut : 0011 # Retiré
  #   - Habitats fauniques : 0036
  #   - Habitats floristiques : 0037
  #   - Colonies d'oiseaux : 0043, 0058
  #   - Gisements mollusques : 0056, 0057
  #   - Habitats côtiers : 0017
  #   - Espèces fauniques à statut : 0059
  #   - Espèces floristiques à statut : 0060
  #
  # Update hiver 2023:
  #   - Frayères: "0073","0074","0075","0076"
  #   - Espèces à statut LEP: "0077"
  #   - Gisements coquilliers: ajouté "0078","0079","0080","0081"
  #   - Herbiers aquatiques: 0084
  #   - Sites d'alevinage: 0086
  #   - Frayères: 0087
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # ------------------------------------------------------
  load_temp <- function(dat) {
    # -----
    dat <- paste0("data", dat)

    # -----
    res <- list()
    for(i in 1:length(dat)) {
      load_format(dat[i])
      res[[i]] <- get(dat[i])
    }

    # -----
    res <- bind_rows(res)

    # -----
    res
  }

  # ------------------------------------------------------
  uid <- function(dat, category = NULL, field = NULL, clip = FALSE) {
    # -----
    if (clip) dat <- st_intersection(dat, st_simplify(aoi, dTolerance = 1000))

    # -----
    if (is.null(category)) {
      uid <- dat %>%
             st_intersects(grid1p) %>%
             unlist() %>%
             unique()
    } else {
      dat <- dat[dat[,field,drop = TRUE] == category, ]
      uid <- dat %>%
             st_intersects(grid1p) %>%
             unlist() %>%
             unique()
    }

    # -----
    dat <- numeric(nrow(grid1p))
    dat[uid] <- 1

    # -----
    dat
  }

  # ------------------------------------------------------
  superficie <- function(dat, category = NULL, field = NULL) {
    # -----
    if (!is.null(category)) {
      dat <- dat[dat[,field,drop = TRUE] == category, ]
    }

    # -----
    sup <- dat %>%
           st_intersection(st_simplify(aoi, dTolerance = 1000)) %>%
           st_union() %>%
           st_area() %>%
           as.numeric(.) * 1e-6

    # -----
    sup
  }

  line_length <- function(dat, category = NULL, field = NULL) {
    # -----
    if (!is.null(category)) {
      dat <- dat[dat[,field,drop = TRUE] == category, ]
    }

    # -----
    len <- dat %>%
           st_intersection(st_simplify(aoi, dTolerance = 1000)) %>%
           st_union() %>%
           st_length() %>%
           as.numeric(.) * 1e-3

    # -----
    len
  }


  # ------------------------------------------------------
  meta_update <- function(meta, dat, accr, fr, descr = "", type = "", zonesNA = NA, en, descr_en = "", type_en = "") {
    meta$rawData <- c(meta$rawData, dat)
    meta$accronyme <- c(meta$accronyme, accr)
    meta$type <- c(meta$type, type)
    meta$francais <- c(meta$francais, fr)
    meta$source <- c(meta$source, paste0(dat, collapse = ","))
    # meta$superficie <- c(meta$superficie, superficie(dat))
    meta$description <- c(meta$description, descr)
    meta$zonesNA <- c(meta$zonesNA, zonesNA)
    meta$english <- c(meta$english, en)
    meta$description_en <- c(meta$description_en, descr_en)
    meta$type_en <- c(meta$type_en, type_en)
    meta
  }

  # ------------------------------------------------------
  meta_temp <- list()
  sup <- numeric()

  # ------------------------------------------------------
  data(grid1p)
  data(aoi)
  habitat <- grid1p

  # ------------------------------------------------------
  # Zostères : 0001, 0002, 0003
  nm <- c("0001", "0002", "0003")
  dat <- load_temp(nm)
  meta_temp <- meta_update(meta_temp, 
                           dat = nm, 
                           accr = "zostere", 
                           fr = "Zostères", 
                           descr = "Distribution de la zostère marine (*Zostera marina*)", 
                           type = "Milieux naturels",
                           en = "Eelgrass",
                           descr_en = "Distribution of eelgrass (*Zostera marina*)",
                           type_en = "Natural areas")
  habitat$zostere <- uid(dat)
  sup <- c(sup, superficie(dat))

  # ------------------------------------------------------
  # Zones inondables : 0013, 0014
  nm <- c("0013", "0014")
  dat <- load_temp(nm)
  meta_temp <- meta_update(meta_temp, 
                           dat = nm, 
                           accr = "zone_inondable", 
                           fr = "Zones inondables", 
                           descr = "Territoires ayant une probabilité élevée d'être inondés selon une récurrence des crues sur 2 ans, 20 ans ou sur 100 ans", 
                           type = "Milieux naturels", 
                           en = "Floodplain zones", 
                           descr_en = "Land with a high probability of flooding based on a 2-year, 20-year or 100-year flood recurrence", 
                           type_en = "Natural areas")
  habitat$zone_inondable <- uid(dat)
  sup <- c(sup, superficie(dat))

  # ------------------------------------------------------
  # Milieux humides : 0004, 0005, 0006, 0042
  # NOTE (2021-10-08): switching to  0053, which incorporates all those datasets and divides
  #                    milieux humides into several categories
  # -----
  # Eau peu profonde
  nm <- c("0053")
  dat <- load_temp(nm)
  meta_temp <- meta_update(meta_temp, 
                           dat = nm, 
                           accr = "eau_peu_profonde", 
                           fr = "Eau peu profonde", 
                           descr = "Milieu humide dont le niveau d’eau est inférieur à 2 m et présentant des plantes aquatiques flottantes ou submergées ainsi que des plantes émergentes dont le couvert fait moins de 25 % de la superficie du milieu.", 
                           type = "Milieux naturels", 
                           en = "Shallow water", 
                           descr_en = "A wetland with a water level of less than 2 m and with floating or submerged aquatic plants and emergent plants that cover less than 25% of the wetland area.", 
                           type_en = "Natural areas")
  habitat$eau_peu_profonde <- uid(dat, "Eau", "TYPE", clip = TRUE)
  sup <- c(sup, superficie(dat, "Eau", "TYPE"))

  # -----
  # Marais
  meta_temp <- meta_update(meta_temp, 
                           dat = nm, 
                           accr = "marais", 
                           fr = "Marais", 
                           descr = "Milieu humide sur dépôt minéral, dominé par une végétation herbacée couvrant plus de 25 % de la superficie. Les arbustes et les arbres, lorsque présents, couvrent moins de 25 % de la superficie du milieu.", 
                           type = "Milieux naturels", 
                           en = "Marshes", 
                           descr_en = "A wetland on a mineral deposit, dominated by herbaceous vegetation covering more than 25% of the area.  Shrubs and trees, when present, cover less than 25% of the area of the wetland.", 
                           type_en = "Natural areas")
  habitat$marais <- uid(dat, "Marais", "TYPE", clip = TRUE)
  sup <- c(sup, superficie(dat, "Marais", "TYPE"))



  # -----
  # Marécage
  meta_temp <- meta_update(meta_temp, 
                           dat = nm, 
                           accr = "marecage", 
                           fr = "Marécage", 
                           descr = "Milieu humide sur dépôt minéral, dominé par une végétation ligneuse arbustive ou arborescente, avec plus de 25 % de couvert.", 
                           type = "Milieux naturels",
                           en = "Swamps", 
                           descr_en = "A wetland on a mineral deposit, dominated by ligneous, shrubby or arborescent vegetation covering more than 25% of the area.", 
                           type_en = "Natural areas")
  habitat$marecage <- uid(dat, "Marécage", "TYPE", clip = TRUE)
  sup <- c(sup, superficie(dat, "Marécage", "TYPE"))


  # -----
  # Milieu humide
  meta_temp <- meta_update(meta_temp, 
                           dat = nm, 
                           accr = "milieu_humide", 
                           fr = "Milieu humide", 
                           descr = "Regroupe les milieux humides dont le type est inconnu.", 
                           type = "Milieux naturels",
                           en = "Wetland", 
                           descr_en = "Includes wetlands of unknown type.", 
                           type_en = "Natural areas")
  habitat$milieu_humide <- uid(dat, "Milieu humide", "TYPE", clip = TRUE)
  sup <- c(sup, superficie(dat, "Milieu humide", "TYPE"))


  # # -----
  # # Tourbière
  # # NOTE: Retiré de l'analyse, milieu terrestre
  # meta_temp <- meta_update(meta_temp, nm, "tourbiere", "Tourbière", "Regroupe les milieux humides dans lesquels il y a une accumulation de tourbe d’au moins 30 cm d’épaisseur.", type = "Milieux naturels")
  # habitat$tourbiere <- uid(dat, "Tourbière", "TYPE")
  # sup <- c(sup, superficie(dat, "Tourbière", "TYPE"))



  # ------------------------------------------------------
  # Marais côtiers : 0007
  # NOTE (2021-10-08): switching to  0053, which incorporates this data and divides
  #                    milieux humides into several categories
  # dat <- "0007"
  # meta_temp <- meta_update(meta_temp, nm, "marais_cotier", "Marais côtiers")
  # habitat$marais_cotier <- uid(dat)

  # ------------------------------------------------------
  # Sites d'alevinage : 0009
  # MAJ 2023-03: Replace 0009 with 0086
  nm <- "0086"
  dat <- load_temp(nm)
  meta_temp <- meta_update(meta_temp, 
                           dat = nm, 
                           accr = "site_alevinage", 
                           fr = "Sites d'alevinage", 
                           descr = "Sites d'alimentation et de protection pour les stades de vie initiaux des poissons (*e.g.* larves, alevins, juvéniles)", 
                           type = "Cycles de vie", 
                           zonesNA = NA,
                           en = "Nursery sites", 
                           descr_en = "Feeding and protection sites for early life stages of fish (e.g. larvae, fry, juveniles)", 
                           type_en = "Life cycles")
  habitat$site_alevinage <- uid(dat)
  sup <- c(sup, superficie(dat))

  # ------------------------------------------------------
  # Frayères : 0010
  # MAJ 2023-03: Replace 0010 with 0087
  nm1 <- "0087"
  nm2 <- c("0073","0074","0075","0076")
  dat1 <- load_temp(nm1)
  dat2 <- load_temp(nm2) |>
          dplyr::filter(CLASSE_A == "Breeding area")
  dat <- bind_rows(dat1,dat2)
  meta_temp <- meta_update(meta_temp, 
                           dat = c(nm1,nm2), 
                           accr = "frayere", 
                           fr = "Frayères", 
                           descr = "Sites de reproduction où des poissons femelles pondent des oeufs pour fécondation par les mâles", 
                           type = "Cycles de vie", 
                           en = "Spawning sites", 
                           descr_en = "Spawning sites where female fish lay eggs for fertilization by males", 
                           type_en = "Life cycles")
  habitat$frayere <- uid(dat)
  sup <- c(sup, superficie(dat))


  # ------------------------------------------------------
  # Zones herbacées : 0029
  # NOTE (2021-10-08): switching to  0053, which incorporates this data and divides
  #                    milieux humides into several categories
  # dat <- "0029"
  # meta_temp <- meta_update(meta_temp, nm, "zone_herbacee", "Zones herbacées")
  # habitat$zone_herbacee <- uid(dat)


  # ------------------------------------------------------
  # Espèces à statut : 0011
  # dat <- "0011"
  # meta_temp <- meta_update(meta_temp, nm, "espece_statut", "Espèces à statut", type = "Habitats importance ecologique")
  # habitat$espece_statut <- uid(dat)
  # sup <- c(sup, superficie(dat))


  # # ------------------------------------------------------
  # # Habitats fauniques : 0036
  # # NOTE: Retirer, trop étendu
  # dat <- "0036"
  # meta_temp <- meta_update(meta_temp, nm, "faunique", "Habitats fauniques", type = "Habitats importance ecologique")
  # habitat$faunique <- uid(dat)
  # sup <- c(sup, superficie(dat))


  # # ------------------------------------------------------
  # # Habitats floristiques : 0037
  # nm <- "0037"
  # dat <- load_temp(nm)
  # meta_temp <- meta_update(meta_temp, nm, "floristique", "Habitats floristiques", type = "Habitats floristiques")
  # habitat$floristique <- uid(dat)
  # sup <- c(sup, superficie(dat))


  # ------------------------------------------------------
  # Colonies d'oiseaux : 0043, 0058
  nm <- c("0043", "0058")
  dat <- load_temp(nm)
  meta_temp <- meta_update(meta_temp, 
                           dat = nm, 
                           accr = "oiseaux", 
                           fr = "Oiseaux", 
                           descr = "Sites connus d'importance pour les oiseaux, tels des sites de nidification et des colonies d'oiseaux marins", 
                           type = "Cycles de vie",
                           en = "Birds", 
                           descr_en = "Known sites of importance to birds, such as nesting sites and seabird colonies", 
                           type_en = "Life cycles")
  habitat$oiseaux <- uid(dat)
  sup <- c(sup, superficie(dat))

  # # ------------------------------------------------------
  # # Milieux sableux : 0008
  # # NOTE: Retirer, couvert par l'intégrité des berges
  # dat <- "0008"
  # meta_temp <- meta_update(meta_temp, nm, "milieux_sableux", "Milieux sableux", type = "Milieux sableux")
  # habitat$milieux_sableux <- uid(dat)
  # sup <- c(sup, line_length(dat))

  # ------------------------------------------------------
  # Gisements coquilliers : 0056, 0057
  # Pétoncle uniquement dans le Golfe, retrait 0056
  # MAJ 2023: 
  # 0078, 0079, 0080, 0081
  # Mactre de l'Atlantique uniquement aux Îles de la Madeleine, retrait 0080
  nm <- c("0057","0078","0079","0081")
  dat <- load_temp(nm)
  meta_temp <- meta_update(meta_temp, 
                           dat = nm, 
                           accr = "gisement_coquilliers", 
                           fr = "Gisements coquilliers", 
                           descr = "Gisements connus et exploités de mactre de Stimpson, de clovisse arctique, de couteau de l'Atlantique et de mye commune des eaux côtières du Québec", 
                           type = "Milieux naturels",
                           en = "Mollusk beds", 
                           descr_en = "Known and harvested Stimpson’s surf clam, Arctic wedge clam, Atlantic razor clam, and softshell clam deposits in Quebec coastal waters", 
                           type_en = "Natural areas")
  habitat$gisement_coquilliers <- uid(dat)
  sup <- c(sup, superficie(dat))


  # ------------------------------------------------------
  # Habitats côtiers : 0017
  # Nous avons sélectionné les habitats côtiers qui touchent à l'eau. Nous n'avons pas considéré les marais maritimes et les milieux humides riverains, qui sont déjà couverts par la base de données 0053
  # -----
  nm <- c("0017")
  dat <- load_temp(nm)
  meta_temp <- meta_update(meta_temp, 
                           dat = nm, 
                           accr = "meuble_sans_falaise", 
                           fr = "Meuble sans falaise", 
                           descr = "Dépôt non consolidé sans falaise", 
                           type = "Milieux naturels", 
                           zonesNA = "berge_fluvial",
                           en = "Soft without cliff", 
                           descr_en = "Unconsolidated deposit without cliff", 
                           type_en = "Natural areas")
  habitat$meuble_sans_falaise <- uid(dat, "MSF", "Type_Berge", clip = FALSE)
  sup <- c(sup, line_length(dat, "MSF", "Type_Berge"))

  # -----
  meta_temp <- meta_update(meta_temp, 
                           dat = nm, 
                           accr = "rocheuse_sans_falaise", 
                           fr = "Rocheux sans falaise", 
                           descr = "Roche consolidée sans falaise", 
                           type = "Milieux naturels", 
                           zonesNA = "berge_fluvial",
                           en = "Rocky without bluff", 
                           descr_en = "Rocky bank without bluff", 
                           type_en = "Natural areas")
  habitat$rocheuse_sans_falaise <- uid(dat, "RSF", "Type_Berge", clip = FALSE)
  sup <- c(sup, line_length(dat, "RSF", "Type_Berge"))

  # -----
  meta_temp <- meta_update(meta_temp, 
                           dat = nm, 
                           accr = "rocheuse_sans_escarpement", 
                           fr = "Rocheux sans escarpement", 
                           descr = "Berge de roc sans escarpement", 
                           type = "Milieux naturels", 
                           zonesNA = "berge_fluvial",
                           en = "Rocky without cliff", 
                           descr_en = "Consolidated rock without cliff", 
                           type_en = "Natural areas")
  habitat$rocheuse_sans_escarpement <- uid(dat, "RSE", "Type_Berge", clip = FALSE)
  sup <- c(sup, line_length(dat, "RSE", "Type_Berge"))

  # -----
  meta_temp <- meta_update(meta_temp, 
                           dat = nm, 
                           accr = "terrasse_fluviale", 
                           fr = "Terrasse fluviale", 
                           descr = "Banc d'accumulation situé à l'embouchure d'une rivière composé de dépôts non consolidés colonisé par de la végétation", 
                           type = "Milieux naturels", 
                           zonesNA = "berge_fluvial",
                           en = "Beach terrace", 
                           descr_en = "Accumulation area of unconsolidated deposits (sand and/or coastal gravel) formed by a flat colonized by herbaceous vegetation, sometimes flooded only during higher high water high tide. The flat is sometimes bordered on its lower part by an erosional bluff (microcliff) of less than 2 m in height and on its upper part by a dead cliff or a flat terrain", 
                           type_en = "Natural areas")
  habitat$terrasse_fluviale <- uid(dat, "TF", "Type_Berge", clip = FALSE)
  sup <- c(sup, line_length(dat, "TF", "Type_Berge"))

  # -----
  meta_temp <- meta_update(meta_temp, 
                           dat = nm, 
                           accr = "terrasse_plage", 
                           fr = "Terrasse de plage", 
                           descr = "Zone d'accumulation de dépôts non consolidés (sable et/ou de gravier littoral) formée d’un replat colonisé par de la végétation herbacée parfois inondée que lors de surcote pendant la pleine mer supérieure de grande marée. Le replat est parfois bordé sur sa partie inférieure par un talus d’érosion (microfalaise) de moins de 2 m de hauteur et sur sa partie supérieure par une falaise morte ou d’un terrain plat", 
                           type = "Milieux naturels", 
                           zonesNA = "berge_fluvial",
                           en = "Fluvial terrace", 
                           descr_en = "Accumulation bank located at the mouth of a river composed of unconsolidated deposits colonized by vegetation", 
                           type_en = "Natural areas")
  habitat$terrasse_plage <- uid(dat, "TP", "Type_Berge", clip = FALSE)
  sup <- c(sup, line_length(dat, "TP", "Type_Berge"))


  # ------------------------------------------------------
  # Espèces fauniques à statut : 0059
  # Qualité des données - à retirer
  qu <- c("D (Faible, non viable)", "F (Non retrouvée)", "H (Historique)",
          "NR (Non attribuée)", "U (Non cartographiable)", "X (Extirpée)")
  nm <- c("0059")
  data0059 <- load_temp(nm) %>%
         filter(!EORANKDESC %in% qu) %>%
         filter(LASTOBSyear >= 2000) %>%
         filter(LOIEMV %in% c("Susceptible","Vulnérable","Menacée"))

  # Diviser par statut
  # Susceptible
  dat2 <- filter(data0059, LOIEMV == "Susceptible")
  meta_temp <- meta_update(meta_temp, 
                           dat = nm, 
                           accr = "faune_susceptible", 
                           fr = "Espèces fauniques susceptibles", 
                           descr = "Espèces fauniques susceptibles d'être désignées menacées ou vulnérables", 
                           type = "Espèces à statut",
                           en = "Susceptible wildlife species", 
                           descr_en = "Wildlife species likely to be designated as threatened or vulnerable", 
                           type_en = "Species at risk")
  habitat$faune_susceptible <- uid(dat2)
  sup <- c(sup, superficie(dat2))

  # Diviser par statut
  # Vulnérable
  dat2 <- filter(data0059, LOIEMV == "Vulnérable")
  meta_temp <- meta_update(meta_temp, 
                           dat = nm, 
                           accr = "faune_vulnerable", 
                           fr = "Espèces fauniques vulnérables", 
                           descr = "Espèces fauniques désignées vulnérables", 
                           type = "Espèces à statut",
                           en = "Vulnerable wildlife species", 
                           descr_en = "Wildlife species designated as vulnerable", 
                           type_en = "Species at risk")
  habitat$faune_vulnerable <- uid(dat2)
  sup <- c(sup, superficie(dat2))

  # Diviser par statut
  # Menacée
  dat2 <- filter(data0059, LOIEMV == "Menacée")
  meta_temp <- meta_update(meta_temp, 
                           dat = nm, 
                           accr = "faune_menacee", 
                           fr = "Espèces fauniques menacées", 
                           descr = "Espèces fauniques désignées menacées", 
                           type = "Espèces à statut",
                           en = "Threatened wildlife species", 
                           descr_en = "Wildlife species designated as threatened", 
                           type_en = "Species at risk")
  habitat$faune_menacee <- uid(dat2)
  sup <- c(sup, superficie(dat2))

  # ------------------------------------------------------
  # Espèces floristiques à statut : 0060
  # Qualité des données - à retirer
  qu <- c("D (Faible, non viable)", "F (Non retrouvée)", "H (Historique)",
          "NR (Non attribuée)", "U (Non cartographiable)", "X (Extirpée)")
  nm <- c("0060")
  data0060 <- load_temp(nm) %>%
         filter(!EORANKDESC %in% qu) %>%
         filter(LASTOBSyear >= 2000) %>%
         filter(LOIEMV %in% c("Susceptible","Vulnérable","Menacée"))

  # Diviser par statut
  # Susceptible
  dat2 <- filter(data0060, LOIEMV == "Susceptible")
  meta_temp <- meta_update(meta_temp, 
                           dat = nm, 
                           accr = "flore_susceptible", 
                           fr = "Espèces floristiques susceptibles", 
                           descr = "Espèces floristiques susceptibles d'être désignées menacées ou vulnérables", 
                           type = "Espèces à statut",
                           en = "Susceptible plant species", 
                           descr_en = "Plant species likely to be designated as threatened or vulnerable", 
                           type_en = "Species at risk")
  habitat$flore_susceptible <- uid(dat2)
  sup <- c(sup, superficie(dat2))

  # Diviser par statut
  # Vulnérable
  dat2 <- filter(data0060, LOIEMV == "Vulnérable")
  meta_temp <- meta_update(meta_temp, 
                           dat = nm, 
                           accr = "flore_vulnerable", 
                           fr = "Espèces floristiques vulnérables", 
                           descr = "Espèces floristiques désignées vulnérables", 
                           type = "Espèces à statut",
                           en = "Vulnerable plant species", 
                           descr_en = "Plant species designated as vulnerable", 
                           type_en = "Species at risk")
  habitat$flore_vulnerable <- uid(dat2)
  sup <- c(sup, superficie(dat2))

  # Diviser par statut
  # Menacée
  dat2 <- filter(data0060, LOIEMV == "Menacée")
  meta_temp <- meta_update(meta_temp, 
                          dat = nm, 
                          accr = "flore_menacee", 
                          fr = "Espèces floristiques menacées", 
                          descr = "Espèces floristiques désignées menacées", 
                          type = "Espèces à statut",
                          en = "Threatened plant species", 
                          descr_en = "Plant species designated as threatened", 
                          type_en = "Species at risk")
  habitat$flore_menacee <- uid(dat2)
  sup <- c(sup, superficie(dat2))
  
  # ------------------------------------------------------
  # Species at risk LEP: 0077
  nm <- c("0077")
  data0077 <- load_temp(nm)
  
  # Retirer béluga puisqu'on le considère déjà dans la section mammifères marins
  data0077 <- dplyr::filter(
    data0077, 
    !Scientific_Name %in% "Delphinapterus leucas"
  )
  
  # Diviser par statut
  # Endangered
  dat2 <- filter(data0077, SARA_Status == "Endangered")
  meta_temp <- meta_update(meta_temp, 
                           dat = nm, 
                           accr = "lep_menacee", 
                           fr = "Espèces menacées", 
                           descr = "Espèces désignées menacées en vertu de la Loi sur les espèces en péril (LEP)", 
                           type = "Espèces à statut",
                           en = "Endangered species", 
                           descr_en = "Species listed as Endangered under the Species at Risk Act (SARA)", 
                           type_en = "Species at risk")
  habitat$lep_menacee <- uid(dat2)
  sup <- c(sup, superficie(dat2))

  # Threatened
  dat2 <- filter(data0077, SARA_Status == "Threatened")
  meta_temp <- meta_update(meta_temp, 
                           dat = nm, 
                           accr = "lep_voie_disparition", 
                           fr = "Espèces en voie de disparition", 
                           descr = "Espèces désignées en voie de disparition en vertu de la Loi sur les espèces en péril (LEP)", 
                           type = "Espèces à statut",
                           en = "Threatened species", 
                           descr_en = "Species listed as Threatened under the Species at Risk Act (SARA)", 
                           type_en = "Species at risk")
  habitat$lep_voie_disparition <- uid(dat2)
  sup <- c(sup, superficie(dat2))
  
  # ------------------------------------------------------
  # Herbires aquatiques : 0084
  nm <- c("0084")
  dat <- load_temp(nm)
  # Diviser en biovolumes faible, modéré, élevés 
  quant <- quantile(dat$BioVolume, probs = c(0,0.33,0.66,1)) |> unname()
  dat_fbl <- dplyr::filter(dat, BioVolume <= quant[2])
  dat_mod <- dplyr::filter(dat, BioVolume > quant[2] & BioVolume <= quant[3])
  dat_elv <- dplyr::filter(dat, BioVolume > quant[3])
  
  # Biovolume faible
  meta_temp <- meta_update(meta_temp, 
                           dat = nm, 
                           accr = "biovolume_herbier_faible", 
                           fr = "Biovolume herbiers aquatique - faible", 
                           descr = "Milieux à faible biovolume d'herbiers aquatiques", 
                           type = "Milieux naturels", 
                           en = "Grassland biovolume - low", 
                           descr_en = "Areas with low grassland biovolume", 
                           type_en = "Natural areas")
  habitat$biovolume_herbier_faible <- uid(dat_fbl)
  sup <- c(sup, superficie(dat_fbl))

  # Biovolume modéré
  meta_temp <- meta_update(meta_temp, 
                           dat = nm, 
                           accr = "biovolume_herbier_modere", 
                           fr = "Biovolume herbiers aquatique - modéré", 
                           descr = "Milieux à modéré biovolume d'herbiers aquatiques", 
                           type = "Milieux naturels", 
                           en = "Grassland biovolume - moderate", 
                           descr_en = "Areas with moderate grassland biovolume", 
                           type_en = "Natural areas")
  habitat$biovolume_herbier_modere <- uid(dat_mod)
  sup <- c(sup, superficie(dat_mod))

  # Biovolume élevé
  meta_temp <- meta_update(meta_temp, 
                           dat = nm, 
                           accr = "biovolume_herbier_eleve", 
                           fr = "Biovolume herbiers aquatique - élevé", 
                           descr = "Milieux à élevé biovolume d'herbiers aquatiques", 
                           type = "Milieux naturels", 
                           en = "Grassland biovolume - high", 
                           descr_en = "Areas with high grassland biovolume", 
                           type_en = "Natural areas")
  habitat$biovolume_herbier_eleve <- uid(dat_elv)
  sup <- c(sup, superficie(dat_elv))
  
  # Single grid cells cannot contain multiple categories. Modify accordingly.
  habitat <- dplyr::mutate(
    habitat,
    biovolume_herbier_modere = ifelse(biovolume_herbier_eleve == 1, 0, biovolume_herbier_modere),
    biovolume_herbier_faible = ifelse(biovolume_herbier_eleve == 1, 0, biovolume_herbier_faible)
  ) |>
  dplyr::mutate(
    biovolume_herbier_faible = ifelse(biovolume_herbier_modere == 1, 0, biovolume_herbier_faible)
  )
  # ------------------------------------------------------------------------- #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Update metadata
  # ----------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  meta <- load_metadata("int_cv_habitat")

  # <=~ - ~=> <=~ - ~=> <=~ - ~=> <=~ - ~=> #
  meta$rawData <- meta_temp$rawData
  meta$dataDescription$categories$accronyme <- meta_temp$accronyme
  meta$dataDescription$categories$francais <- meta_temp$francais
  meta$dataDescription$categories$source <- meta_temp$source
  meta$dataDescription$categories$superficie <- sup
  meta$dataDescription$categories$type <- meta_temp$type
  meta$dataDescription$categories$description <- meta_temp$description
  meta$dataDescription$categories$zonesNA <- meta_temp$zonesNA
  meta$dataDescription$categories$english <- meta_temp$english
  meta$dataDescription$categories$type_en <- meta_temp$type_en
  meta$dataDescription$categories$description_en <- meta_temp$description_en

  # --- For proper referencing in markdown syntax
  meta$dataDescription$categories$mdref <- modif_md(meta$dataDescription$categories$accronyme)

  # -----
  temp <- st_drop_geometry(habitat) %>%
          rowSums(na.rm = TRUE)
  temp <- habitat[temp > 0, ]
  meta$dataDescription$spatial$extent <- st_bbox(temp)

  # <=~ - ~=> <=~ - ~=> <=~ - ~=> <=~ - ~=> #
  # ----- Info for frayères et aires d'alevinage
  # <=~ - ~=> <=~ - ~=> <=~ - ~=> <=~ - ~=> #
  # --- Alevinage
  # Nombre
  alevinage <- as.data.frame(table(data0086$Source), stringsAsFactors = FALSE)
  colnames(alevinage) <- c("Source","Nombre_alevinage")

  # Superficie
  alevinage$Superficie_alevinage <- 0
  for(i in 1:nrow(alevinage)) {
    dat <- data0086[data0086$Source == alevinage[i,"Source"], ] %>%
           st_union() %>%
           st_area() %>%
           as.numeric(.) * 1e-6 %>%
           sum()
    alevinage$Superficie_alevinage[i] <- dat
  }

  # --- Frayères
  # Nombre
  frayere <- as.data.frame(table(data0087$Source), stringsAsFactors = FALSE)
  colnames(frayere) <- c("Source","Nombre_frayere")

  # Superficie
  frayere$Superficie_frayere <- 0
  for(i in 1:nrow(frayere)) {
    dat <- data0087[data0087$Source == frayere[i,"Source"], ] %>%
           st_union() %>%
           st_area() %>%
           as.numeric(.) * 1e-6 %>%
           sum()
    frayere$Superficie_frayere[i] <- dat
  }

  # --- Frayères update 2023
  # Nombre
  maj_frayere <- bind_rows(data0073,data0074,data0075,data0076)
  frayere2 <- as.data.frame(table(maj_frayere$REFERENCE), stringsAsFactors = FALSE)
  colnames(frayere2) <- c("Source","Nombre_frayere")

  # Superficie
  frayere2$Superficie_frayere <- 0
  for(i in 1:nrow(frayere2)) {
    dat <- maj_frayere[maj_frayere$REFERENCE == frayere2[i,"Source"], ] %>%
           st_union() %>%
           st_area() %>%
           as.numeric(.) * 1e-6 %>%
           sum()
    frayere2$Superficie_frayere[i] <- dat
  }
  
  # -----
  frayere <- bind_rows(frayere, frayere2)

  # -----
  dat <- left_join(frayere, alevinage, by = "Source")
  iid <- !alevinage$Source %in% dat$Source
  dat <- bind_rows(dat, alevinage[iid,])


  # -----
  meta$dataDescription$frayere_alevinage$Source <- dat$Source
  meta$dataDescription$frayere_alevinage$Nombre_frayere <- dat$Nombre_frayere
  meta$dataDescription$frayere_alevinage$Superficie_frayere <- dat$Superficie_frayere
  meta$dataDescription$frayere_alevinage$Nombre_alevinage <- dat$Nombre_alevinage
  meta$dataDescription$frayere_alevinage$Superficie_alevinage <- dat$Superficie_alevinage

  # <=~ - ~=> <=~ - ~=> <=~ - ~=> <=~ - ~=> #
  # --- ZICOs
  # <=~ - ~=> <=~ - ~=> <=~ - ~=> <=~ - ~=> #
  load_format("data0058")
  uid <- st_intersects(grid1p, data0058) %>% unlist() %>% unique()
  nid <- length(uid)
  sup <- st_area(data0058[uid, ]) %>%
         sum() %>%
         units::set_units(km^2) %>%
         round(2) %>%
         as.numeric()

  # -----
  meta$dataDescription$ZICO$Nombre <- nid
  meta$dataDescription$ZICO$Superficie <- sup

  # <=~ - ~=> <=~ - ~=> <=~ - ~=> <=~ - ~=> #
  # --- BIOMQ
  # <=~ - ~=> <=~ - ~=> <=~ - ~=> <=~ - ~=> #
  load_format("data0043")
  uid <- st_intersects(grid1p, data0043) %>% unlist() %>% unique()
  nid <- length(uid)
  sup <- st_area(data0043[uid, ]) %>%
         sum() %>%
         units::set_units(km^2) %>%
         round(2) %>%
         as.numeric()

  # -----
  meta$dataDescription$BIOMQ$Nombre <- nid
  meta$dataDescription$BIOMQ$Superficie <- sup

  # <=~ - ~=> <=~ - ~=> <=~ - ~=> <=~ - ~=> #
  # --- Faune à statut
  # <=~ - ~=> <=~ - ~=> <=~ - ~=> <=~ - ~=> #
  uid <- st_intersects(grid1p, data0059) %>% unlist() %>% unique() %>% sort()
  dat <- data0059[uid, ] %>%
         mutate(area = units::set_units(st_area(.), km^2)) %>%
         st_drop_geometry()

  nFauneSp <- length(unique(dat$SNAME))
  nFauneSite <- nrow(dat)

  datFaune <- dat %>%
         group_by(LOIEMV) %>%
         summarize(species = length(unique(SNAME)),
                   area = round(as.numeric(sum(area)),2)) %>%
         mutate(Type = "Faune")
         
  datFauneSP <- dat %>%
                group_by(LOIEMV) %>%
                summarize(species = unique(SNAME), 
                          common = unique(SCOMNAME)) %>%
                mutate(common = str_to_sentence(common)) 

  # <=~ - ~=> <=~ - ~=> <=~ - ~=> <=~ - ~=> #
  # --- Flore à statut
  # <=~ - ~=> <=~ - ~=> <=~ - ~=> <=~ - ~=> #
  uid <- st_intersects(grid1p, data0060) %>% unlist() %>% unique() %>% sort()
  dat <- data0060[uid, ] %>%
         mutate(area = units::set_units(st_area(.), km^2)) %>%
         st_drop_geometry()

  nFloreSp <- length(unique(dat$SNAME))
  nFloreSite <- nrow(dat)

  datFlore <- dat %>%
         group_by(LOIEMV) %>%
         summarize(species = length(unique(SNAME)),
                   area = round(as.numeric(sum(area)),2)) %>%
         mutate(Type = "Flore")

  datFloreSP <- dat %>%
                group_by(LOIEMV) %>%
                summarize(species = unique(SNAME), 
                          common = unique(SCOMNAME)) %>%
                mutate(common = str_to_sentence(common)) 

  # ----
  dat <- rbind(datFaune, datFlore)

  # -----
  meta$dataDescription$CDPNQ$NombreFaune <- nFauneSite
  meta$dataDescription$CDPNQ$EspecesFaune <- nFauneSp
  meta$dataDescription$CDPNQ$NombreFlore <- nFloreSite
  meta$dataDescription$CDPNQ$EspecesFlore <- nFloreSp
  meta$dataDescription$CDPNQ$details$type <- dat$Type
  meta$dataDescription$CDPNQ$details$loiemv <- dat$LOIEMV
  meta$dataDescription$CDPNQ$details$species <- dat$species
  meta$dataDescription$CDPNQ$details$area <- dat$area
  meta$dataDescription$CDPNQ$Faune$Statut <- datFauneSP$LOIEMV
  meta$dataDescription$CDPNQ$Faune$Scientific <- datFauneSP$species
  meta$dataDescription$CDPNQ$Faune$Common <- datFauneSP$common
  meta$dataDescription$CDPNQ$Flore$Statut <- datFloreSP$LOIEMV
  meta$dataDescription$CDPNQ$Flore$Scientific <- datFloreSP$species
  meta$dataDescription$CDPNQ$Flore$Common <- datFloreSP$common
  
  # <=~ - ~=> <=~ - ~=> <=~ - ~=> <=~ - ~=> #
  # --- Species at Risk (SARA/LEP)
  # <=~ - ~=> <=~ - ~=> <=~ - ~=> <=~ - ~=> #
  uid <- st_intersects(grid1p, data0077) %>% unlist() %>% unique() %>% sort()
  dat <- data0077[uid, ] %>%
         mutate(area = units::set_units(st_area(.), km^2)) %>%
         st_drop_geometry()

  nSp <- length(unique(dat$Scientific_Name))
  nSite <- nrow(dat)

  datLEP <- dat %>%
         group_by(SARA_Status) %>%
         summarize(species = length(unique(Scientific_Name)),
                   area = round(as.numeric(sum(area)),2)) |>
         mutate(LEP_Statut = SARA_Status) |>
         mutate(
           LEP_Statut = stringr::str_replace(LEP_Statut, "Endangered","Menacée"),
           LEP_Statut = stringr::str_replace(LEP_Statut, "Threatened","En voie de disparition")
         )
         
  datSp <- dat %>%
    group_by(SARA_Status) %>%
    summarize(species = unique(Scientific_Name), 
              commonEn = unique(Common_Name_EN),
              commonFr = unique(Common_Name_FR)) %>%
    mutate(
      LEP_Statut = SARA_Status,
      commonEn = str_to_sentence(commonEn),
      commonFr = str_to_sentence(commonFr),
      LEP_Statut = stringr::str_replace(LEP_Statut, "Endangered","Menacée"),
      LEP_Statut = stringr::str_replace(LEP_Statut, "Threatened","En voie de disparition")
    )

  # -----
  meta$dataDescription$LEP$NombreSites <- nSite
  meta$dataDescription$LEP$NombreEspeces <- nSp
  meta$dataDescription$LEP$details$LEP <- datLEP$LEP_Statut
  meta$dataDescription$LEP$details$SARA <- datLEP$SARA_Status
  meta$dataDescription$LEP$details$species <- datLEP$species
  meta$dataDescription$LEP$details$area <- datLEP$area
  meta$dataDescription$LEP$Especes$LEP <- datSp$LEP_Statut
  meta$dataDescription$LEP$Especes$SARA <- datSp$SARA_Status
  meta$dataDescription$LEP$Especes$Scientific <- datSp$species
  meta$dataDescription$LEP$Especes$Common_Fr <- datSp$commonFr
  meta$dataDescription$LEP$Especes$Common_En <- datSp$commonEn

  # <=~ - ~=> <=~ - ~=> <=~ - ~=> <=~ - ~=> #
  # --- Zosteres
  # <=~ - ~=> <=~ - ~=> <=~ - ~=> <=~ - ~=> #
  # -----
  load_format("data0001")
  uid <- st_intersects(grid1p, data0001) %>% unlist() %>% unique() %>% sort()
  dat <- data0001[uid, ] %>%
         mutate(area = as.numeric(units::set_units(st_area(.), km^2))) %>%
         st_drop_geometry()
  meta$dataDescription$zostere$mpo$sites <- nrow(dat)
  meta$dataDescription$zostere$mpo$superficie <- round(sum(dat$area),2)

  # -----
  load_format("data0002")
  dat <- data0002 %>%
         mutate(area = units::set_units(st_area(.), km^2)) %>%
         st_drop_geometry()
  meta$dataDescription$zostere$pop$sites <- nrow(dat)
  sup <- as.numeric(sum(dat$area))
  meta$dataDescription$zostere$pop$superficie <- ifelse(sup < 1, "< 1", round(sup,2))

  # -----
  load_format("data0003")
  dat <- data0003 %>%
         mutate(area = units::set_units(st_area(.), km^2)) %>%
         st_drop_geometry()
  meta$dataDescription$zostere$metis$sites <- nrow(dat)
  sup <- as.numeric(sum(dat$area))
  meta$dataDescription$zostere$metis$superficie <- ifelse(sup < 1, "< 1", round(sup,2))
  # _____________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # -----
  write_yaml(meta, "./data/data-metadata/int_cv_habitat.yml")

  # -----
  st_write(obj = habitat,
           dsn = "./data/data-integrated/cv_habitat.geojson",
           delete_dsn = TRUE,
           quiet = TRUE)
  # ------------------------------------------------------------------------- #}

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Clean global environment
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  clean()
  # ------------------------------------------------------------------------- #}
}
