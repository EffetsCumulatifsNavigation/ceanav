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
  meta_update <- function(meta, dat, accr, fr, descr = "", type = "", zonesNA = NA) {
    meta$rawData <- c(meta$rawData, dat)
    meta$accronyme <- c(meta$accronyme, accr)
    meta$type <- c(meta$type, type)
    meta$francais <- c(meta$francais, fr)
    meta$source <- c(meta$source, paste0(dat, collapse = ","))
    # meta$superficie <- c(meta$superficie, superficie(dat))
    meta$description <- c(meta$description, descr)
    meta$zonesNA <- c(meta$zonesNA, zonesNA)
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
  meta_temp <- meta_update(meta_temp, nm, "zostere", "Zostères", descr = "Distribution de la zostère marine (*Zostera marina*)", type = "Milieux naturels")
  habitat$zostere <- uid(dat)
  sup <- c(sup, superficie(dat))

  # ------------------------------------------------------
  # Zones inondables : 0013, 0014
  nm <- c("0013", "0014")
  dat <- load_temp(nm)
  meta_temp <- meta_update(meta_temp, nm, "zone_inondable", "Zones inondables", descr = "Territoires ayant une probabilité élevée d'être inondés selon une récurrence des crues sur 2 ans, 20 ans ou sur 100 ans", type = "Milieux naturels")
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
  meta_temp <- meta_update(meta_temp, nm, "eau_peu_profonde", "Eau peu profonde", "Milieu humide dont le niveau d’eau est inférieur à 2 m et présentant des plantes aquatiques flottantes ou submergées ainsi que des plantes émergentes dont le couvert fait moins de 25 % de la superficie du milieu.", type = "Milieux naturels")
  habitat$eau_peu_profonde <- uid(dat, "Eau", "TYPE", clip = TRUE)
  sup <- c(sup, superficie(dat, "Eau", "TYPE"))

  # -----
  # Marais
  meta_temp <- meta_update(meta_temp, nm, "marais", "Marais", "Milieu humide sur dépôt minéral, dominé par une végétation herbacée couvrant plus de 25 % de la superficie. Les arbustes et les arbres, lorsque présents, couvrent moins de 25 % de la superficie du milieu.", type = "Milieux naturels")
  habitat$marais <- uid(dat, "Marais", "TYPE", clip = TRUE)
  sup <- c(sup, superficie(dat, "Marais", "TYPE"))



  # -----
  # Marécage
  meta_temp <- meta_update(meta_temp, nm, "marecage", "Marécage", "Milieu humide sur dépôt minéral, dominé par une végétation ligneuse arbustive ou arborescente, avec plus de 25 % de couvert.", type = "Milieux naturels")
  habitat$marecage <- uid(dat, "Marécage", "TYPE", clip = TRUE)
  sup <- c(sup, superficie(dat, "Marécage", "TYPE"))


  # -----
  # Milieu humide
  meta_temp <- meta_update(meta_temp, nm, "milieu_humide", "Milieu humide", "Regroupe les milieux humides dont le type est inconnu.", type = "Milieux naturels")
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
  nm <- "0009"
  dat <- load_temp(nm)
  meta_temp <- meta_update(meta_temp, nm, "site_alevinage", "Sites d'alevinage", descr = "Sites d'alimentation et de protection pour les stades de vie initiaux des poissons (*e.g.* larves, alevins, juvéniles)", type = "Cycles de vie")
  habitat$site_alevinage <- uid(dat)
  sup <- c(sup, superficie(dat))

  # ------------------------------------------------------
  # Frayères : 0010
  nm <- "0010"
  dat <- load_temp(nm)
  meta_temp <- meta_update(meta_temp, nm, "frayere", "Frayères", descr = "Sites de reproduction où des poissons femelles pondent des oeufs pour fécondation par les mâles", type = "Cycles de vie")
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
  meta_temp <- meta_update(meta_temp, nm, "oiseaux", "Oiseaux", descr = "Sites connus d'importance pour les oiseaux, tels des sites de nidification et des colonies d'oiseaux marins", type = "Cycles de vie")
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
  # Pétoncle uniquement dans l'estuaire, donc considation de la mactre uniquement (0057)
  nm <- "0057"
  dat <- load_temp(nm)
  meta_temp <- meta_update(meta_temp, nm, "gisement_coquilliers", "Gisements coquilliers", descr = "Gisements connus et exploités de mactre de Stimpson des eaux côtières du Québec", type = "Milieux naturels")
  habitat$gisement_coquilliers <- uid(dat)
  sup <- c(sup, superficie(dat))


  # ------------------------------------------------------
  # Habitats côtiers : 0017
  # Nous avons sélectionné les habitats côtiers qui touchent à l'eau. Nous n'avons pas considéré les marais maritimes et les milieux humides riverains, qui sont déjà couverts par la base de données 0053
  # -----
  nm <- c("0017")
  dat <- load_temp(nm)
  meta_temp <- meta_update(meta_temp, nm, "meuble_sans_falaise", "Meuble sans falaise", "Dépôt non consolidé sans falaise", type = "Milieux naturels")
  habitat$meuble_sans_falaise <- uid(dat, "MSF", "Type_Berge", clip = FALSE)
  sup <- c(sup, line_length(dat, "MSF", "Type_Berge"))

  # -----
  meta_temp <- meta_update(meta_temp, nm, "rocheuse_sans_falaise", "Rocheux sans falaise", "Roche consolidée sans falaise", type = "Milieux naturels")
  habitat$rocheuse_sans_falaise <- uid(dat, "RSF", "Type_Berge", clip = FALSE)
  sup <- c(sup, line_length(dat, "RSF", "Type_Berge"))

  # -----
  meta_temp <- meta_update(meta_temp, nm, "rocheuse_sans_escarpement", "Rocheux sans escarpement", "Berge de roc sans escarpement", type = "Milieux naturels")
  habitat$rocheuse_sans_escarpement <- uid(dat, "RSE", "Type_Berge", clip = FALSE)
  sup <- c(sup, line_length(dat, "RSE", "Type_Berge"))

  # -----
  meta_temp <- meta_update(meta_temp, nm, "terrasse_fluviale", "Terrasse fluviale", "Banc d'accumulation situé à l'embouchure d'une rivière composé de dépôts non consolidés colonisé par de la végétation", type = "Milieux naturels")
  habitat$terrasse_fluviale <- uid(dat, "TF", "Type_Berge", clip = FALSE)
  sup <- c(sup, line_length(dat, "TF", "Type_Berge"))

  # -----
  meta_temp <- meta_update(meta_temp, nm, "terrasse_plage", "Terrasse de plage", "Zone d'accumulation de dépôts non consolidés (sable et/ou de gravier littoral) formée d’un replat colonisé par de la végétation herbacée parfois inondée que lors de surcote pendant la pleine mer supérieure de grande marée. Le replat est parfois bordé sur sa partie inférieure par un talus d’érosion (microfalaise) de moins de 2 m de hauteur et sur sa partie supérieure par une falaise morte ou d’un terrain plat", type = "Milieux naturels")
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
  meta_temp <- meta_update(meta_temp, nm, "faune_susceptible", "Espèces fauniques susceptibles", "Espèces fauniques susceptibles d'être désignées menacées ou vulnérables", type = "Espèces à statut")
  habitat$faune_susceptible <- uid(dat2)
  sup <- c(sup, superficie(dat2))

  # Diviser par statut
  # Vulnérable
  dat2 <- filter(data0059, LOIEMV == "Vulnérable")
  meta_temp <- meta_update(meta_temp, nm, "faune_vulnerable", "Espèces fauniques vulnérables", "Espèces fauniques désignées vulnérables", type = "Espèces à statut")
  habitat$faune_vulnerable <- uid(dat2)
  sup <- c(sup, superficie(dat2))

  # Diviser par statut
  # Menacée
  dat2 <- filter(data0059, LOIEMV == "Menacée")
  meta_temp <- meta_update(meta_temp, nm, "faune_menacee", "Espèces fauniques menacées", "Espèces fauniques désignées menacées", type = "Espèces à statut")
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
  meta_temp <- meta_update(meta_temp, nm, "flore_susceptible", "Espèces floristiques susceptibles", "Espèces floristiques susceptibles d'être désignées menacées ou vulnérables", type = "Espèces à statut")
  habitat$flore_susceptible <- uid(dat2)
  sup <- c(sup, superficie(dat2))

  # Diviser par statut
  # Vulnérable
  dat2 <- filter(data0060, LOIEMV == "Vulnérable")
  meta_temp <- meta_update(meta_temp, nm, "flore_vulnerable", "Espèces floristiques vulnérables", "Espèces floristiques désignées vulnérables", type = "Espèces à statut")
  habitat$flore_vulnerable <- uid(dat2)
  sup <- c(sup, superficie(dat2))

  # Diviser par statut
  # Menacée
  dat2 <- filter(data0060, LOIEMV == "Menacée")
  meta_temp <- meta_update(meta_temp, nm, "flore_menacee", "Espèces floristiques menacées", "Espèces floristiques désignées menacées", type = "Espèces à statut")
  habitat$flore_menacee <- uid(dat2)
  sup <- c(sup, superficie(dat2))
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
  alevinage <- as.data.frame(table(data0009$Source), stringsAsFactors = FALSE)
  colnames(alevinage) <- c("Source","Nombre_alevinage")

  # Superficie
  alevinage$Superficie_alevinage <- 0
  for(i in 1:nrow(alevinage)) {
    dat <- data0009[data0009$Source == alevinage[i,"Source"], ] %>%
           st_union() %>%
           st_area() %>%
           as.numeric(.) * 1e-6 %>%
           sum()
    alevinage$Superficie_alevinage[i] <- dat
  }

  # --- Frayères
  # Nombre
  frayere <- as.data.frame(table(data0010$Source), stringsAsFactors = FALSE)
  colnames(frayere) <- c("Source","Nombre_frayere")

  # Superficie
  frayere$Superficie_frayere <- 0
  for(i in 1:nrow(frayere)) {
    dat <- data0010[data0010$Source == frayere[i,"Source"], ] %>%
           st_union() %>%
           st_area() %>%
           as.numeric(.) * 1e-6 %>%
           sum()
    frayere$Superficie_frayere[i] <- dat
  }

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
