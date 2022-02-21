#' Pêches commerciales
#'
#' Couche de données transformées pour les pêches commerciales dans le Saint-Laurent
#'
#' @keywords pêche commerciale
#' @keywords stresseurs
#'
#' @export
#'
#' @details Cette fonction importe et formatte les données pour l'analyse d'effets cumulatifs
#'

st_peche_commerciale <- function() {
  # Load gear type dataset and fishing data
  load_format("data0033")
  load_format("data0034")
  load_format("data0035")
  data_metadata <- c("0033","0034","0035")

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Classify gear types
  # -------------------
  # NOTE:
  #
  # Fishing activities are performed using a variety of gears types, e.g. trap,
  # trawl, dredge, driftnet, hand line, longline, scuba diving, purse seine, seine,
  # beach seine and jig fishing. Intensity of fishing activities was divided among
  # gear types and based on their respective types of environmental impacts.
  # Gear classification is done using the classification presented in Halpern
  # et al. (2008) and Halpern et al. (2015a) and is broken down into 5 distinct
  # classes:
  #
  #  - demersal destructive (DD),
  #  - demersal, non-destructive, low-bycatch (DNL),
  #  - demersal, non-destructive, high-bycatch (DNH),
  #  - pelagic, low-bycatch (PLB),
  #  - pelagic, high-bycatch (PHB),
  #  - hunting (HN)
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  data0034$gearClass <- ""
  data0034$gearClass[data0034$Codes == 0] <- NA # Valeur manquante","Null data",""
  data0034$gearClass[data0034$Codes == 1] <- NA # Engin fixe","Fixed gear","F"
  data0034$gearClass[data0034$Codes == 2] <- NA # Engin mobile","Mobile gear","M"
  data0034$gearClass[data0034$Codes == 3] <- "DNH" # Casier","Trap","F"
  data0034$gearClass[data0034$Codes == 4] <- "PHB" # Filet maillant","Gill net","F"
  data0034$gearClass[data0034$Codes == 5] <- "PHB" # Filet maillant et ligne à main","Gill net and hand line","F"
  data0034$gearClass[data0034$Codes == 6] <- "DD" # Drague","Dredge","M"
  data0034$gearClass[data0034$Codes == 9] <- "DD" # Chalut à perche pour la crevette","Shrimp beam trawl",""
  data0034$gearClass[data0034$Codes == 10] <- "DD" # "Chalut de fond à panneaux (indéterminé)","Bottom otter trawl (unspecified)","M"
  data0034$gearClass[data0034$Codes == 11] <- "DD" # "Chalut de fond à panneaux (de côté)","Bottom otter trawl (side)","M"
  data0034$gearClass[data0034$Codes == 12] <- "DD" # "Chalut de fond à panneaux (arrière)","Bottom otter trawl (stern)","M"
  data0034$gearClass[data0034$Codes == 13] <- "PHB" # "Chalut mésopélagique (indéterminé)","Midwater trawl (unspecified)","M"
  data0034$gearClass[data0034$Codes == 14] <- "PHB" # "Chalut mésopélagique (de côté)","Midwater trawl (side)","M"
  data0034$gearClass[data0034$Codes == 15] <- "PHB" # "Chalut mésopélagique (arrière)","Midwater trawl (stern)","M"
  data0034$gearClass[data0034$Codes == 16] <- "DD" # "Chalut de fond boeuf","Bottom pair trawl","M"
  data0034$gearClass[data0034$Codes == 17] <- "PHB" # "Chalut pélagique boeuf","Midwater pair trawl","M"
  data0034$gearClass[data0034$Codes == 18] <- "PHB" # "Chalut semi-pélagique","Semi-midwater trawl","M"
  data0034$gearClass[data0034$Codes == 19] <- "DD" # "Chalut à crevettes","Shrimp trawl","M"
  data0034$gearClass[data0034$Codes == 21] <- "DNH" # "Seine danoise","Danish seine","M"
  data0034$gearClass[data0034$Codes == 22] <- "DNH" # "Seine écossaise","Scottish seine","M"
  data0034$gearClass[data0034$Codes == 24] <- "DNH" # "Seine de plage","Beach and bar seine","F"
  data0034$gearClass[data0034$Codes == 25] <- "DNH" # "Senne de plage modifiée","Tuck seine","F"
  data0034$gearClass[data0034$Codes == 27] <- "PHB" # "Filet maillant turbot","Turbot gill net","F"
  data0034$gearClass[data0034$Codes == 28] <- "PHB" # "Filet maillant morue","Cod gill net","F"
  data0034$gearClass[data0034$Codes == 29] <- "PHB" # "Filet maillant maquereau","Mackerel gill net","F"
  data0034$gearClass[data0034$Codes == 30] <- "PHB" # "Filet maillant hareng","Herring gill net","F"
  data0034$gearClass[data0034$Codes == 31] <- "PLB" # "Seine bourse","Purse seine","M"
  data0034$gearClass[data0034$Codes == 32] <- "PHB" # "Lampara","Lampara","M"
  data0034$gearClass[data0034$Codes == 33] <- "DNH" # "Seine coulissante (boeuf)","Pair seine","M"
  data0034$gearClass[data0034$Codes == 34] <- "PHB" # "Filet maillant plie","Plaice gill net","F"
  data0034$gearClass[data0034$Codes == 35] <- "PHB" # "Filet dérivant hareng","Herring drift net","F"
  data0034$gearClass[data0034$Codes == 36] <- "PHB" # "Filet dérivant maquereau","Mackerel drift net","F"
  data0034$gearClass[data0034$Codes == 37] <- "PHB" # "Palangre morue","Cod - longline","F"
  data0034$gearClass[data0034$Codes == 38] <- "PHB" # "Palangre plie","Plaice - longline","F"
  data0034$gearClass[data0034$Codes == 39] <- "PHB" # "Palangre  flétan","Turbot  - longline","F"
  data0034$gearClass[data0034$Codes == 40] <- "PHB" # "Palangre  thon","Tuna - longline","F"
  data0034$gearClass[data0034$Codes == 41] <- "PHB" # "Filet maillant (fixe)","Gillnet (set or fixed)","F"
  data0034$gearClass[data0034$Codes == 42] <- "PHB" # "Filet maillant (dérivant)","Gillnet (drift)","F"
  data0034$gearClass[data0034$Codes == 43] <- "PHB" # "Filet maillant (indéterminé)","Gillnet (unspecified)","F"
  data0034$gearClass[data0034$Codes == 44] <- "PHB" # "Carrelet","Square net","F"
  data0034$gearClass[data0034$Codes == 45] <- "DNH" # "Parc fermé","Box net",""
  data0034$gearClass[data0034$Codes == 46] <- "DNH" # "Filet à poche","Bag net",""
  data0034$gearClass[data0034$Codes == 47] <- "DNH" # "Verveux","Fyke net",""
  data0034$gearClass[data0034$Codes == 48] <- "PHB" # "Filet haut fond","Shoal net","F"
  data0034$gearClass[data0034$Codes == 50] <- "PHB" # "Palangrotte","Setheared hooks","F"
  data0034$gearClass[data0034$Codes == 51] <- "PHB" # "Palangre","Longline","F"
  data0034$gearClass[data0034$Codes == 52] <- "PHB" # "Palangre à requin","Shark longline","F"
  data0034$gearClass[data0034$Codes == 53] <- "PLB" # "Turlutte","Jigger","F"
  data0034$gearClass[data0034$Codes == 54] <- "PLB" # "Ligne trainante","Troller lines","F"
  data0034$gearClass[data0034$Codes == 55] <- "PLB" # "Turlutte mécanique calmar","Mechanized squid jigger","F"
  data0034$gearClass[data0034$Codes == 56] <- "PLB" # "Turlutte  automatisée (ligne à main)","Automated jigger  (hand line) ","F"
  data0034$gearClass[data0034$Codes == 57] <- NA # "Dispositif mécanique (maquereau)","Mechanical device (mackerel)","F"
  data0034$gearClass[data0034$Codes == 58] <- "PLB" # "Canne et moulinet (amorçage)","Rod and reel (chumming)","F"
  data0034$gearClass[data0034$Codes == 59] <- "PLB" # "Ligne à main (appâtée)","Hand line (baited)","F"
  data0034$gearClass[data0034$Codes == 60] <- "PLB" # "Pêche à la ligne","Angling","F"
  data0034$gearClass[data0034$Codes == 61] <- "DNH" # "Trappe","Trap net","F"
  data0034$gearClass[data0034$Codes == 62] <- "DNH" # "Casier non spécifié","Pot ","F"
  data0034$gearClass[data0034$Codes == 63] <- "DNH" # "Fascine","Weir","F"
  data0034$gearClass[data0034$Codes == 64] <- "PHB" # "Carrelet fixe","Stationary lift nets","F"
  data0034$gearClass[data0034$Codes == 65] <- "DNH" # "Vivier à homard","Lobster pound","F"
  data0034$gearClass[data0034$Codes == 66] <- "DNH" # "Casier japonais","Japanese trap","F"
  data0034$gearClass[data0034$Codes == 67] <- "DNH" # "Casier rectangulaire","Rectangular trap","F"
  data0034$gearClass[data0034$Codes == 68] <- "DNH" # "Casier conique","Conical trap","F"
  data0034$gearClass[data0034$Codes == 69] <- "DNH" # "Casier pyramidal","Pyramidal trap","F"
  data0034$gearClass[data0034$Codes == 70] <- "DNL" # "Épuisette","Dip net",""
  data0034$gearClass[data0034$Codes == 71] <- "DD" # "Drague non spécifiée","Dredge (boat)",""
  data0034$gearClass[data0034$Codes == 72] <- "DD" # "Drague à main","Dredge (hand)","F"
  data0034$gearClass[data0034$Codes == 72] <- "DD" # "Râteau hydraulique","Hydraulic rake","M"
  data0034$gearClass[data0034$Codes == 73] <- NA # "Engin mécanisé","Automatic gear","F"
  data0034$gearClass[data0034$Codes == 74] <- "DD" # "Drague hydraulique à convoyeur","Hydraulic device","M"
  data0034$gearClass[data0034$Codes == 75] <- "DNL" # "Plongée avec outil manuel","Diving with hand tool","F"
  data0034$gearClass[data0034$Codes == 76] <- NA # "Câble","Rope",""
  data0034$gearClass[data0034$Codes == 77] <- "DD" # "Drague à concombre de mer","Sea Cucumber drag","M"
  data0034$gearClass[data0034$Codes == 78] <- "DNH" # "Casier à crabe commun (3 pieds)","Rock crab trap (3 feet)","F"
  data0034$gearClass[data0034$Codes == 79] <- "DNH" # "Casier à crabe commun (4 pieds)","Rock crab trap (4 feet)","F"
  data0034$gearClass[data0034$Codes == 80] <- "DNH" # "Casier conique (3 pieds)","Conical trap (3 feet)","F"
  data0034$gearClass[data0034$Codes == 81] <- "DNL" # "Harpon et lance","Harpoon and spear","M"
  data0034$gearClass[data0034$Codes == 82] <- "HN" # "Pêche (chasse) du phoque","Seal hunting","F"
  data0034$gearClass[data0034$Codes == 83] <- "DNL" # "Foène","Spear",""
  data0034$gearClass[data0034$Codes == 84] <- "DNH" # "Nasse à anguille","Eel pot","F"
  data0034$gearClass[data0034$Codes == 85] <- "DNL" # "Harpon électrique","Electric harpoon","M"
  data0034$gearClass[data0034$Codes == 86] <- "DNH" # "Baril de Myxine du nord","Hagfish Barrel","F"
  data0034$gearClass[data0034$Codes == 87] <- "DNH" # "Casier conique (4 pieds)","","F"
  data0034$gearClass[data0034$Codes == 88] <- "DNH" # "Casier à buccin (0,15 mètre cube et moins)","Trap net less than 0.15 m3","F"
  data0034$gearClass[data0034$Codes == 89] <- "DNH" # "Casier à buccin (0,16 à 0,30 mètre cube)","Trap net 0.16 m3 to 0.30 m3","F"
  data0034$gearClass[data0034$Codes == 90] <- NA # "Divers","Miscellaneous","F"
  data0034$gearClass[data0034$Codes == 91] <- "DNL" # "Râteaux et pinces","Rakes and tongs","F"
  data0034$gearClass[data0034$Codes == 92] <- "DNH" # "Viviers","Retaining Ponds","F"
  data0034$gearClass[data0034$Codes == 93] <- "DD" # "Râteau traînant","Drag rake","M"
  data0034$gearClass[data0034$Codes == 94] <- "DNL" # "Coupeur","Cutter",""
  data0034$gearClass[data0034$Codes == 95] <- NA # "Bateau à déchargement","Pumper",""
  data0034$gearClass[data0034$Codes == 96] <- "DNL" # "Main et outils à main","Hand and hand held tools","F"
  data0034$gearClass[data0034$Codes == 98] <- "DNH" # "Mélange de casiers","Mixed of trap - crab","F"
  data0034$gearClass[data0034$Codes == 99] <- NA # "Engin fixe non spécifié","Unspecified fixed gear","F"
  data0034$gearClass[data0034$Codes == 99] <- NA # "Engin mobile non spécifié","Unspecified mobile gear","M"
  # --------------------------------------------------------------------------------

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Classify gear mobility
  # ----------------------
  # NOTE:
  #
  # Gear types can also be further classified into fixed or mobile engines based
  # on their mobility. We used these two mobility classes to generate a buffer of
  # impact around each fishing activity coordinates to consider potential spatial
  # uncertainty associated with locations and the fact that mobile engines can be
  # tracted over several kilometers during fishing activities and that we do not
  # have the beginning and end points of mobile fishing events. Buffer sizes for
  # fixed (F) and mobile (M) engine is of 200 and 2000 meters, respectively.
  #
  # The index holds the mobility information for almost all gear types, but some
  # are missing. We manually add them here. Categories 81 and 85 were also changed
  # from mobile to fixed for the purposes of our analysis, even though it does not
  # affect the actual data we are working with because there are no observations of
  # those gear types in the study area for this project.
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  data0034$Categorie[data0034$Codes == 0] <- NA # Valeur manquante","Null data",""
  data0034$Categorie[data0034$Codes == 9] <- "M" # Chalut à perche pour la crevette","Shrimp beam trawl",""
  data0034$Categorie[data0034$Codes == 45] <- "F" # "Parc fermé","Box net",""
  data0034$Categorie[data0034$Codes == 46] <- "F" # "Filet à poche","Bag net",""
  data0034$Categorie[data0034$Codes == 47] <- "F" # "Verveux","Fyke net",""
  data0034$Categorie[data0034$Codes == 70] <- "F" # "Épuisette","Dip net",""
  data0034$Categorie[data0034$Codes == 71] <- "M" # "Drague non spécifiée","Dredge (boat)",""
  data0034$Categorie[data0034$Codes == 76] <- "F" # "Câble","Rope",""
  data0034$Categorie[data0034$Codes == 83] <- "F" # "Foène","Spear",""
  data0034$Categorie[data0034$Codes == 94] <- "F" # "Coupeur","Cutter",""
  data0034$Categorie[data0034$Codes == 95] <- "M" # "Bateau à déchargement","Pumper",""
  # ---
  data0034$Categorie[data0034$Codes == 81] <- "F" # "Harpon et lance","Harpoon and spear","M"
  data0034$Categorie[data0034$Codes == 85] <- "F" # "Harpon électrique","Electric harpoon","M"
  # --------------------------------------------------------------------------------

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Classify and filter fisheries data
  # ----------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  data0034 <- data0034 %>%
              select(Codes, gearClass, mobility = Categorie)

  # -----
  peche <- left_join(data0033, data0034, by = c("engin" = "Codes")) %>%
           filter(!is.na(gearClass)) %>%
           filter(!is.na(mobility))

  # -----
  peche <- peche %>%
           filter(as.Date(date_cap) >= as.Date("2010-01-01"))

  # -----
  # LBS to KG
  uid <- peche$un_mes == "P"
  peche$pd_deb[uid] <- peche$pd_deb[uid] *  0.453592

  # -----
  peche <- unique(peche)

  # ------------------------------------------------------------
  # NOTE: For metadata
  species_cible <- sort(unique(peche$prespvis))
  species <- table(peche$cod_esp) %>%
             as.data.frame() %>%
             rename(ESP_STAT = Var1) %>%
             mutate(ESP_STAT = as.numeric(as.character(ESP_STAT))) %>%
             left_join(data0035, by = "ESP_STAT") %>%
             select(ID = ESP_STAT, Scientific = DL_ESP, Espece = DF_ESP,
                    Species = DA_ESP, Freq)
  # ------------------------------------------------------------

  # -----
  peche <- peche %>%
           group_by(date_cap, latit_ori, longit_ori, gearClass, mobility) %>%
           summarise(catch = sum(pd_deb))

  # -----
  fix <- peche[peche$mobility == "F", ] %>%
         st_buffer(200)

  mob <- peche[peche$mobility == "M", ] %>%
         st_buffer(2000)

  peche <- bind_rows(fix, mob)

  # -----
  peche <- peche %>%
           arrange(as.Date(date_cap))

  # -----
  peche$ID <- 1:nrow(peche)
  # --------------------------------------------------------------------------------

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Measure intensity
  # ----------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  data(grid1p)
  grid1p$ID <- 1:nrow(grid1p)

  # # -----
  # peche$year <- format(as.Date(peche$date_cap), "%Y")
  # years <- unique(peche$year)
  # type <- unique(peche$gearClass)
  # comb <- expand.grid(type, years, stringsAsFactors = F)
  # colnames(comb) <- c('type','years')
  #
  # # Empty list to store fishing intensity by gear type and by year
  # intensity <- vector('list', nrow(comb))
  # names(intensity) <- apply(comb, 1, paste, collapse = "-")
  #
  # # Fishing intensity evaluation
  # for(i in 1:length(intensity)) {
  #   uid <- peche$year == comb[i, 'years'] & peche$gearClass == comb[i, 'type']
  #   intensity[[i]] <- fishingMetrics(peche[uid, ], grid1p)
  # }
  #

  # -----
  peche_commerciale <- grid1p %>%
                       mutate(
                         DNL = fishingMetrics(peche[peche$gearClass == 'DNL', ], .)[, 2],
                         DNH = fishingMetrics(peche[peche$gearClass == 'DNH', ], .)[, 2],
                         DD = fishingMetrics(peche[peche$gearClass == 'DD', ], .)[, 2],
                         PLB = fishingMetrics(peche[peche$gearClass == 'PLB', ], .)[, 2],
                         PHB = fishingMetrics(peche[peche$gearClass == 'PHB', ], .)[, 2]
                       ) %>%
                       select(-ID) %>%
                       # ugly
                       mutate(
                         DNL = ifelse(DNL == 0, NA, DNL),
                         DNH = ifelse(DNH == 0, NA, DNH),
                         DD = ifelse(DD == 0, NA, DD),
                         PLB = ifelse(PLB == 0, NA, PLB),
                         PHB = ifelse(PHB == 0, NA, PHB)
                       )
  # --------------------------------------------------------------------------------

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Update metadata
  # ----------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  meta <- load_metadata("int_st_peche_commerciale")

  # -----
  meta$rawData <- data_metadata

  # -----
  meta$dataDescription$spatial$extent <- st_bbox(data0033)

  # -----
  peche$years <- format(as.Date(peche$date_cap), "%Y")
  meta$dataDescription$temporal$start <- min(peche$years)
  meta$dataDescription$temporal$end <- max(peche$years)

  # -----
  meta$dataDescription$categories$accronyme <- c("DD", "DNL", "DNH", "PLB", "PHB")
  meta$dataDescription$categories$english <- c(
    "Demersal, destructive, high-bycatch",
    "Demersal, non-destructive, low-bycatch",
    "Demersal, non-destructive, high-bycatch",
    "Pelagic, low-bycatch",
    "Pelagic, high-bycatch")

  meta$dataDescription$categories$francais <- c(
    "Démersale destructive, prises accessoires élevées",
    "Démersale non-destructive, prises accessoires faibles",
    "Démersale non-destructive, prises accessoires élevées",
    "Pélagique prises accessoires faibles",
    "Pélagique prises accessoires élevées")

  meta$dataDescription$categories$source <- rep(paste0(meta$rawData, collapse = ","),
                                                length(meta$dataDescription$categories$accronyme))

  meta$dataDescription$categories$description <- c(
    "Activités de pêches commerciales à l'aide d'engins de pêche démersaux pouvant causer des dommages aux habitats ou au substrat, e.g. chalut et drague.",
    "Activités de pêches commerciales à l'aide d'engins de pêche démersaux avec peu ou en l'absence de prises accessoires et ne causant aucune modification des habitats, e.g. la pêche en plongée sous-marine.",
    "Activités de pêches commerciales à l'aide d'engins de pêche démersaux avec d'importantes prises accessoires et ne causant aucune modification des habitats, e.g. casiers et senne.",
    "Activités de pêches commerciales à l'aide d'engins de pêche pélagiques avec peu ou en l'absence de prises accessoires et ne causant aucune modification des habitats, e.g. pêche à la ligne, senne bourse.",
    "Activités de pêches commerciales à l'aide d'engins de pêche pélagiques avec d'importantes prises accessoires et ne causant aucune modification des habitats, e.g. filet maillant et palangre.")

  # -----
  obs <- peche %>% group_by(years) %>% summarize(total = n())
  meta$dataDescription$observations$total <- sum(obs$total)
  meta$dataDescription$observations$moyenne <- round(mean(obs$total), 0)
  meta$dataDescription$observations$sd <- round(sd(obs$total), 0)

  # -----
  meta$dataDescription$especes$cible <- species_cible
  meta$dataDescription$especes$capture <- species

  # --- For proper referencing in markdown syntax
  meta$dataDescription$categories$mdref <- modif_md(meta$dataDescription$categories$accronyme)
  # --------------------------------------------------------------------------------

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export
  # ------
  #
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # -----
  write_yaml(meta, "./data/data-metadata/int_st_peche_commerciale.yml")

  # -----
  st_write(obj = peche_commerciale,
           dsn = "./data/data-integrated/st_peche_commerciale.geojson",
           delete_dsn = TRUE,
           quiet = TRUE)
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Clean global environment
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  clean()
  # ------------------------------------------------------------------------- #}
}
