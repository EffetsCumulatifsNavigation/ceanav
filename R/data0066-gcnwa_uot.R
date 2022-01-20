#' Data 0066 : Utilisation et occupation traditionnelle (UOT) GCNWA
#'
#' Rapport d’intrants fournis par le Bureau du Ndakina quant à l’étude des effets cumulatifs de la navigation commerciale. Rapport à l’intention de Transport Canada et Pêches et Océans Canada.
#'
#' @keywords GCNWA
#' @keywords Utilisation et occupation traditionnelle
#' @keywords composante valorisée
#'
#' @source Grand Conseil de la Nation Waban-Aki (GCNWA), 2021. Rapport d’intrants fournis par le Bureau du Ndakina quant à l’étude des effets cumulatifs de la navigation commerciale. Rapport à l’intention de Transport Canada et Pêches et Océans Canada. Rédigé par E. Blanchet et G. Treyvaud, Bureau du Ndakina, 42 p. et annexes.
#'
#' @export
#'
#' @details Cette fonction formatte les données
#'

get_data0066 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # WARNING:
  message("Les données brutes sont soumis à une entente de partage de données et doivent être supprimées au terme du projet")

  # Output folder
  output <- "data0066-gcnwa_uot/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # WARNING: Data transfered physically, no cloud access
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import and format data
  # ----------------------------------------
  data0066 <- read.csv(paste0(folder, "UOT_grille_anonyme.csv")) %>%
              select(-val_ras) %>%
              mutate(category_ressource = "")

  # Classify resources
    ## Gibier
    nm <- c("CV","DS","OR","ON","GH","LA")
    data0066$category_ressource[data0066$ressource %in% nm] <- "Gibier"

    ## Oiseaux migrateurs
    nm <- c("BC","OB","CD","XG")
    data0066$category_ressource[data0066$ressource %in% nm] <- "Oiseaux migrateurs"

    ## Animaux à fourrure
    nm <- c("CA","CO","LR","LP","LX","MU","RE","RM","XF","XT")
    data0066$category_ressource[data0066$ressource %in% nm] <- "Animaux à fourrure"

    ## Cueillette et collecte
    nm <- c("AB","AF","AM","AR","BP","CE","CG","CH","EB","FN","FR",
            "FV","GC","HO","IF","LG","MC","MF","PM","QU","RC","SB",
            "SV","TH","XV","XE")
    data0066$category_ressource[data0066$ressource %in% nm] <- "Cueillette et collecte"

    ## Sites de coucher
    nm <- c("CP","SC","XN")
    data0066$category_ressource[data0066$ressource %in% nm] <- "Sites de coucher"

    ## Sites culturels
    nm <- c("LS","SF","SL","SR","OA","ST")
    data0066$category_ressource[data0066$ressource %in% nm] <- "Sites culturels"

    ## Sites essentiels
    nm <- c("MI","AP","PL","XS")
    data0066$category_ressource[data0066$ressource %in% nm] <- "Sites essentiels"

    ## Problèmes liés au territoire
    nm <- c("PA","PZ")
    data0066$category_ressource[data0066$ressource %in% nm] <- "Problèmes liés au territoire"

    ## Zones d'activités
    nm <- c("ZC","ZT","ZP","ZE","ZX")
    data0066$category_ressource[data0066$ressource %in% nm] <- "Zones d'activités"

    ## Navigation
    nm <- c("BT","DB","XC")
    data0066$category_ressource[data0066$ressource %in% nm] <- "Navigation"

    # ## Zones particulières
    # nm <- c("ZF","ZS")
    # data0066$category_ressource[data0066$ressource %in% nm] <- "Zones particulières"

    ## Pêche
    # TODO: Need to check, those accronyms are not defined
    # "DC"
    # "DR" = Doré  (noir ou jaune)
    # "ES" = Esturgeon (noir ou jaune)
    # "BA" = Barbotte ou barbue
    # "AC" = Achigan (petite ou grande bouche)
    # "BR" = Brochet ou maskinong 
    # "PE" = Perchaude
    # "DQ" = 
    # "OE" = 
    # "XP" = Autre esp ce poisson
    # "VT" = 
    uid <- data0066$category_ressource == ""
    data0066$category_ressource[uid] <- "Pêche"

  # -----
  grid2p <- st_read(paste0(folder, "Grid_Poly2000.geojson"), quiet = TRUE) %>%
            mutate(gridnum = 1:nrow(.)) %>%
            select(-val_ras)

  # --------------------------------------------------
  # Single dataset
  data0066 <- left_join(grid2p, data0066, by = "gridnum") %>%
              filter(!is.na(fid))

  # Transform projection
  data0066 <- st_transform(data0066, crs = global_parameters()$crs)
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0066,
           dsn = "./data/data-format/data0066-gcnwa_uot.geojson",
           delete_dsn = TRUE,
           quiet = TRUE)
  # _________________________________________________________________________ #
}
