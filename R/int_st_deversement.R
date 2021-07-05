#' Déversements accidentels
#'
#' Couche de données transformées pour les déversements accidentels dans le Saint-Laurent
#'
#' @keywords déversement accidentel
#' @keywords stresseurs
#'
#' @export
#'
#' @details Cette fonction importe et formatte les données pour l'analyse d'effets cumulatifs
#'

st_deversement <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Format data
  # ------------------------------------
  #
  # WARNING: Method has to be revisited and document
  # https://github.com/EffetsCumulatifsNavigation/ceanav/issues/7
  #
  # For now, I will do something simple:
  #   1. Modify volume in classes from 1 to 5
  #       1 = 0 litre
  #       2 = 0 - 100 litres
  #       3 = 100 - 1000 litres
  #       4 = 1000 - 7000 litres
  #       5 = 100000 - 1000000 litres
  #   2. 5km buffer around spills (totally arbitrary, no scientific basis, just for viz)
  #   3. Intersect with grid
  #   4. Sum of intersects in each grid cell
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load grid
  data(grid1p)

  # Classify by volume
  lvls <- c('0 litre','0 - 100 litres','100 - 1000 litres','1000 - 7000 litres','100000 - 1000000 litres')
  dev$volume <- as.numeric(factor(dev$VOLUME_DEVERSE, levels = lvls))
  dev$volume[is.na(dev$volume)] <- 6

  # Classify by spill type
  hydrocarbures <- c("bunker C","Carburant diésel","Diesel","Essence","Gaoline",
                     "hydrocarbure inconnu","Petcoke","Pètrole brut","Propane",
                     "Tar","Huile de graissage","Huile Hydraulique","Huile moteur",
                     "bilge","BioSpec Hyd 32","Hydrox Bio 100","Lub oil",
                     "Huile hydraulique biodégradable","Asphalte liquide")

  autres <- c("Acide d'hypochlorite","Ballast","Débris","Déchet","Eau de cale",
              "eau huileuse","Eau usée","Eaux us","Lait","Matière organique",
              "MINERAI DE FER","Oxyde de calcium (chaux)","Phosphate d'ammonium",
              "Pollution","Sludge","Suie","Soude caustique","Huile lapio",
              "Huile Hydraulique Végétale","Huile végétale","Mélange huileux",
              "Charbon")

  inconnus <- c('Inconnu',"0","non identifi","P")


  # Identify grid cells ---------
  # Use grid as dataset
  deversement <- grid1p

  # Buffer around points
  dev <- st_buffer(dev, 5000)

  # Intersect polluant types with grid
  # Hydrocarbures
  hydrocarbures <- dev[dev$TYPE_POLLUANT %in% hydrocarbures, ] %>%
                   st_intersects(grid1p,.) %>%
                   lapply(., function(x) sum(dev$volume[x])) %>%
                   unlist()

  # Autres polluants
  autres <- dev[dev$TYPE_POLLUANT %in% autres, ] %>%
            st_intersects(grid1p,.) %>%
            lapply(., function(x) sum(dev$volume[x])) %>%
            unlist()

  # Inconnus
  inconnus <- dev[dev$TYPE_POLLUANT %in% inconnus, ] %>%
              st_intersects(grid1p,.) %>%
              lapply(., function(x) sum(dev$volume[x])) %>%
              unlist()

  # Add info to grid
  deversement <- grid1p %>%
                 mutate(hydrocarbures = hydrocarbures,
                        autres = autres,
                        inconnus = inconnus)

  # exportMapview(deversement[, 'hydrocarbures'], './share/hydrocarbures.html')
  # exportMapview(deversement[, 'autres'], './share/autres.html')
  # exportMapview(deversement[, 'inconnus'], './share/inconnus.html')
  # ------------------------------------------------------------------------- #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  save(deversement, file = './data/str_deversement.RData')
  # ------------------------------------------------------------------------- #

}
