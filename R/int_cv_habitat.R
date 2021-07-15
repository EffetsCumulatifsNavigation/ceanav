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
  #   - Milieux humides : 0004, 0005, 0006
  #   - Marais côtiers : 0007
  #   - Sites d'alevinage : 0009
  #   - Frayères : 0010
  #   - Zones herbacées : 0029
  #   - Espèces à statut : 0011
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # ------------
  data(grid1p)
  habitat <- grid1p

  # ------------
  # Zostère : 0001, 0002, 0003
  load_format("data0001")
  load_format("data0002")
  load_format("data0003")

  uid <- bind_rows(data0001, data0002, data0003) %>%
         st_intersects(grid1p) %>%
         unlist() %>%
         unique()

  habitat$zostere <- 0
  habitat$zostere[uid] <- 1


  # ------------
  # Milieux humides : 0004, 0005, 0006
  # load_format("data0004") Point database already covered by the other two datasets
  load_format("data0005")
  load_format("data0006")

  uid <- bind_rows(data0005, data0006) %>%
         st_intersects(grid1p) %>%
         unlist() %>%
         unique()

  habitat$milieu_humide <- 0
  habitat$milieu_humide[uid] <- 1
  # ------------------------------------------------------------------------- #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  st_write(obj = habitat,
           dsn = "./data/data-integrated/cv_habitat.geojson",
           delete_dsn = TRUE)
  # ------------------------------------------------------------------------- #}
}
