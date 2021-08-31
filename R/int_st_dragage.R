#' Dragage
#'
#' Couche de données transformées pour les activités de dragage dans le Saint-Laurent
#'
#' @keywords dragage
#' @keywords stresseurs
#'
#' @export
#'
#' @details Cette fonction importe et formatte les données pour l'analyse d'effets cumulatifs
#'

st_dragage <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  load_format("data0018")
  load_format("data0019")
  load_format("data0046")
  load_format("data0048")

  # ------------------------------------------------------
  meta <- load_metadata("int_st_dragage")
  meta$rawData <- c("0018", "0019", "0046", "0048")
  # ------------------------------------------------------------------------- #



  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Format and combine data for historic dredging
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # -----
  dat0019 <- rename(data0019, name = site_dragage,  volume = volume_m3) %>%
              mutate(volume = as.numeric(volume),
                     area_tot = as.numeric(st_area(.)) * 1e-6) %>%
              select(municipalite, name, annee, volume, type, area_tot, geometry)

  # -----
  dat0046 <- filter(data0046, annee > 2000 & volume > 0) %>%
              mutate(area_tot = as.numeric(st_area(.)) * 1e-6) %>%
              select(municipalite, name, annee, volume, type, area_tot, geometry)

  # -----
  dat0048 <- data0048 %>%
              mutate(type = "dragage",
                     area_tot = as.numeric(st_area(.)) * 1e-6) %>%
              select(municipalite, name, annee, volume, type, area_tot, geometry) %>%
              filter(annee <= 2021)

  # -----
  dragage <- bind_rows(dat0019, dat0046, dat0048) %>%
             filter(type == "dragage") %>%
             group_by(municipalite, name, type, area_tot) %>%
             summarise(volume = sum(volume))

  # -----
  depot <- bind_rows(dat0019, dat0046) %>%
           filter(type == "depot") %>%
           group_by(municipalite, name, type, area_tot) %>%
           summarise(volume = sum(volume))
  # ------------------------------------------------------------------------- #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Format and combine data for future dredging
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  dat0048 <- data0048 %>%
              mutate(type = "dragage",
                     area_tot = as.numeric(st_area(.)) * 1e-6) %>%
              select(municipalite, name, annee, volume, type, area_tot, geometry) %>%
              filter(annee > 2021)


  # ------------------------------------------------------------------------- #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Integrate to study grid
  # ------------------------------------
  # NOTE: Area weighted total
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  data(grid1p)
  grid1p$id <- 1:nrow(grid1p)

  # -----
  dragage <- st_intersection(grid1p, dragage) %>%
             mutate(area = as.numeric(st_area(.)) * 1e-6,
                    area_prop = area / area_tot,
                    intensite = volume * area_prop) %>%
             group_by(id) %>%
             summarise(dragage = sum(intensite)) %>%
             st_drop_geometry()

  # -----
  depot <- st_intersection(grid1p, depot) %>%
           mutate(area = as.numeric(st_area(.)) * 1e-6,
                  area_prop = area / area_tot,
                  intensite = volume * area_prop) %>%
           group_by(id) %>%
           summarise(depot = sum(intensite)) %>%
           st_drop_geometry()

  # -----
  grid1p <- grid1p %>%
            left_join(dragage, by = "id") %>%
            left_join(depot, by = "id")

  # -----
  dragage <- grid1p %>%
             select(-id)
  # ------------------------------------------------------------------------- #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  message("Dragage: Warning: still missing Montreal and Quebec")
  # -----
  write_yaml(meta, "./data/data-metadata/int_st_dragage.yml")

  # -----
  st_write(obj = dragage,
           dsn = "./data/data-integrated/st_dragage.geojson",
           delete_dsn = TRUE,
           quiet = TRUE)
  # ------------------------------------------------------------------------- #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Clean global environment
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  clean()
  # ------------------------------------------------------------------------- #}
}
