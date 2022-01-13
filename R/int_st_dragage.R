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
  load_format("data0049")
  load_format("data0050")
  load_format("data0052")

  # ------------------------------------------------------
  meta <- load_metadata("int_st_dragage")
  meta$rawData <- c("0018", "0019", "0046", "0048", "0049", "0050", "0052")
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
  data0052 <- data0052 %>%
              mutate(type = "dragage",
                     area_tot = as.numeric(st_area(.)) * 1e-6) %>%
              select(municipalite, name, annee, volume, type, area_tot, geometry)


  # -----
  dragage <- bind_rows(dat0019, dat0046, dat0048, data0052) %>%
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
  # WARNING: Retrait des dragages d'entretien prévus, puisque les informations
  #          disponibles ne sont pas exhaustives
  # dat0048 <- data0048 %>%
  #            filter(annee > 2021) %>%
  #            mutate(area_tot = as.numeric(st_area(.)) * 1e-6) %>%
  #            select(municipalite, name, annee, volume, area_tot, geometry)

  # -----
  dat0049 <- data0049 %>%
             mutate(area_tot = as.numeric(st_area(.)) * 1e-6) %>%
             select(municipalite, name, annee, volume, area_tot, geometry)

  # -----
  dat0050 <- data0050 %>%
             mutate(area_tot = as.numeric(st_area(.)) * 1e-6) %>%
             select(municipalite, name, annee, volume, area_tot, geometry)

  # -----
  dragage_prevu <- bind_rows(dat0049, dat0050) %>%
                   group_by(municipalite, name, area_tot) %>%
                   summarise(volume = sum(volume))
  # ------------------------------------------------------------------------- #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Update metadata
  # ----------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # -----
  meta$dataDescription$spatial$extent <- st_bbox(dragage)

  # -----
  meta$dataDescription$categories$accronyme <- c("dragage","depot","dragage_prevu")

  meta$dataDescription$categories$francais <- c(
    "Sites de dragage",
    "Sites de dépôts",
    "Dragages de capitalisation prévus"
  )

  meta$dataDescription$categories$source <- c(
    "0018,0019,0046,0048,0052",
    "0018,0019,0046,0048",
    "0049,0050"
  )

  meta$dataDescription$categories$sites <- c(
    nrow(dragage), nrow(depot), nrow(dragage_prevu)
  )

  meta$dataDescription$categories$volume <- c(
    sum(dragage$volume), sum(depot$volume), sum(dragage_prevu$volume)
  )

  meta$dataDescription$categories$volume_moyen <- c(
    mean(dragage$volume), mean(depot$volume), mean(dragage_prevu$volume)
  )

  meta$dataDescription$categories$volume_sd <- c(
    sd(dragage$volume), sd(depot$volume), NA
  )

  meta$dataDescription$categories$description <- c(
    "Sites de dragage d'entretien, i.e. dragage plus ou moins réguler de la voie navigable afin de maintenir des conditions sécuritaire pour la navigation.",
    "Sites de dépôt en milieu aquatique ou marin des sédiments dragués lors d'opération de dragage d'entretien ou de capitalisation.",
    "Sites de dragage de capitalisation prévus, i.e. dragage visant à approfondir et élargir la voie de navigation."
  )


  # -----
  meta$dataDescription$dragage$municipalite <- dragage$municipalite
  meta$dataDescription$dragage$name <- dragage$name
  meta$dataDescription$dragage$volume <- dragage$volume

  # -----
  meta$dataDescription$depot$municipalite <- depot$municipalite
  meta$dataDescription$depot$name <- depot$name
  meta$dataDescription$depot$volume <- depot$volume

  # -----
  meta$dataDescription$dragage_prevu$municipalite <- dragage_prevu$municipalite
  meta$dataDescription$dragage_prevu$name <- dragage_prevu$name
  meta$dataDescription$dragage_prevu$volume <- dragage_prevu$volume

  # --- For proper referencing in markdown syntax
  meta$dataDescription$categories$mdref <- modif_md(meta$dataDescription$categories$accronyme)
  # --------------------------------------------------------------------------------

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
  dragage_prevu <- st_intersection(grid1p, dragage_prevu) %>%
                   mutate(area = as.numeric(st_area(.)) * 1e-6,
                          area_prop = area / area_tot,
                          intensite = volume * area_prop) %>%
                   group_by(id) %>%
                   summarise(dragage_prevu = sum(intensite)) %>%
                   st_drop_geometry()

  # -----
  grid1p <- grid1p %>%
            left_join(dragage, by = "id") %>%
            left_join(depot, by = "id") %>%
            left_join(dragage_prevu, by = "id")

  # -----
  dragage <- grid1p %>%
             select(-id)
  # ------------------------------------------------------------------------- #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
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
