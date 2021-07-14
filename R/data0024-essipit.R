#' Data 0024 : Sites d'importance communauté Essipit
#'
#' Collection de sites d'intérêt sociaux, culturels, archéologiques et de pratiques Innu-Aitun issue de l'intégration d'ouvrages divers
#'
#' @keywords Essipit
#' @keywords sites d'importance
#' @keywords composante valorisée
#'
#' @source Bouchard D., Tremblay P. (communication personnelle)  Collection de sites d'intérêt sociaux, culturels, archéologiques et de pratiques Innu-Aitun issue de l'intégration d'ouvrages divers. Secteur Territoire et Consultation, Conseil de Bande Essipit.
#'
#' @export
#'
#' @details Cette fonction formatte les données
#'

get_data0024 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # WARNING:
  message("Les données brutes sont soumis à une entente de partage de données et doivent être supprimées au terme du projet")

  # Output folder
  output <- "data0024-essipit/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # WARNING: Data transfered physically, no cloud access currently

  # Unzip
  unzip(zipfile = paste0(folder, 'Effets_cumul_datas_Essipit.gdb.zip'), exdir = folder)

  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import and format data
  # ----------------------------------------
  lay <- st_layers(paste0(folder, "Effets_cumul_datas_Essipit.gdb"))

  # WARNING: There ios something weird with the structure of certain layers,
  # which are LINESTRING, but also MULTISURFACE and CURVEPOLYGON. I cannot find
  # a clean way to deal with this, so I will use a brute force approach.
  # Just keep in mind that if another set of rawdata comes along, this code
  # will likely not be reproducible

  # --------------------------------------------------
  # Function to format individual layers
  geom_format <- function(dat) {
    # Only geometries
    geom <- st_geometry(dat) %>%
            st_zm() %>%
            st_sfc() %>%
            st_as_text() # Transforms geometry as character string

    # Remove MULTISURFACE and CURVEPOLYGON
    geom <- gsub("MULTISURFACE \\(CURVEPOLYGON \\(COMPOUNDCURVE \\(CIRCULARSTRING", "LINESTRING", geom)
    geom <- gsub("MULTISURFACE \\(CURVEPOLYGON \\(", "", geom)
    geom <- gsub("CIRCULARSTRING \\(", "LINESTRING \\(", geom)
    geom <- gsub("MULTIPOLYGON \\(\\(\\(", "LINESTRING \\(", geom)
    geom <- gsub("COMPOUNDCURVE \\(", "", geom)
    geom <- gsub("\\), LINESTRING \\(", ",", geom)
    geom <- gsub("\\)\\)\\)\\)", "\\)", geom)
    geom <- gsub("\\)\\)\\)", "\\)", geom)
    geom <- gsub("\\)\\)", "\\)", geom)
    # geom <- gsub("\\(\\(", "\\(", geom)
    # geom <- gsub("\\), \\(", ",", geom)

    # Recreate polygons
    dat <- st_as_sfc(geom) %>%
           st_sf(st_drop_geometry(dat[1,]), crs = st_crs(dat)) %>%
           st_cast("POLYGON")

    # Return
    dat
  }

  # --------------------------------------------------
  data0024 <- list()

  # Names of columns to retain
  nm <- c("Type", "Source", "Description", "Sous_cat", "Commentaire", "Nom_secteur", "Geometry")

  # WARNING: This could all be done more simply in a loop, but I keep them seperate
  #          for human redeability

  # --------------------------------------------------
  # Culture et patrimoine
  dat <- st_read(paste0(folder, "Effets_cumul_datas_Essipit.gdb"), layer = lay$name[1], quiet = TRUE)
  data0024[[1]] <- geom_format(dat) %>%
                   mutate(Type = lay$name[1]) %>%
                   rename(Geometry = ".") %>%
                   .[,nm]


  # --------------------------------------------------
  # Utilis_terr_Trad_peche
  dat <- st_read(paste0(folder, "Effets_cumul_datas_Essipit.gdb"), layer = lay$name[2], quiet = TRUE)
  data0024[[2]] <- geom_format(dat) %>%
                   mutate(Type = lay$name[2]) %>%
                   rename(Geometry = ".") %>%
                   .[,nm]

  # --------------------------------------------------
  # Utilis_terr_Trad_ois_migr
  dat <- st_read(paste0(folder, "Effets_cumul_datas_Essipit.gdb"), layer = lay$name[3], quiet = TRUE)
  data0024[[3]] <- geom_format(dat) %>%
                   mutate(Type = lay$name[3]) %>%
                   rename(Geometry = ".") %>%
                   .[,nm]

  # --------------------------------------------------
  # Utilis_terr_Trad_phoque
  dat <- st_read(paste0(folder, "Effets_cumul_datas_Essipit.gdb"), layer = lay$name[4], quiet = TRUE)
  data0024[[4]] <- geom_format(dat) %>%
                   mutate(Type = lay$name[4]) %>%
                   rename(Geometry = ".") %>%
                   .[,nm]

  # --------------------------------------------------
  # Utilis_terr_peche_com
  dat <- st_read(paste0(folder, "Effets_cumul_datas_Essipit.gdb"), layer = lay$name[5], quiet = TRUE)
  data0024[[5]] <- geom_format(dat) %>%
                   mutate(Type = lay$name[5]) %>%
                   rename(Geometry = ".") %>%
                   .[,nm]


  # --------------------------------------------------
  # Utilis_terr_Trad_phoque
  dat <- st_read(paste0(folder, "Effets_cumul_datas_Essipit.gdb"), layer = lay$name[6], quiet = TRUE)[2:3, ]
  data0024[[6]] <- geom_format(dat) %>%
                   mutate(Type = lay$name[6])

  # First geometry is problematic. Dealing with it seperately
  dat <- st_read(paste0(folder, "Effets_cumul_datas_Essipit.gdb"), layer = lay$name[6], quiet = TRUE)[1,]

  # Transform geoetry is a usable format
  geom <- st_geometry(dat) %>%
          st_zm() %>%
          st_cast("POLYGON") %>%
          st_multipolygon(lapply(., function(x) x[1])) %>% # Remove holes in polygon that have no business being there
          st_sfc() %>%
          # This next part is to remove a weird little polygon that is somehow
          # present rather than only the desired geometry
          st_union() %>%
          st_cast("POLYGON") %>%
          .[2, ] %>%
          st_sf(st_drop_geometry(dat), crs = st_crs(dat)) %>%
          mutate(Type = lay$name[6])

  # Bind with rest of data
  data0024[[6]] <- rbind(data0024[[6]], geom) %>%
                   rename(Geometry = ".") %>%
                   .[,nm]

  # --------------------------------------------------
  # Utilis_terr_activ_touristique_p
  message("Utilisation d'un buffer de 300m autour des pooints pour l'instant. Pourrait être revisité plus tard au besoin.")
  dat <- st_read(paste0(folder, "Effets_cumul_datas_Essipit.gdb"), layer = lay$name[7], quiet = TRUE)
  data0024[[7]] <- st_zm(dat) %>%
                   st_buffer(300) %>%
                   mutate(Type = lay$name[7]) %>%
                   rename(Geometry = Shape) %>%
                   .[,nm]



  # --------------------------------------------------
  # Utils_terr_acces_plans_eau
  dat <- st_read(paste0(folder, "Effets_cumul_datas_Essipit.gdb"), layer = lay$name[8], quiet = TRUE)
  data0024[[8]] <- st_zm(dat) %>%
                   st_buffer(300) %>%
                   mutate(Type = lay$name[8]) %>%
                   rename(Geometry = Shape) %>%
                   .[,nm]

  # --------------------------------------------------
  # Utils_terr_acces_plans_eau
  dat <- st_read(paste0(folder, "Effets_cumul_datas_Essipit.gdb"), layer = lay$name[9], quiet = TRUE)
  data0024[[9]] <- geom_format(dat) %>%
                   mutate(Type = lay$name[9]) %>%
                   rename(Geometry = ".") %>%
                   .[,nm]


  # --------------------------------------------------
  # Single dataset
  data0024 <- bind_rows(data0024)

  # Transform projection
  data0024 <- st_transform(data0024, crs = global_parameters()$crs)
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0024,
           dsn = "./data/data-format/data0024-essipit.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #
}
