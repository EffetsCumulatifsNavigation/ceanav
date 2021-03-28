server <- function(input, output, session) {
  updateSelectInput(session, "layers", choices = dat)
  observeEvent(input$layers, {
    # Data
    # tmp <- alevinage %>% st_transform(crs = 4326)
    tmp <- get(input$layers) %>% st_transform(crs = 4326)

    # Colors
    # Create a continuous palette function
    pal <- colorNumeric(palette = "Blues",
                        domain = tmp$alevinage)

    # Map
    map <- leaflet(tmp) %>%
      setView(lng = -63, lat = 48, zoom = 5) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1, color = ~pal(alevinage))
      # addCircleMarkers(
        # stroke = FALSE)#,
        # popup = ~as.character(Nom.de.colonie.Colony.name))
    edits <<- callModule(editMod, leafmap = map, id = "map")
  })
  # observeEvent(input$save, {
  #   geom <- edits()$finished
  #   if (!is.null(geom)) {
  #      assign('new_geom', geom, envir = .GlobalEnv)
  #      sf::write_sf(geom, 'new_geom.geojson', delete_layer = TRUE, delete_dsn = TRUE)
  #    } # see https://github.com/r-spatial/mapedit/issues/95
  #  })
}
