server <- function(input, output, session) {
  updateSelectInput(session, "layers", choices = colnames(ceanav_data))
  observeEvent(input$layers, {
    tmp <- ceanav_data %>% select(!!input$layers)
    map <- leaflet(tmp) %>%
      setView(lng = -63, lat = 48, zoom = 5) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(
        stroke = FALSE)#,
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
