#' Figures données de navigation pour exploration
#'
#' Fonctions pour générer des figures pour les données de navigation AIS
#'
#' @keywords figure
#'
#' @rdname fig_navigation_exploration
#'
#' @export
#'

fig_navigation_exploration <- function() {
  ais <- st_read('./data/data-raw/data0021-navigation/ais.geojson')
  seg <- st_read('./data/data-raw/data0021-navigation/ais_segments.geojson')
  
  # Vessel types
  vessel_type <- unique(seg$NTYPE)
  
  # Remove NAs 
  vessel_type <- vessel_type[!is.na(vessel_type)]
  
  # ------------------
  data(aoi)
  aoi <- suppressWarnings(st_simplify(aoi, dTolerance = 100, preserveTopology = F))

  # ------------------
  global_parameters()

  # ------------------
  bbox <- global_param$bbox$base

  # ------------------
  plotDat <- function(d = dat, a = aoi) {
    # -----
    plot(
      st_geometry(d),
      lwd = .25,
      add = TRUE,
      pch = 20,
      cex = .2,
      col = "#196a6a44",
      border = "#196a6a44"
    )

    # ------------------
    plot(
      st_geometry(a),
      lwd = .5,
      border = global_parameters()$col$integrated$coastline,
      add = TRUE
    )
  }

  # ------------------------------------------------------------------------
  # Graph principal
  
  # ------------------
  # Text
  addTitles <- function(subtitle) {
    y <- bbox$ymax
    y <- y - 10000
    text(x = bbox$xmin + 1000,
         y = y,
         labels = "Navigation",
         font = 2,
         adj = c(0,.5),
         cex = .8
       )
       
    y <- y - 30000
    text(
      x = bbox$xmin + 1000,
      y = y,
      labels = subtitle,
      adj = c(0,.5),
      font = 3,
      cex = .6
    )
  }
  
  # Exploratory figures
  # ------------------------
  for(i in 1:length(vessel_type)) {
    aisid <- ais$NTYPE == vessel_type[i] 
    segid <- seg$NTYPE == vessel_type[i] 
    
    # ------------------
    png(glue('./figures/figures-exploration/navigation-{vessel_type[i]}.png'), res = 300, width = 200, height = 70, units = "mm", pointsize = 12)
    
    par(mfrow = c(1,2))

    # AIS 
    par(family = 'serif', mar = c(.5, .5, .5, .5))
    plot0(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax))
    box()
    addTitles(vessel_type[i])
    plotDat(ais[aisid, ])
    
    # Segments
    par(family = 'serif', mar = c(.5, .5, .5, .5))
    plot0(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax))
    box()
    plotDat(seg[segid, ])  

    # 
    dev.off()  
  }
}