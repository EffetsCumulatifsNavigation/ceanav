#' Figure of study area
#'
#' @export

fig_aoi <- function() {
  # ------------------
  data(grid1p)

  # ------------------
  data(aoi)
  aoi <- suppressWarnings(st_simplify(aoi, dTolerance = 100, preserveTopology = F))

  # ------------------
  basemap("cities")
  basemap("quebec")

  # ------------------
  global_parameters()

  # ------------------
  plotDat <- function(trans = "77") {
    # ------------------
    plot(
      st_geometry(grid1p),
      lwd = .1,
      border = paste0("#000000", trans),
      add = TRUE
    )

    # ------------------
    plot(
      st_geometry(aoi),
      lwd = .5,
      border = "#000000",
      add = TRUE
    )



  }

  # ------------------------------------------------------------------------
  # Graph principal
  png(glue('./figures/aoi.png'), res = global_param$figures$resolution, width = global_param$figures$width, height = global_param$figures$height, units = "mm", pointsize = global_param$figures$pointsize)

  # ------------------
  par(family = 'serif', mar = c(.5, .5, .5, .5))

  # ------------------
  bbox <- global_param$bbox$base
  qc <- st_bbox(quebec)
  lacstpierre <- global_param$bbox$lacstpierre

  # ------------------
  pal <- colorRampPalette(viridis::viridis(100))
  # pal <- colorRampPalette(global_param$col$integrated$palette)

  # ------------------
  # Basemap
  plot0(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax))
  box()


  # ------------------
  # Text
  text(x = bbox$xmin + 1000,
       y = bbox$ymax - 10000,
       labels = "Zone d'étude",
       font = 2,
       adj = c(0,.5),
       cex = .8
     )


  # ------------------
  # Data
  plotDat()

  # ------------------
  # Cities
  plot(st_geometry(cities), add = TRUE, pch = 21, col = "#3e3e3e", bg = "#9f9f9f", cex = .4)
  for(i in 1:nrow(cities)) {
    text(x = cities$X[i]+cities$offX[i],
         y = cities$Y[i]+cities$offY[i],
         labels = cities$city[i],
         cex = .35,
         col = "#000000",
         adj = c(cities$adjX[i], .5))
  }

  # ------------------
  # Secteurs
  lines(x = rep(-200000, 2), y = c(270000, 370000), lwd = 1.5, lty = 2)
  text(x = -335000, 350000, labels = c("Secteur fluvial"), cex = .7, font = 3, adj = c(.5,.5), srt = 30)
  text(x = 60000, 460000, labels = c("Secteur maritime"), cex = .7, font = 3, adj = c(.5,.5), srt = 30)

  # ------------------
  rect(lacstpierre[1], lacstpierre[3], lacstpierre[2], lacstpierre[4], lty = 2,
       border = "#00000088")

  # ---------------------------
  # Grille
  par(new = TRUE)
  par(fig = c(.035,.25,.6,.825), mar = c(0,0,0,0))
  # par(fig = c(.765,.965,.05,.25), mar = c(0,0,0,0))
  plot0(x = c(lacstpierre$xmin, lacstpierre$xmax), y = c(lacstpierre$ymin, lacstpierre$ymax))
  box(col = "#00000088")
  plotDat(trans = "FF")
  text(x = lacstpierre$xmin + 750, y = lacstpierre$ymax - 3000, labels = "Grille d'étude", cex = .5, font = 3, adj = c(0,.5))

  # ---------------------------
  # Québec
  par(new = TRUE)
  par(fig = c(.65,.965,.0525,.5), mar = c(0,0,0,0))
  # par(fig = c(.765,.965,.275,.475), mar = c(0,0,0,0))
  plot0(x = c(qc$xmin, qc$xmax), y = c(qc$ymin, qc$ymax))
  # ------------------
  plot(
    st_geometry(quebec),
    lwd = .5,
    border = "#6b6b6b",
    add = TRUE
  )

  # ------------------
  plot(
    st_geometry(aoi),
    lwd = .5,
    border = global_param$col$integrated$focus,
    col = global_param$col$integrated$focus,
    add = TRUE
  )

  # ----
  box(col = "#000000")

  # ---------------------------
  dev.off()
}
