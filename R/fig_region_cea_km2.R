#' Figure effects cumulatifs par région administrative par km2
#'
#' @keywords figure
#'
#' @export
#'
#' @examples
#' # Figure for specific dataset
#' fig_region_cea_km2()


fig_region_cea_km2 <- function(lang = "fr") {
  # Output folder
  if (lang == "fr") {
    output <- "./figures/figures-output/"
  } else if (lang == "en") {
    output <- "./figures_en/figures-output/"
  }
  
  temp <- function(data_id, output_name, suffix = NULL, language = lang) {
    #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
    # Data
    #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
    load_output(data_id)
    assign("cekm", get(data_id))
    st <- read.csv("data/data-metadata/metadata_stresseurs.csv")

    # Stressor fullnames
    st <- mutate(st, fullname = glue("{stresseur}_{accronyme}"))

    # Stressor groups    
    if (language == "fr") {
      grNames <- st[,c('stresseur','title')] %>%
                 group_by(stresseur, title) %>%
                 summarize(size = n()) %>%
                 ungroup() %>%
                 mutate(cumsize = cumsum(size))
      nGroup <- nrow(grNames)      
    } else if (language == "en") {
      grNames <- st[,c('stresseur','title_en')] %>%
                 rename(title = title_en) %>%
                 group_by(stresseur, title) %>%
                 summarize(size = n()) %>%
                 ungroup() %>%
                 mutate(cumsize = cumsum(size))
      nGroup <- nrow(grNames)
    }


    # Régions administratives
    load_format("data0065")
    data0065 <- data0065[data0065$RES_NM_REG %in% cekm$region, ]

    # Intersect with study region
    data(aoi)
    aoi <- suppressWarnings(st_simplify(aoi, dTolerance = 100, preserveTopology = F))
    aoi <- st_buffer(aoi, 2000)
    regions <- st_intersection(data0065, aoi) %>%
               left_join(cekm[,c('region','cea')], by = c("RES_NM_REG" = "region"))

    # Reorder data by longitude and latitude
    regions <- st_centroid(regions) %>% st_coordinates %>%
               cbind(regions) %>%
               arrange(X, Y)

    # Reorder cekm
    uid <- match(regions$RES_NM_REG, cekm$region)
    cekm <- cekm[uid, ]


    #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
    # Data
    #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
    # Adapted from: https://stackoverflow.com/questions/31873151/how-rotate-map-in-r?noredirect=1&lq=1
    # Rotate an sf geom around a center point. If no center is
    # specified then it rotates around the center of the geom.
    # This is technically an affine transformation: https://r-spatial.github.io/sf/articles/sf3.html#affine-transformations-1
    st_ellide_rotate = function(x, degrees, center_coords=NULL){
      if(degrees < -360 | degrees > 360) stop('Degrees must be in the range -360 to 360')
      x = sf::st_combine(x)
      if(is.null(center_coords)){
        center_coords = sf::st_centroid(x)
      }
      radians = degrees * pi/180
      transform_matrix = matrix(c(cos(radians), sin(radians), -sin(radians), cos(radians)), 2, 2)

      return((x-center_coords) * transform_matrix + center_coords)
    }

    center_coords <- sf::st_combine(regions) %>% sf::st_centroid()
    l <- list()
    for(i in 1:nrow(regions)) {
      l[[i]] <- st_ellide_rotate(regions[i, ], 35, center_coords) %>%
                st_sf() %>%
                mutate(cea = regions$cea[i])
    }
    regions_rotate <- bind_rows(l)

    # Region centroids
    regions_center <- st_centroid(regions) %>%
                      st_ellide_rotate(35, center_coords)

    # Normalize spatial objects
    # regions_rotate <- st_normalize(regions_rotate)
    # regions_center <- st_normalize(regions_center, domain = st_bbox(regions_rotate))

    #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
    # Lines between graph 1 and 2
    #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
    # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    # NOTE: This is true of the current settings from 2nd graph only
    #       Should be updated if x and y ranges change for either graph
    g1 <- c(-543864.9, 182186.7)
    g2 <- c(-4.56, 11.56)
    # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    # Graph ranges
    g1_range <- diff(g1)
    g2_range <- diff(g2)

    # Relative position on 2nd graph
    vertical_lines <- 1:nrow(cekm)
    vertical_lines_relative <- (vertical_lines - g2[1]) / g2_range

    # Transform that to relative positions on 1st graph
    vertical_lines_1 <- (vertical_lines_relative * g1_range) +g1[1]

    # XY1 graph 1
    xy1 <- st_coordinates(regions_center)

    # Lines coordinates
    l <- data.frame(x1 = xy1[,'X'], y1 = xy1[,'Y'], x2 = vertical_lines_1, y2 = 385000)


    #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
    # Colors
    #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
    # Colors for stressors
    gg_color_hue <- function(n) {
      hues = seq(15, 375, length = n + 1)
      hcl(h = hues, l = 65, c = 100, alpha = .5)[1:n]
    }

    cols <- c("#426E88",
              "#9EA8B5",
              "#413249",
              "#876F74",
              "#BD2A4D",
              "#E89B40",
              "#5BB2AD")

    cols <- c("#2a2a2a",
              "#622b2b",
              "#856226",
              "#286039",
              "#2d81a4",
              "#68457c",
              "#8c3449")
    gg_color_hue <- colorRampPalette(cols)

    pal <- colorRampPalette(viridis::viridis(100))
    #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
    # Stresseurs
    #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
    # trans1 <- c('FF','DD','BB','99','77','55','33','11')
    # trans2 <- c('FF','DD','AA','88','66','44')
    # trans3 <- c('FF','DD','BB','99','77','55','33','11','22','44','66','88','AA','CC','EE')
    st$col <- gg_color_hue(nGroup)[as.numeric(as.factor(st$title))]
    # for(i in 1:nrow(st)) st$col[i] <- darken(st$col[i], 20)

    # for(i in levels(as.factor(st$title))) {
    #   id <- which(st$title == i)
    #   nId <- length(id)
    #   if (nId > 5) trans <- trans3
    #   if (nId <= 5) trans <- trans2
    #   for(j in 1:nId) {
    #     st$col[id[j]] <- paste0(substr(st$col[id[j]],1,7), trans[j])
    #   }
    # }

    #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
    # Graph data
    #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#

    str <- data.frame(st_id = 1:nrow(st), stresseur = st$fullname)
    reg <- data.frame(rg_id = 1:nrow(cekm), region = cekm$region)

    dat <- cekm[, c("region", st$fullname)] %>%
           pivot_longer(cols = st$fullname,
                        names_to = "stresseur",
                        values_to = "cea") %>%
           left_join(str, by = "stresseur") %>%
           left_join(reg, by = "region") %>%
           left_join(st[, c('fullname','col')], by = c("stresseur" = "fullname")) %>%
           mutate(cea = cea / max(cea))


    #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
    # Graph
    #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
    png(glue('{output}{output_name}.png'), res = 300, width = 300, height = 300, units = "mm")
    layout(matrix(1:2, 2, 1), heights = c(.25,.75))

    #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
    # Map
    #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
    par(mar = c(0,2,0,2))
    cols <- regions_rotate$cea
    cols <- pal(101)[((regions_rotate$cea / max(regions_rotate$cea))*100)+1]
    # plot0(c(0,1), c(0,1))
    # plot(st_geometry(regions_rotate), col = cols, add = TRUE)
    plot(st_geometry(regions_rotate), col = cols)
    plot(st_geometry(regions_center), add = TRUE, pch = 20, col = "#909090", cex = 1.5)
    # points(x = vertical_lines_1, y = rep(385000, length(vertical_lines_1)), cex = 2,  col = 'red')
    for(i in 1:nrow(l)) lines(x = c(l$x1[i],l$x2[i]), y = c(l$y1[i],l$y2[i]), lty = 2, col = "#909090", lwd = 1.5)

    # Legend
    maxDat <- max(regions_rotate$cea)
    cols <- pal(101)[((regions_rotate$cea / maxDat)*100)+1]
    plot_legend_cont(
      range = range(regions_rotate$cea),
      pal = pal,
    )

    if (language == "fr") {
      lab <- "Effets cumulatifs par région administrative"
    } else if (language == "en") {
      lab <- "Cumulative effects per administrative region"
    }
    bbox <- st_bbox(regions_rotate)
    text(x = bbox$xmin + 1000,
         y = bbox$ymax,
         labels = lab,
         font = 2,
         adj = c(0,.5),
         cex = 1
       )

    if (!is.null(suffix)) {
      text(x = bbox$xmax - 2500,
           y = bbox$ymax,
           labels = suffix,
           font = 2,
           adj = c(1,.5),
           cex = 1
         )
    }

    if (language == "fr") {
      lab <- "Effets cumulatifs * $km^{-2}"
    } else if (language == "en") {
      lab <- "Cumulative effects * $km^{-2}"
    }
    text(x = bbox$xmin + 2500,
         y = bbox$ymax - 10000,
         labels = TeX(lab),
         font = 2,
         adj = c(0,.5),
         cex = .7
       )
    #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
    # Point graph
    #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
    par(family = 'serif')
    par(mar = c(0,2,0,2))
    xR <- c(-4, nrow(cekm))
    yR <- c(-5, nrow(st))
    plot0(x = xR, y = yR)
    for(i in 1:nrow(cekm)) lines(x = c(i,i), y = c(0.5,yR[2]+3), lty = 2, lwd = 1.5, col = "#909090")
    for(i in 1:nrow(st)) lines(x = c(0.75,xR[2]+.25), y = c(i,i), lty = 2, lwd = 1.5, col = "#90909066")
    points(x = dat$rg_id, y = dat$st_id, cex = dat$cea*4, col = "#000000", bg = dat$col, pch = 21)
    text(x = 1:xR[2], y = 0, labels = cekm$region, srt = 45, cex = .85, adj = c(1,.5))
    if (language == "fr") {
      lab <- st$francais 
    } else if (language == "en") {
      lab <- st$english
    }
    text(x = -3.6, y = 1:yR[2], labels = lab, cex = .85, adj = c(0,.5))

    # Stressor groups
    for(i in 1:nGroup) {
      if (grNames$size[i] > 1) {
        r1 <- ifelse(i == 1, 0, grNames$cumsize[i-1])
        grRange <- c(r1+1, grNames$cumsize[i])
        lines(x = c(-3.8,-3.8), y = c(grRange[1], grRange[2]))
        text(x = -4, y = mean(grRange), labels = gsub(" ", "\n", grNames$title[i]), 
             srt = 90, adj = c(.5,0), font = 3)
      }
    }
    
    #<=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
    dev.off()
  }

  # All
  temp("cumulative_effects_region_km2",
       "cumulative_effects_region_cea")

  # Suffix 
  if (lang == "fr") {
    nms <- c("Intégrité des berges","Habitats","Mammifères marins",
                "Sites d'intérêt culturels, patrimoniaux\net archéologiques")
  } else if (lang == "en") {
    nms <- c("Bank integrity","Habitats","Marine mammals",
                "Sites of cultural, heritage and\narcheological interest")
  }

  # Berge
  temp("cumulative_effects_region_km2_berge",
       "cumulative_effects_region_cea_berge",
       nms[1])

  # Habitat
  temp("cumulative_effects_region_km2_habitat",
       "cumulative_effects_region_cea_habitat",
       nms[2])

  # Mammifères marins
  temp("cumulative_effects_region_km2_mammiferes_marins",
       "cumulative_effects_region_cea_mammiferes_marins",
       nms[3])

  # Sites
  temp("cumulative_effects_region_km2_site",
       "cumulative_effects_region_cea_site",
       nms[4])

  # -----
  # Stack individual figures using magick package
  i1 <- magick::image_read(glue("{output}cumulative_effects_region_cea_berge.png"))
  i2 <- magick::image_read(glue("{output}cumulative_effects_region_cea_habitat.png"))
  i3 <- magick::image_read(glue("{output}cumulative_effects_region_cea_mammiferes_marins.png"))
  i4 <- magick::image_read(glue("{output}cumulative_effects_region_cea_site.png"))

  l1 <- image_append(c(i1,i2))
  l2 <- image_append(c(i3,i4))

  img <- image_append(c(l1,l2), stack = TRUE)
  magick::image_write(img, path = glue("{output}cumulative_effects_region_cea_panel.png"), format = "png")


} # end function
