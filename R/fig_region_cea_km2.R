#' Figure effects cumulatifs par région administrative par km2
#'
#' @keywords figure
#'
#' @export
#'
#' @examples
#' # Figure for specific dataset
#' fig_region_cea_km2()


fig_region_cea_km2 <- function() {

#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
# Data
#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
load_output("cumulative_effects_region_km2")
cekm <- cumulative_effects_region_km2
st <- read.csv("data/data-metadata/metadata_stresseurs.csv")

# Stressor fullnames
st <- mutate(st, fullname = glue("{stresseur}_{accronyme}"))

# Stressor groups
grNames <- unique(st[,c('stresseur','title')])
nGroup <- nrow(grNames)

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

center_coords <- sf::st_combine(x) %>% sf::st_centroid()
l <- list()
for(i in 1:nrow(regions)) {
  l[[i]] <- st_ellide_rotate(regions[i, ], 35, center_coords) %>%
            st_sf() %>%
            mutate(cea = regions$cea[i])
}
regions <- bind_rows(l)

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
trans2 <- c('FF','DD','AA','88','66','44')
trans3 <- c('FF','DD','BB','99','77','55','33','11','22','44','66','88','AA','CC','EE')
st$col <- gg_color_hue(nGroup)[as.numeric(as.factor(st$title))]
# for(i in 1:nrow(st)) st$col[i] <- darken(st$col[i], 20)

for(i in levels(as.factor(st$title))) {
  id <- which(st$title == i)
  nId <- length(id)
  if (nId > 5) trans <- trans3
  if (nId <= 5) trans <- trans2
  for(j in 1:nId) {
    st$col[id[j]] <- paste0(substr(st$col[id[j]],1,7), trans[j])
  }
}

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
       left_join(st[, c('fullname','col')], by = c("stresseur" = "fullname"))


#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
# Graph
#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
png('./figures/figures-output/cumulative_effects_region_cea.png', res = 300, width = 300, height = 300, units = "mm")
layout(matrix(1:2, 2, 1), heights = c(.25,.75))
par(mar = c(0,3,0,3))
cols <- regions$cea
cols <- pal(101)[((regions$cea / max(regions$cea))*100)+1]

plot(st_geometry(regions), col = cols)
# plot(st_geometry(aoi), add = TRUE)


par(family = 'serif')
par(mar = c(0,0,0,0))
xR <- c(-3, nrow(cekm))
yR <- c(-5, nrow(st))
plot0(x = xR, y = yR)
for(i in 1:nrow(cekm)) lines(x = c(i,i), y = c(1,yR[2]), lty = 3, col = "#00000044")
# for(i in 1:nrow(st)) lines(x = c(1,xR[2]), y = c(i,i), lty = 3, col = "#00000044")
points(x = dat$rg_id, y = dat$st_id, cex = dat$cea*1.5, col = "#000000", bg = dat$col, pch = 21)
text(x = 1:xR[2], y = 0, labels = cekm$region, srt = 45, cex = .85, adj = c(1,.5))
text(x = .5, y = 1:yR[2], labels = st$francais, cex = .85, adj = c(1,.5))

#<=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
dev.off()
} # end function
