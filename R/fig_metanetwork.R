#' Figure metanetwork
#'
#' @keywords figure
#'
#' @export
#'
#' @examples
#' # Figure for specific dataset
#' fig_metanetwork()


fig_metanetwork <- function() {

#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
# Data
#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
load_output("cumulative_effects_cv_km2")
cekm <- cumulative_effects_km2
st <- read.csv("data/data-metadata/metadata_stresseurs.csv")
cv <- read.csv("data/data-metadata/metadata_composantes_valorisees.csv")

# Stressor groups
grNames <- unique(st[,c('stresseur','title')])
nGroup <- nrow(grNames)

# Stressor fullnames
st <- mutate(st, fullname = glue("{stresseur}_{accronyme}"))


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

# Groups
grNames$cols <- gg_color_hue(nGroup)[as.numeric(as.factor(grNames$stresseur))]
for(i in 1:length(grNames$cols)) grNames$cols[i] <- darken(grNames$cols[i], 10)


#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
# Valued components groups
#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
cv <- cv %>%
      mutate(gr1 = title,
             gr2 = type,
             fullname = glue("{comp_val}_{accronyme}"))

# Join cea data and reorder
cekm <- left_join(cekm, cv, by = c('cv' = 'fullname')) %>%
        # arrange(gr1, gr2, gr3, desc(impactNormalized))
        arrange(gr1, gr2, desc(cea))

# Group location on graph
gr1 <- cekm[, 'gr1', drop = FALSE] %>%
          mutate(id = 1:n()) %>%
          group_by(gr1) %>%
          summarize(min = min(id), max = max(id)) %>%
          as.data.frame(stringsAsFactors = FALSE)

gr2 <- cekm[, 'gr2', drop = FALSE] %>%
          mutate(id = 1:n()) %>%
          group_by(gr2) %>%
          summarize(min = min(id), max = max(id)) %>%
          as.data.frame(stringsAsFactors = FALSE)

# gr3 <- cekm[, 'gr3', drop = FALSE] %>%
#           mutate(id = 1:n()) %>%
#           group_by(gr3) %>%
#           summarize(min = min(id), max = max(id)) %>%
#           filter(gr3 != 'X') %>%
#           as.data.frame(stringsAsFactors = FALSE)

# Manually adjust some names
gr1$gr1 <- gsub("Intégrité des berges", "Berges", gr1$gr1)
gr2$gr2 <- gsub("Wolastoqiyik Wahsipekuk","",gr2$gr2)
cekm$simple[cekm$francais == "Wolastoqiyik Wahsipekuk - Pêche traditionnelle"] <- "Wolastoqiyik Wahsipekuk - Pêche traditionnelle"
gr2$gr2 <- gsub("Naturelle","", gr2$gr2)
gr2$gr2 <- gsub("Artificielle","", gr2$gr2)




#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
# Link and nodes for drivers
#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
# <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
# Nodes list with proper groups included
nodesTx <- data.frame(group = cekm$gr1, network = cekm$gr2, name = cekm$cv, stringsAsFactors = FALSE)
nodesTx$cex <- cekm$cea*1.25

# <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
# Add colors to node now
metanetwork <- list()
metanetwork$nodes <- nodesTx
metanetwork$links <- data.frame(1)

colBerge <- '#1ba28e'
colHab <- '#ad7c27'
colMM <- '#734444'
colSite <- '#7343fc'

trans <- c('FF','DD','BB','99','77','55')
cols <- c(paste0(colBerge, trans[c(1,4)]),
          paste0(colHab, trans[c(1,3,5)]),
          paste0(colMM, trans[c(1,4)]),
          paste0(colSite, trans))
pal_insileco <- cols


# Colors
metanetwork$networkGroup <- bound(metanetwork, order = unique(nodesTx$network))
metanetwork <- colGroups(metanetwork, colPal = pal_insileco)
nodesTx <- metanetwork[[1]]
colGr <- metanetwork[[3]][,c('Var1','cols')]

#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
# Link and nodes for drivers
#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
# Direct intensity
dat <- cekm[, st$fullname] %>% round(2)

# <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
# Links with taxa
# <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
# For now, only select links > 0.05
# For now, select all links
uid <- which(dat > 0.00, arr.ind = T)
links <- matrix(nrow = nrow(uid), ncol = 2, dimnames = list(c(), c('from','to')))
for(i in 1:nrow(uid)) {
  links[i, 'from'] <- colnames(dat)[uid[i,2]]
  links[i, 'to'] <- cekm$cv[uid[i,1]]
}
linksDr <- as.data.frame(links, stringsAsFactors = FALSE)

# Add color for links
linksDr <- left_join(linksDr, st[,c('fullname','col')], by = c('from' = 'fullname'))

# <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
# Nodes
# <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
# Node size
cexDr <- data.frame(drivers = colnames(dat), cex = log(colMeans(dat)+1)*10+.4, stringsAsFactors = FALSE)

nodesDr <- st %>%
           left_join(grNames, by = "stresseur") %>%
           dplyr::select(title.x, fullname, cols) %>%
           rename(network = title.x, name = fullname, cols = cols) %>%
           left_join(cexDr, by = c('name'='drivers')) %>%
           mutate(group = 'Stresseurs') %>%
           dplyr::select(group, network, name, cex, cols)

# # Add transparent nodes for spacing between each stressors
# randomString <- function() paste0(letters[runif(20,1,26)], collapse = '')
#
#
#
# # Ancrage
# x <- data.frame(group = 'Stressors', network = 'Marine traffic', name = randomString(), cex = 0,
#

#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
# Combine
#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
# Combine in a single object
metanetwork <- vector('list', 0)
metanetwork$nodes <- rbind(nodesDr, nodesTx)
metanetwork$links <- linksDr


#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
# Graph elements
#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
# Network order
orderNet <- c(unique(nodesDr$network), unique(nodesTx$network))

# Network boundaries
metanetwork$networkGroup <- bound(metanetwork, order = orderNet)

# Node coordinates
metanetwork <- nodePos(metanetwork, edgeRad = .875, groupRad = .6)

# Manually add colors
# metanetwork[[3]]$cols <- NA
metanetwork[[3]] <- left_join(metanetwork[[3]], grNames[, c('title','cols')], by = c('Var1' = 'title'))
metanetwork[[3]]$cols[match(colGr$Var1, metanetwork[[3]]$Var1)] <- colGr$cols

# Others
rad1 = .925
rad2 = 1
shadowEdge = TRUE

#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
# Graph elements
#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#

png('./figures/figures-output/cumulative_effects_metanetwork.png', res = 300, width = 300, height = 300 ,units = "mm")
# Plot
par(mar = c(2,2,2,2))
plot0(x = c(-1.1, 1.1))

metanetwork$networkGroup$Var1[12] <- 'Others'
boxGroup(metanetwork,
         rad1 = rad1,
         colBox = metanetwork$networkGroup$cols,
         colNames = metanetwork$networkGroup$colNames,
         border = 'transparent',
         # border = '#000000',
         cexNetwork = .75)

plotLinks(metanetwork, col = metanetwork$links$col)

if (shadowEdge) {
  points(metanetwork$nodes$x,
         metanetwork$nodes$y,
         pch = 20,
         cex = (metanetwork$nodes$cex * 5),
         col = '#d7d7d7')
}

points(metanetwork$nodes$x,
       metanetwork$nodes$y,
       pch = 20,
       cex = (metanetwork$nodes$cex * 3),
       col = metanetwork$nodes$cols)


#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
# Extra elements to graph
#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
# Add Vertebrates, Invertebrates and Stressors
metanetwork$nodes$network <- metanetwork$nodes$group
metanetwork$networkGroup <- bound(metanetwork, order = unique(metanetwork$nodes$network))
metanetwork <- nodePos(metanetwork, edgeRad = .875, groupRad = .5)

boxGroup2(metanetwork,
         rad1 = 1.03, rad2 = 1.13,
         colBox = '#00000000', colNames = '#000000',
         border = '#000000',
         cexNetwork = 1.1)

# Letter
text(x = -1.1, y = 1.065, labels = 'b', cex = 1.5, font = 2)





#<=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
dev.off()
} # end function
