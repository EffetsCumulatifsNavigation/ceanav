#' Figure metanetwork
#'
#' @keywords figure
#'
#' @export
#'
#' @examples
#' # Figure for specific dataset
#' fig_metanetwork()


fig_metanetwork_en <- function() {

#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
# Data
#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
load_output("cumulative_effects_cv_km2")
cekm <- cumulative_effects_cv_km2
st <- read.csv("data/data-metadata/metadata_stresseurs.csv")
cv <- read.csv("data/data-metadata/metadata_composantes_valorisees.csv")
 
# #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
# # Manually modify certain names to 
# # make graph clearer
# #=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
# Composantes valorisées
cv$title_en[cv$title_en == "Areas of interest"] <- "Areas of cultural, heritage and archeological interest"
cv$type_en[cv$type_en == "Mi'kmaq and Maliseet Aboriginal Fisheries Management Association"] <- "MMAFMA"

#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
# Stressor groups
grNames <- unique(st[,c('stresseur','title_en')]) %>%
           rename(title = title_en)
nGroup <- nrow(grNames)

# Stressor fullnames
st <- mutate(st, fullname = glue("{stresseur}_{accronyme}"))

#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
# Function to add transparent nodes for spacing between each node groups
#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
insertRow <- function(dat, uid, group, network) {
  r1 <- min(uid)
  r2 <- max(uid)+2
  randomString <- function() paste0(letters[runif(20,1,26)], collapse = '')
  newrow <- data.frame(group = group, network = network, name = randomString(), 
                       cex = 0, cols = '#00000000', stringsAsFactors = FALSE)
  dat[seq(r1+1,nrow(dat)+1),] <- dat[seq(r1,nrow(dat)),]
  dat[r1,] <- newrow
  dat[seq(r2+1,nrow(dat)+1),] <- dat[seq(r2,nrow(dat)),]
  dat[r2,] <- newrow
  dat
}

#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
# Colors
#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
# Colors for stressors
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100, alpha = .5)[1:n]
}
global_parameters() 
cols <- global_param$col$stresseurs
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
for(i in 1:length(grNames$cols)) grNames$cols[i] <- darken(grNames$cols[i], 30)


#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
# Valued components groups
#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
cv <- cv %>%
      mutate(gr1 = title_en,
             gr2 = type_en,
             fullname = glue("{comp_val}_{accronyme}"))

# Join cea data and reorder
cekm <- left_join(cekm, cv, by = c('cv' = 'fullname')) %>%
        # arrange(gr1, gr2, gr3, desc(impactNormalized))
        arrange(title, type, desc(cea))

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
# gr1$gr1 <- gsub("Intégrité des berges", "Berges", gr1$gr1)
# gr2$gr2 <- gsub("Wolastoqiyik Wahsipekuk","",gr2$gr2)
# cekm$simple[cekm$francais == "Wolastoqiyik Wahsipekuk - Pêche traditionnelle"] <- "Wolastoqiyik Wahsipekuk - Pêche traditionnelle"
# gr2$gr2 <- gsub("Naturelle","", gr2$gr2)
# gr2$gr2 <- gsub("Artificielle","", gr2$gr2)




#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
# Link and nodes for drivers
#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
# <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
# Nodes list with proper groups included
nodesTx <- data.frame(group = cekm$gr1, network = cekm$gr2, name = cekm$cv, stringsAsFactors = FALSE)
nodesTx$cex <- cekm$cea*.4

# <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
# Add colors to node now
metanetwork <- list()
metanetwork$nodes <- nodesTx
metanetwork$links <- data.frame(1)

colBerge <- '#356638'
colHab <- '#ad7c27'
colMM <- '#734444'
colSite <- '#0f2e40'

trans <- c('FF','DD','BB','99','77','55','33')
cols <- c(paste0(colHab, trans[c(1,3,5)]),
          paste0(colBerge, trans[c(1,4)]),
          paste0(colMM, trans[c(1,4)]),
          paste0(colSite, trans))
pal_insileco <- cols


# Colors
metanetwork$networkGroup <- bound(metanetwork, order = unique(nodesTx$network))
metanetwork <- colGroups(metanetwork, colPal = pal_insileco)
nodesTx <- metanetwork[[1]]
colGr <- metanetwork[[3]][,c('Var1','cols')]

# <=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
# Add blank spaces 
# Cycles de vie
uid <- which(nodesTx$network == "Life cycles")
nodesTx <- insertRow(nodesTx, uid, "Habitats","Life cycles")

# Artificielle
uid <- which(nodesTx$network == "Artificial")
nodesTx <- insertRow(nodesTx, uid, "Bank integrity","Artificial")

# Naturelle
uid <- which(nodesTx$network == "Natural")
nodesTx <- insertRow(nodesTx, uid, "Bank integrity","Natural")

# AGHAMM
uid <- which(nodesTx$network == "MMAFMA")
nodesTx <- insertRow(nodesTx, uid, "Areas of cultural, heritage and archeological interest","MMAFMA")

# Nation Mohawk de Kahnawà:ke
uid <- which(nodesTx$network == "Mohawk Nation of Kahnawà:ke")
nodesTx <- insertRow(nodesTx, uid, "Areas of cultural, heritage and archeological interest","Mohawk Nation of Kahnawà:ke")

# Nation Wolastoqiyik Wahsipekuk
uid <- which(nodesTx$network == "Wolastoqiyik Wahsipekuk Nation")
nodesTx <- insertRow(nodesTx, uid, "Areas of cultural, heritage and archeological interest","Wolastoqiyik Wahsipekuk Nation")

# Public
uid <- which(nodesTx$network == "Public")
nodesTx <- insertRow(nodesTx, uid, "Areas of cultural, heritage and archeological interest","Public")

# Remove last line if empty
ll <- nrow(nodesTx)
if (is.na(nodesTx$group[ll])) nodesTx <- nodesTx[-ll, ]

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
           dplyr::select(title_en, fullname, cols) %>%
           rename(network = title_en, name = fullname, cols = cols) %>%
           left_join(cexDr, by = c('name'='drivers')) %>%
           mutate(group = 'Environmental stressors') %>%
           dplyr::select(group, network, name, cex, cols)

# Add blank spaces
# Ancrage
uid <- which(nodesDr$network == "Anchorage")
nodesDr <- insertRow(nodesDr, uid, "Environmental stressors","Anchorage")

# Naufrages
uid <- which(nodesDr$network == "Shipwreck")
nodesDr <- insertRow(nodesDr, uid, "Environmental stressors","Shipwreck")

# Pollution maritime
uid <- which(nodesDr$network == "Marine pollution")
nodesDr <- insertRow(nodesDr, uid, "Environmental stressors","Marine pollution")

# Pêche commerciale
uid <- which(nodesDr$network == "Commercial fishing")
nodesDr <- insertRow(nodesDr, uid, "Environmental stressors","Commercial fishing")

# Dragage
uid <- which(nodesDr$network == "Dredging")
nodesDr <- insertRow(nodesDr, uid, "Environmental stressors","Dredging")

# Déversements accidentels
uid <- which(nodesDr$network == "Accidental spills")
nodesDr <- insertRow(nodesDr, uid, "Environmental stressors","Accidental spills")

# Remove last line if empty
ll <- nrow(nodesDr)
if (is.na(nodesDr$group[ll])) nodesDr <- nodesDr[-ll, ]

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
# Graph 
#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
png('./figures_en/figures-output/cumulative_effects_metanetwork.png', res = 300, width = 300, height = 300 ,units = "mm")
# Plot
par(mar = c(2,2,2,2))
plot0(x = c(-1.1, 1.1))

# Remove problematic names 
nmrm <- c("Marine pollution","Accidental spills","Wolastoqiyik Wahsipekuk Nation")
uid <- numeric()
for(i in 1:length(nmrm)) uid[i] <- which(metanetwork$networkGroup$Var1 == nmrm[i])
colNames <- rep("#ffffff", nrow(metanetwork$networkGroup))
colNames[uid] <- metanetwork$networkGroup$cols[uid]
metanetwork$networkGroup$Var1[uid] <- "."

boxGroup(metanetwork,
         rad1 = rad1,
         colBox = metanetwork$networkGroup$cols,
         colNames = colNames,
         border = 'transparent',
         # border = '#000000',
         cexNetwork = .6)

# # Problematic names 
metanetwork$networkGroup$Var1[uid] <- nmrm
arctext2 <- function(var1, l1, l2, cl = TRUE, cx = .6) {
  uid <- metanetwork$networkGroup$Var1 == var1
  middle <- mean(c(metanetwork$networkGroup$lower[uid],
                   metanetwork$networkGroup$upper[uid]))
  plotrix::arctext(x = as.character(l1),radius = rad2-.02, middle = middle, 
                   col = "#ffffff", clockwise = cl, font = 2, cex = cx)
  plotrix::arctext(x = as.character(l2), radius = rad1+.02, middle = middle, 
                   col = "#ffffff", clockwise = cl, font = 2, cex = cx)  
}
arctext2("Accidental spills", "Accidental", "spills")
arctext2("Marine pollution", "Marine", "marine", cx = .55)
arctext2("Wolastoqiyik Wahsipekuk Nation", "Wahsipekuk Nt.", "Wolastoqiyik", cl = FALSE, cx = .55)

plotLinks(metanetwork, col = metanetwork$links$col)

if (shadowEdge) {
  points(metanetwork$nodes$x,
         metanetwork$nodes$y,
         pch = 20,
         cex = (metanetwork$nodes$cex * 4),
         col = '#d7d7d7')
}

points(metanetwork$nodes$x,
       metanetwork$nodes$y,
       pch = 20,
       cex = (metanetwork$nodes$cex * 2.5),
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

#<=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
dev.off()
} # end function
