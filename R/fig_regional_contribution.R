#' Figure contribution régionale par km2
#'
#' Contribution régionale des effets cumulatifs par km2 pour chaque stresseur sur chaque composante valorisée
#'
#' @keywords figure
#'
#' @export
#'
#' @examples
#' # Figure for specific dataset
#' fig_regional_contribution()


fig_regional_contribution <- function() {

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
cv$title[cv$title == "Sites d’intérêt"] <- "Sites d’intérêt culturels, patrimoniaux et archéologiques"
cv$type[cv$type == "Association de gestion halieutique Mi'kmaq et Malécite"] <- "AGHAMM"

#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
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
global_parameters() 
cols <- global_param$col$stresseurs
gg_color_hue <- colorRampPalette(cols)

#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
# Stresseurs
#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
st$col <- gg_color_hue(nGroup)[as.numeric(as.factor(st$title))]

for(i in levels(as.factor(st$title))) {
  id <- which(st$title == i)
  nId <- length(id)
  if (nId > 5) {
    for(j in 1:nId) {
      st$col[id[j]] <- lighten(st$col[id[j]], j*8)
    }    
  }
  if (nId <= 5) {
    for(j in 1:nId) {
      st$col[id[j]] <- lighten(st$col[id[j]], j*15)
    }        
  }
}



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
gr1$gr1 <- gsub("Intégrité des berges", "Intégrité\ndes berges", gr1$gr1)
gr2$gr2 <- gsub("Nation Wolastoqiyik Wahsipekuk","Nt. Wolastoqiyik\nWahsipekuk",gr2$gr2)
cekm$simple[cekm$francais == "Wolastoqiyik Wahsipekuk - Pêche traditionnelle"] <- "Wolastoqiyik Wahsipekuk - Pêche traditionnelle"
gr2$gr2 <- gsub("Naturelle","", gr2$gr2)
gr2$gr2 <- gsub("Artificielle","", gr2$gr2)
#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
# Prepare data for graphs
#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
# Param
xG = .3
yG = .02

# Direct intensity
cea <- cekm[, st$fullname]
cea_total <- rowSums(cea)

# Maximum values
maxVals <- c(max(cea_total), 0)

# Direct
di <- cea[, st$fullname] %>%
       t() %>%
       as.data.frame() %>%
       cumsum() %>%
       t() %>%
       cbind(temp = 0, .) %>%
       as.data.frame() %>%
       mutate(id = 1:n()) %>%
       gather("Stressor","ymax", -id) %>%
       arrange(id) %>%
       mutate(ymin = c(0,ymax[1:(length(ymax)-1)])) %>%
       filter(Stressor != 'temp') %>%
       mutate(ymax = ymax+yG, ymin = ymin+yG,
              xmax = id+xG, xmin = id-xG) %>%
       left_join(st[,c('fullname','col')], by = c('Stressor' = 'fullname'))

#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
# Graph
#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
png('./figures/figures-output/cumulative_effects_regional_contribution.png', res = 300, width = 600, height = 225, units = "mm")
layout(matrix(1:2, nrow = 2), heights = c(.8,.2))
par(family = 'serif')
par(mar = c(0,1,1,0))

yMax <- maxVals[1]+2.5
yMin <- -.1

plot0(x = c(-2,nrow(cea)), y = c(yMin,yMax+.1))

#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
# Graph elements
#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
#<=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
# Axes
dL <- c(yG, max(cea_total)+yG)

# Polygons
dat <- gr2[order(gr2$min), ]

for(i in seq(1,nrow(dat), by = 2)) polygon(x = c(rep(dat$min[i],2)-.5, rep(dat$max[i],2)+.5), y = c(yMin,yMax-.1,yMax-.1,yMin), col = '#f5f4f4', border = '#00000000')
# Lines
lines(x = c(-2,-2), y = dL, lwd = 1.5)
lines(x = c(0,nrow(cea)), y = c(0,0), lty = 2)
# Text
text(x = -3, y = mean(c(0, dL[2])), labels = 'Contribution moyenne des stresseurs aux effets cumulatifs', srt = 90, adj = .5, font = 2)

#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
# Contribution
#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
# axis(1);axis(2)
for(i in 1:nrow(di)) {
    x <- c(di$xmin[i],di$xmax[i],di$xmax[i],di$xmin[i])
    y <- c(rep(di$ymin[i],2), rep(di$ymax[i],2))
    polygon(x = x, y = y, border = '#00000000', col = di$col[i])
}


#<=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
# Groups
# 1st group
for(i in 1:nrow(gr1)) {
  x = as.numeric(gr1[i, c('min','max')])
  lines(y = rep(yMax-.1,2), x = x)
  text(y = yMax+.175, x = mean(x), adj = c(.5,.5), font = 2, labels = gr1$gr1[i])
}

# 2nd group
for(i in 1:nrow(gr2)) {
  x = as.numeric(gr2[i, c('min','max')])
  lines(y = rep(yMax-.55, 2), x = x)
  text(y = yMax-.35, x = mean(x), adj = .5, font = 1, labels = gr2$gr2[i], cex = .75)
}

#<=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
# Names
for(i in 1:nrow(cekm)) {

  text(x = i-xG/4, y = yMax-.75, labels = cekm$simple[i], cex = .75, srt = 90, adj = c(1,.5))
}

#<=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
# Drivers legend
yG = .3
par(mar = c(.5,0,0,0))
plot0(x = c(0,8), y = c(0.2,-12.5))
# Groups names
x = c(.5,1.4,2.4,3.4,4.4,5.5,7)
text(x = x, y = rep(0.15,4), labels = grNames$title, font = 2, adj = c(0,.5), cex = .85)
# Groups
for(j in 1:nGroup) {
  dat <- st[st$stresseur == grNames$stresseur[j], ]
  dat$id <- -(1:nrow(dat))
  for(i in 1:nrow(dat)) {
    xi <- c(x[j]+.05,rep(x[j]+.15, 2),x[j]+.05)
    y <- c(rep((dat$id[i]-yG),2), rep((dat$id[i]+yG),2))
    polygon(x = xi, y = y, border = '#585858', col = dat$col[i])
    text(x = x[j]+.2, y = dat$id[i], labels = dat$francais[i], cex = .7, adj = c(0,.5))
  }
}

#<=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=><=~-.-~=>
dev.off()
} # end function
