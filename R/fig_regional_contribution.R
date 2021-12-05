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
load_output("cumulative_effects_km2")
cekm <- cumulative_effects_km2
st <- read.csv("data/data-metadata/metadata_stresseurs.csv")
cv <- read.csv("data/data-metadata/metadata_composantes_valorisees.csv")

# Stressor groups
grNames <- unique(st[,c('stresseur','title')])
nGroup <- nrow(grNames)

#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
# Colors
#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
# Colors for stressors
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100, alpha = .5)[1:n]
}

#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
# Stresseurs
#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
trans <- c('FF','DD','BB','99','77','55','33','11')
trans <- c('FF','DD','AA','88','66','44')
st$col <- gg_color_hue(nGroup)[as.numeric(as.factor(st$title))]
for(i in 1:nrow(st)) st$col[i] <- darken(st$col[i], 20)

for(i in levels(as.factor(st$title))) {
  id <- which(st$title == i)
  nId <- length(id)
  for(j in 1:nId) {
    st$col[id[j]] <- paste0(substr(st$col[id[j]],1,7), trans[j])
  }
}


#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
# Valued components groups
#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
cv <- cv %>%
      mutate(gr1 = title,
             gr2 = group,
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
gr2$gr2 <- gsub('Others2','Others',gr2$gr2)

# gr3 <- cekm[, 'gr3', drop = FALSE] %>%
#           mutate(id = 1:n()) %>%
#           group_by(gr3) %>%
#           summarize(min = min(id), max = max(id)) %>%
#           filter(gr3 != 'X') %>%
#           as.data.frame(stringsAsFactors = FALSE)





} # end function
