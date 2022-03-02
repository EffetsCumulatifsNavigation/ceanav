#' Function to generate a heatmap with given data, and rows and columns names
#'
#' @keywords figure
#'
#' @export
#'


fnc_heatmap <- function(dat, columns, rows, gr, output) {

  # # Simulate data
  # nC <- 20
  # dat <- round(matrix(nrow = nR, ncol = nC, data = runif(16*10)),2)
  # rows <- paste0("row", 1:nR)
  # columns <- paste0("col", 1:nC)
  # output <- "./figures/figures-vulnerability/heatmap.png"

  # Some simplifies parameters
  nC <- length(columns)
  nR <- length(rows)

  # colors
  pal <- colorRampPalette(viridis::viridis(100))
  cols <- paste0(pal(101)[((dat / 1)*100)+1], "99")
  colsDat <- dat
  colsDat[1:length(colsDat)] <- cols

  # Graph
  width <- 100 + nC*5 # largeur texte + nombre colonnes
  height <- 80 + nR*5 # hauteur texte + nombre lignes
  png(output, res = 300, width = width, height = height, units = "mm", pointsize = 8)
  par(mar = c(1,1,1,1))
  plot0(x = c(-13,nC+5), y = c(1,(nR+6)))

  # Heatmap
  for(y in 1:nR) {
    for(x in 1:nC) {
      rect(x-.5, y-.5, x+.5, y+.5, col = colsDat[y,x])
    }
  }

  # Text
  text(x = 1:nC, y = nR+.75, labels = columns, srt = 65, adj = c(0,.5), cex = .9)
  text(x = 1-.75, y = 1:nR, labels = rows, adj = c(1,.5), cex = .9)
  for(i in 1:nR) text(x = 1:nC, y = i, labels = dat[i, ], adj = c(.5,.5), cex = .7)
  
  # Stressor groups
  nGroup <- nrow(gr)
  grNames <- gr
  for(i in 1:nGroup) {
    if (grNames$size[i] > 1) {
      r1 <- ifelse(i == 1, 0, grNames$cumsize[i-1])
      grRange <- c(r1+1, grNames$cumsize[i])
      lines(x = c(nC+1,nC+1), y = c(grRange[1], grRange[2]))
      text(x = nC+1.5, y = mean(grRange), labels = gsub(" ", "\n", grNames$title[i]), 
           srt = 90, adj = c(.5,1), font = 3)
    }
  }
  
  # -----
  dev.off()
}
