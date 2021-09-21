#' Plot legend
#'
#' Function to create a legend
#'
#' @rdname plot_legend
#'
#' @export
#'
#' @param range numeric, vector with minimal and maximal values
#' @param pal character, vector of colors, or color palette
#' @param cex.text numeric, cex for legend text
#' @param mainTitle character, type of legend. Choices are 'continuous', 'binary' or 'categorical'
#' @param subTitle character, type of legend. Choices are 'continuous', 'binary' or 'categorical'
#' @param type character, type of legend. Choices are 'continuous', 'binary' or 'categorical'
#' @param nTick numeric, number of ticks in the legend
#'
#' @return Opens a graphical interface with the plot
#'
#' @keywords plot, legend
plot_legend_cont <- function (range = c(0,1),
                              pal = NULL,
                              cexMain = 1,
                              cexSub = .75,
                              mainTitle = NULL,
                              subTitle = NULL,
                              n = 5) {
  # Legends
  # Palette
  if(class(pal) == 'character') {
    pal <- colorRampPalette(pal)
  }

  # Determine plot boundaries, in units of the data
  xmin <- par("usr")[1]
  xmax <- par("usr")[2]
  ymin <- par("usr")[3]
  ymax <- par("usr")[4]
  xR <- xmax - xmin
  yR <- ymax - ymin

  xinit <- xmin + .041*xR # minimum left side to write
  yinit <- ymax - .175*yR # minimum upper side to write
  ygap <- .056*yR
  xgap <- .014*xR
  ybarUp <- yinit - ygap/2 - .0041*yR
  ybarDn <- yinit - ygap -ygap/2 + .0041*yR


  # Plot
   x <- seq(from = xinit, to = xinit + .17*xR, by = .0003*xR)
   z <- data.frame(y1 = ybarUp,
                  y2 = ybarDn,
                  x1 = x[1:length(x)-1],
                  x2 = x[2:length(x)],
                  col = pal(length(x)-1),
                  stringsAsFactors = F)
   for(k in 1:nrow(z)) {
    polygon(x = c(z$x1[k],z$x2[k],z$x2[k],z$x1[k],z$x1[k]),
            y = c(z$y1[k],z$y1[k],z$y2[k],z$y2[k],z$y1[k]),
            col = z$col[k],
            border = z$col[k])
   }

   # Add axis
   x <- seq(from = xinit, to = xinit + .17*xR, length.out = n)
   lines(x = c(xinit, xinit + .17*xR), y = rep(z$y2[1], 2))
   for(i in 1:n) lines(x = rep(x[i],2), y = c(z$y2[1], z$y2[1]- .003*yR))

   text(x = x,
        y =  rep(z$y2[1] - .01*yR, n),
        labels = round(seq(from = 0, to = max(range[2]), length.out = n)),
        cex = cexSub*.75,
        adj = c(1, 1),
        srt = 45)

  # Add titles
  yText <- ybarUp + .035*yR

  # Add sub text
  if(!is.null(subTitle)) {
    text(x = xinit,
         y = yText,
         labels = TeX(subTitle, italic = TRUE),
         cex = cexSub,
         adj = c(0,1))
     yText <- yText + .0224*yR
   }

  # Add main title
  if(!is.null(mainTitle)) {
  text(x = xinit,
       y = yText,
       labels = mainTitle,
       cex = cexMain,
       font = 2,
       adj = c(0,1))
  }
}


# =================================================================
#' @rdname plot_legend
#' @export
plot_legend_bin <- function (col,
                             cexMain = 1,
                             cexSub = .75,
                             mainTitle = NULL,
                             subTitle = NULL) {

  # Determine plot boundaries, in units of the data
  xmin <- par("usr")[1]
  xmax <- par("usr")[2]
  ymin <- par("usr")[3]
  ymax <- par("usr")[4]
  xR <- xmax - xmin
  yR <- ymax - ymin

  xinit <- xmin + .041*xR # minimum left side to write
  yinit <- ymax - .175*yR # minimum upper side to write
  ygap <- .056*yR
  xgap <- .014*xR
  ybarUp <- yinit - ygap/2 - .0041*yR
  ybarDn <- yinit - ygap -ygap/2 + .0041*yR


  # Plot
  sq <- .05
  polygon(x = c(xinit, xinit, xinit + sq*xR, xinit + sq*xR, xinit),
          y = c(ybarUp,ybarDn,ybarDn,ybarUp,ybarUp),
          col = col,
          border = "#000000")

  # Add titles
  yText <- ybarUp + .035*yR

  # Add sub text
  if(!is.null(subTitle)) {
    text(x = xinit,
         y = yText,
         labels = TeX(subTitle, italic = TRUE),
         cex = cexSub,
         adj = c(0,1))
     yText <- yText + .0224*yR
   }

  # Add main title
  if(!is.null(mainTitle)) {
  text(x = xinit,
       y = yText,
       labels = mainTitle,
       cex = cexMain,
       font = 2,
       adj = c(0,1))
  }
}
