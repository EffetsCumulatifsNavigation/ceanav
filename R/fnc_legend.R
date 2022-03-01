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
#' @param minUp numeric, minimum upper side to write as a function of bbox extent
#' @param showNA logical, add box that shows regions identified as NA in the figure
#'
#' @return Opens a graphical interface with the plot
#'
#' @keywords plot, legend
plot_legend_cont <- function (range = c(0,1),
                              pal = NULL,
                              cexMain = 1,
                              cexSub = .75,
                              minUp = .175,
                              mainTitle = NULL,
                              subTitle = NULL,
                              n = 5,
                              showNA = FALSE) {
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
  yinit <- ymax - minUp*yR # minimum upper side to write
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

   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # 0 values as transparent
   g0 <- .010*xR 
   g1 <- .040*xR
   nz <- nrow(z)
   polygon(x = c(z$x2[nz]+g0,z$x2[nz]+g1,z$x2[nz]+g1,z$x2[nz]+g0,z$x2[nz]+g0),
           y = c(z$y1[k],z$y1[k],z$y2[k],z$y2[k],z$y1[k]),
           col = "#00000000",
           border = "#000000", 
           lwd = .5)
   text(labels = "0", x = mean(c(z$x2[nz]+g0,z$x2[nz]+g1)), y = mean(c(z$y1, z$y2)), adj = c(.5,.5), cex = cexSub*.75)
   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # NA values for regions without data
   if (showNA) {
     g0 <- .050*xR 
     g1 <- .080*xR
     nz <- nrow(z)
     polygon(x = c(z$x2[nz]+g0,z$x2[nz]+g1,z$x2[nz]+g1,z$x2[nz]+g0,z$x2[nz]+g0),
             y = c(z$y1[k],z$y1[k],z$y2[k],z$y2[k],z$y1[k]),
             col = global_param$col$naValues,
             border = "#000000", 
             lwd = .5)
     text(labels = "NA", x = mean(c(z$x2[nz]+g0,z$x2[nz]+g1)), y = mean(c(z$y1, z$y2)), adj = c(.5,.5), cex = cexSub*.75)        
   }
   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   # Labels
   if(range[2] <= 5) {
     lab <- round(seq(from = 0, to = max(range[2]), length.out = n), 2)     
   } else {
     lab <- round(seq(from = 0, to = max(range[2]), length.out = n))     
   }
   
   # Add that it's > 0, not 0
   lab[1] <- glue("> {lab[1]}")
   
   text(x = x,
        y =  rep(z$y2[1] - .01*yR, n),
        labels = lab,
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
                             minUp = .175,
                             mainTitle = NULL,
                             subTitle = NULL,
                             showNA = FALSE) {

  # Determine plot boundaries, in units of the data
  xmin <- par("usr")[1]
  xmax <- par("usr")[2]
  ymin <- par("usr")[3]
  ymax <- par("usr")[4]
  xR <- xmax - xmin
  yR <- ymax - ymin

  xinit <- xmin + .041*xR # minimum left side to write
  yinit <- ymax - minUp*yR # minimum upper side to write
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

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # NA values for regions without data
  if (showNA) {
    g0 <- .090*xR 
    polygon(x = c(xinit+g0,xinit+g0+sq*xR,xinit+g0+sq*xR,xinit+g0,xinit+g0),
            y = c(ybarUp,ybarUp,ybarDn,ybarDn,ybarUp),
            col = global_param$col$naValues,
            border = "#000000")
    text(labels = "NA", x = mean(c(xinit+g0,xinit+g0+sq*xR)), y = mean(c(ybarUp,ybarDn)), adj = c(.5,.5), cex = cexSub*.75)        
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



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
