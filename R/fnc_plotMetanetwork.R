#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
# Let's begin by creating a function that will give us the x and y coordinates
# of the outside of a circle given a certain radius
coordCircle <- function(theta = NULL, radius = 1) {
  data.frame(x = radius * cos(theta),
             y = radius * sin(theta))
  }
#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#


#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
bound <- function(metanetwork, order = NULL, gap = .05, addGap = T) {
  # Metanetwork list composed of "nodes" and "links"
  # Size of gap between groups on the graph
  # addGap logical whether to add gap or not
  nGroup <- as.data.frame(table(metanetwork$nodes$network))
  nGroup <- nGroup[match(order, nGroup$Var1), ]
  nGroup$Prop <- nGroup$Freq / sum(nGroup$Freq)
  nGroup$spanDeg <- 2 * pi * nGroup$Prop
  nGroup$upper <- nGroup$lower <- 0
  for(i in 2:nrow(nGroup)) nGroup$lower[i] <- nGroup$lower[i-1] + nGroup$spanDeg[i-1]
  nGroup$upper <- nGroup$lower + nGroup$spanDeg

  if (addGap) {
    nGroup$lower <- nGroup$lower + gap/2
    nGroup$upper <- nGroup$upper - gap/2
  }

  nGroup
}
#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
# plot0()
# points(metanetwork$nodes$x, metanetwork$nodes$y, pch = 20, cex = 2)
# points(metanetwork$networkGroup$x, metanetwork$networkGroup$y, pch = 20, cex = 2)
#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
nodePos <- function(metanetwork, edgeRad = 0.975, groupRad = 0.5, gapEdge = 0.1, addGap = T) {
    # Add x and y columns to nodes and networkGroup data
      metanetwork$nodes$y <- metanetwork$nodes$x <- 0
      metanetwork$networkGroup$y <- metanetwork$networkGroup$x <- 0

    # Get coordinates for all networks
      for(i in 1:nrow(metanetwork$networkGroup)) {
        # Distribute points within each network space
          edgeDeg <- seq((metanetwork$networkGroup$lower[i] + (gapEdge/2)),
                         (metanetwork$networkGroup$upper[i] - (gapEdge/2)),
                         length = metanetwork$networkGroup$Freq[i])

        # Get position for each edge
          nodePos <- coordCircle(theta = edgeDeg, radius = edgeRad)

        # Add to nodes data
          metanetwork$nodes$x[metanetwork$nodes$network == metanetwork$networkGroup$Var1[i]] <- nodePos$x
          metanetwork$nodes$y[metanetwork$nodes$network == metanetwork$networkGroup$Var1[i]] <- nodePos$y

        # Distribute network groups in space
          groupDeg <- mean(c(metanetwork$networkGroup$lower[i],metanetwork$networkGroup$upper[i]))

        # Get position for each group
          groupPos <- coordCircle(theta = groupDeg, radius = groupRad)

        # Add to group data
          metanetwork$networkGroup$x[i] <- groupPos$x
          metanetwork$networkGroup$y[i] <- groupPos$y
      }

  metanetwork
}
#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#



#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
boxGroup <- function(metanetwork, rad1 = .95, rad2 = 1, colBox = NULL, names = NULL, colNames = NULL, addNames = T, cexNetwork = 1, ...) {
  # metanetwork = data list composed of 'nodes', 'links' & 'networkGroup'
  # rad1 = lower boundary for polygons
  # rad2 = upper boundary for polygons
  # colBox = color of boxes
  # names = names of individual networks
  # colNames = color of names
  # addNames = logical, add names of networks to graph
  if (!is.null(colNames) & length(colNames) == 1) {
    colNames <- rep(colNames, nrow(metanetwork$links))
  }

  if (!is.null(colBox) & length(colBox) == 1) {
    colBox <- rep(colBox, nrow(metanetwork$links))
  }

  for(i in 1:nrow(metanetwork$networkGroup)) {
    a <- coordCircle(theta = seq(metanetwork$networkGroup$lower[i],
                             metanetwork$networkGroup$upper[i],
                             length = 200),
                     radius = rad1)

    b <- coordCircle(theta = seq(metanetwork$networkGroup$upper[i],
                             metanetwork$networkGroup$lower[i],
                             length = 200),
                     radius = rad2)

    polygon(rbind(a, b, a[1L,]), col = colBox[i], ...)

    if (addNames) {
      middle <- mean(c(metanetwork$networkGroup$lower[i],
                       metanetwork$networkGroup$upper[i]))
      clockwise <- if (middle > pi) F else T
      plotrix::arctext(x = as.character(metanetwork$networkGroup$Var1[i]),
                       radius = mean(c(rad1,rad2)),
                       middle = middle,
                       col = colNames[i],
                       clockwise = clockwise,
                       font = 2,
                       cex = cexNetwork)
    }
  }
}
#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#

#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
boxGroup2 <- function(metanetwork, rad1 = .95, rad2 = 1, colBox = NULL, names = NULL, colNames = NULL, addNames = T, cexNetwork = 1, ...) {
  # metanetwork = data list composed of 'nodes', 'links' & 'networkGroup'
  # rad1 = lower boundary for polygons
  # rad2 = upper boundary for polygons
  # colBox = color of boxes
  # names = names of individual networks
  # colNames = color of names
  # addNames = logical, add names of networks to graph
  if (!is.null(colNames) & length(colNames) == 1) {
    colNames <- rep(colNames, nrow(metanetwork$links))
  }

  if (!is.null(colBox) & length(colBox) == 1) {
    colBox <- rep(colBox, nrow(metanetwork$links))
  }

  for(i in 1:nrow(metanetwork$networkGroup)) {
    a <- coordCircle(theta = seq(metanetwork$networkGroup$lower[i],
                             metanetwork$networkGroup$upper[i],
                             length = 200),
                     radius = rad1)

    b <- coordCircle(theta = seq(metanetwork$networkGroup$upper[i],
                             metanetwork$networkGroup$lower[i],
                             length = 200),
                     radius = rad2)

    # polygon(rbind(a, b, a[1L,]), col = colBox[i], ...)
    # polygon(rbind(a, b, a[1L,]), col = '#00000000', ...)
    lines(a, col = '#000000', lwd = 3)

    if (addNames) {
      middle <- mean(c(metanetwork$networkGroup$lower[i],
                       metanetwork$networkGroup$upper[i]))
      clockwise <- if (middle > pi) F else T
      plotrix::arctext(x = as.character(metanetwork$networkGroup$Var1[i]),
                       radius = mean(c(rad1,rad2)),
                       middle = middle,
                       col = colNames[i],
                       clockwise = clockwise,
                       font = 2,
                       cex = cexNetwork)
    }
  }
}
#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#


#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
plotLinks <- function(metanetwork, cols = NULL, ...) {
    if (!is.null(cols) & length(cols) == 1) {
      cols <- rep(cols, nrow(metanetwork$links))
    }

    for(i in 1:nrow(metanetwork$links)) {
      link <- metanetwork$links[i,]
      edgeFromID <- which(metanetwork$nodes$name == link$from)
      edgeToID <- which(metanetwork$nodes$name == link$to)
      groupFromID <- which(metanetwork$networkGroup$Var1 == metanetwork$nodes$network[edgeFromID])
      groupToID <- which(metanetwork$networkGroup$Var1 == metanetwork$nodes$network[edgeToID])

      if (metanetwork$nodes$network[edgeFromID] != metanetwork$nodes$network[edgeToID]) {
        linkPath <- rbind(metanetwork$nodes[edgeFromID, c('x','y')],
                          metanetwork$networkGroup[groupFromID, c('x','y')],
                          metanetwork$networkGroup[groupToID, c('x','y')],
                          metanetwork$nodes[edgeToID, c('x','y')])
      } else {
        linkPath <- rbind(metanetwork$nodes[edgeFromID, c('x','y')],
                          metanetwork$networkGroup[groupFromID, c('x','y')],
                          metanetwork$nodes[edgeToID, c('x','y')])
      }

      lines(xspline(linkPath$x, linkPath$y, shape = 1, draw=FALSE), col = cols[i], ...)
    }
  }

#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
colGroups <- function(metanetwork, colPal = pal_insileco) {
  # Group colors
    metanetwork$networkGroup$cols <- colPal[1:nrow(metanetwork$networkGroup)]

  # Node colors
    metanetwork$nodes$cols <- NA
    for(i in 1:nrow(metanetwork$networkGroup)) {
      metanetwork$nodes$cols[metanetwork$nodes$network == metanetwork$networkGroup$Var1[i]] <- metanetwork$networkGroup$cols[i]
    }

  metanetwork
}
#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#

# nodeSize <- function(metanetwork, freq = T) {
#     if (isTRUE(freq)) {
#       nLink <- as.data.frame(table(c(metanetwork$links$from, metanetwork$links$to)), stringsAsFactors = F)
#       colnames(nLink)[1L] <- 'name'
#       metanetwork$nodes <- dplyr::left_join(metanetwork$nodes, nLink, by = 'name')
#       metanetwork$nodes$cex <- (metanetwork$nodes$Freq / max(metanetwork$nodes$Freq))
#     } else {
#       metanetwork$nodes$cex <- .33
#     }
#
#     return(metanetwork)
# }


#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
linkCol <- function(metanetwork, type = 'all', focus = NULL, colLinks = '#876b40', colShadow = '#f4f4f4') {
  # metanetwork = list composed of 'nodes', 'links' and 'networkGroup'
  # type        = type of colors:
  #                 'all' = all links with single color = `colLinks`
  #                 'focus' = focus on the links of identified network
  # focus       = character, name of network(s) to focus on;
  #                 if length(focus) == 1, all links towards a single network
  #                 if length(focus) > 1, links focused on identified networks
  # colLinks    = color of links of `type` == 'all'
  # colShadow   = color of links that we are not focused on

  # Function
  if (type == 'all') {
    metanetwork$links$cols <- colLinks
  }

  if (type == 'focus' & length(focus) == 1) {
    # Box colors
    focusID <- metanetwork$networkGroup$Var1 %in% focus
    colBox <- metanetwork$networkGroup$cols
    metanetwork$networkGroup$cols[!focusID] <- colShadow
    metanetwork$networkGroup$colNames <- colBox
    metanetwork$networkGroup$colNames[focusID] <- colShadow

    # Link colors
    # metanetwork$links$cols <- paste0(colShadow, 88)
    metanetwork$links$cols <- colShadow
    linkCol <- data.frame(from = metanetwork$nodes$network[match(metanetwork$links$from,
                                                                 metanetwork$nodes$name)],
                          to = metanetwork$nodes$network[match(metanetwork$links$to,
                                                                metanetwork$nodes$name)],
                          stringsAsFactors = F)

    linkID <- linkCol$from %in% focus & linkCol$to %in% focus
    metanetwork$links$cols[linkID] <- metanetwork$networkGroup$cols[focusID] # "cannibalism"

    linkID <- (linkCol$from %in% focus | linkCol$to %in% focus) & !linkID
    cols <- paste0(linkCol$from[linkID], linkCol$to[linkID])
    cols <- gsub(focus, '', cols)
    cols <- match(cols, metanetwork$networkGroup$Var1)
    cols <- metanetwork$networkGroup$colNames[cols]
    metanetwork$links$cols[linkID] <- cols
  }

  if (type == 'focus' & length(focus) > 1) {
    # Box colors
    focusID <- metanetwork$networkGroup$Var1 %in% focus
    colBox <- metanetwork$networkGroup$cols
    metanetwork$networkGroup$cols[!focusID] <- colShadow
    metanetwork$networkGroup$colNames <- colBox
    metanetwork$networkGroup$colNames[focusID] <- colShadow

    # Link colors
    metanetwork$links$cols <- colShadow
    linkCol <- data.frame(from = metanetwork$nodes$network[match(metanetwork$links$from,
                                                                 metanetwork$nodes$name)],
                          to = metanetwork$nodes$network[match(metanetwork$links$to,
                                                                metanetwork$nodes$name)],
                          stringsAsFactors = F)

    linkID <- linkCol$from %in% focus & linkCol$to %in% focus
    metanetwork$links$cols[linkID] <- colLinks
  }

  # Add transparency
  metanetwork$links$cols <- paste0(metanetwork$links$cols, '22')

  metanetwork
}
#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#



#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
plotMetanetwork <- function(metanetwork,
                            rad1 = .925,
                            rad2 = 1,
                            sizeEdge = T,
                            colPal = pal_insileco,
                            type = 'all',
                            focus = NULL,
                            colLinks = '#876b40',
                            colShadow = '#f4f4f4',
                            shadowEdge = T,
                            cexSize = NULL
                          ) {

    # Metanetwork = list composed of 'nodes' and 'links'
    # rad1 = lower boundary for individual networks
    # rad2 = upper boundary for individual networks
    # colPal = color palette
    # colLinks = color for links

    # Function
    # Boundaries of individual networks
    metanetwork$networkGroup <- bound(metanetwork)

    # Node coordinates
      metanetwork <- nodePos(metanetwork, edgeRad = .875, groupRad = .5)

    # Colors
      metanetwork <- colGroups(metanetwork, colPal = colPal)

    # Node size
      # metanetwork <- nodeSize(metanetwork, freq = sizeEdge)
      metanetwork$nodes$cex <- cexSize

    # Link col
      metanetwork <- linkCol(metanetwork, type = type, focus = focus, colLinks = colLinks, colShadow = colShadow)

    # Plot
    par(mar = c(0,0,0,0))
    plot0()
    boxGroup(metanetwork,
             rad1 = rad1,
             colBox = metanetwork$networkGroup$cols,
             colNames = metanetwork$networkGroup$colNames,
             border = 'transparent')
    plotLinks(metanetwork,
              col = metanetwork$links$cols)
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
}
#=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=#
