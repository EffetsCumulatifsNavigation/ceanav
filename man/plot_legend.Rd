% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fnc_legend.R
\name{plot_legend_cont}
\alias{plot_legend_cont}
\alias{plot_legend_bin}
\title{Plot legend}
\usage{
plot_legend_cont(
  range = c(0, 1),
  pal = NULL,
  cexMain = 1,
  cexSub = 0.75,
  minUp = 0.175,
  mainTitle = NULL,
  subTitle = NULL,
  n = 5,
  showNA = FALSE
)

plot_legend_bin(
  col,
  cexMain = 1,
  cexSub = 0.75,
  minUp = 0.175,
  mainTitle = NULL,
  subTitle = NULL,
  showNA = FALSE
)
}
\arguments{
\item{range}{numeric, vector with minimal and maximal values}

\item{pal}{character, vector of colors, or color palette}

\item{minUp}{numeric, minimum upper side to write as a function of bbox extent}

\item{mainTitle}{character, type of legend. Choices are 'continuous', 'binary' or 'categorical'}

\item{subTitle}{character, type of legend. Choices are 'continuous', 'binary' or 'categorical'}

\item{showNA}{logical, add box that shows regions identified as NA in the figure}

\item{cex.text}{numeric, cex for legend text}

\item{type}{character, type of legend. Choices are 'continuous', 'binary' or 'categorical'}

\item{nTick}{numeric, number of ticks in the legend}
}
\value{
Opens a graphical interface with the plot
}
\description{
Function to create a legend
}
\keyword{legend}
\keyword{plot,}
