% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grid_jitter.R
\name{grid_jitter}
\alias{grid_jitter}
\title{Remove over-plotting by gridding point data intelligently}
\usage{
grid_jitter(x, y = NULL, nx = 50, ny = NULL, tol = 5,
  plotresults = TRUE, file = NULL, w = 20, h = 20)
}
\arguments{
\item{x}{Numeric vector or 2 column matrix or data.frame of data points to plot}

\item{y}{Numeric vector, y coordinates matching x (if a vector)}

\item{nx}{Numeric, grid x/y dimensions}

\item{ny}{Numeric}

\item{tol}{Numeric, the maximum distance overplotted points are allowed to move to the nearest vacant grid cell}

\item{plotresults}{Logical, output plots to illustrate point displacements and cell-reallocations}

\item{file}{String, if not NULL the result of plotresults will save to filename instead of rendering in R}

\item{w}{Numeric, width/height (inches) of plotresults if output to file (PDF)}

\item{h}{Numeric}
}
\value{
A 2 column matrix of grid-jittered point coordinates
}
\description{
Data jittering reduces overplotting by adding small variances to values.  grid_jitter removes it entirely by fitting points to a custom grid.  This function applies Hungarian algorithm to match entire points set to the whole grid.  (By contrast grid_jitter2 applies Hungarian with small area constraints.)  This function works well for smaller grids and point sets, but will slow considerably with larger sets.
}
\examples{
# 1. normal distribution
d1 = data.frame(x = rnorm(300), y = rnorm(300))
d1g = grid_jitter(d1, nx=50, ny=50, tol=5)

# 2. faithful dataset
d2 = faithful
d2g = grid_jitter(d2, nx=50, ny=50, tol=3)

# 3. mpg example
d3 = ggplot2::mpg[,c(3,8)]
d3g = grid_jitter(d3, nx=100, ny=100, tol=3)

# 4. US States
d4 = data.frame(x=state.center$x, y=state.center$y, id=state.abb)
d4j = grid_jitter(d4[,1:2], nx=10, ny=8, tol=3, plotresults=TRUE)
cols = sample(colorRampPalette(c('darkblue','blue','lightblue'))(50))
par(mai=rep(.6,4), mfrow=c(1,2))
plot(d4[,1:2], pch=15, cex=2.5, col=cols, asp=2.5, xlab=NA, ylab=NA)
text(d4[,1:2], labels=state.abb, col='white', cex=.7)
plot(d4j, pch=15, cex=2.5, col=cols, asp=2.5, xlab=NA, ylab=NA)
text(d4j, labels=d4$id, col='white', cex=.7)

# 5. to illuste cell reallocation 
d5 = data.frame(x = c(1,rep(50, 398), 100), y = c(1,rep(50, 398), 100))
d5g = grid_jitter(d5, nx=30, ny=30, tol=15)
}

