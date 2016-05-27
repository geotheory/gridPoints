\name{gridPoints-package}
\alias{gridPoints-package}
\alias{gridPoints}
\docType{package}
\title{
Functions to grid point data
}
\description{
Package offers 2 functions to grid point data:  grid_points() is a flexible data tool for aggregating point data to a grid.  grid_jitter() deterministically adjusts point positions to improve perceptual properties when visualised and to prevent over-plotting.
}
\details{
\tabular{ll}{
Package: \tab gridPoints\cr
Type: \tab Package\cr
Version: \tab 0.2\cr
Date: \tab 2015-02-02\cr
License: \tab GPL-2\cr
}
See individual function pages for self-explanatory examples ("?grid_points" and "?grid_jitter")
}
\author{
Robin Edwards

Maintainer: Robin Edwards <geotheory1@gmail.com>
~~ The author and/or maintainer of the package ~~
}
\references{
~~ Literature or other references for background information ~~
}
\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
# most basic examples
d = faithful

# grid_points function
grd1 = grid_points(d, nx = 20, FUN=length)
plot(d[,1:2], pch=16, cex=.3, col='red')
symbols(grd1[,1:2], squares=sqrt(grd1$n), inches=.3, add=T)

# grid_jitter function
grd2 = grid_jitter(d, nx=75, ny=75, tol=5)
head(grd1)
}
