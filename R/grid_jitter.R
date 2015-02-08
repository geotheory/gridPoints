#' Remove over-plotting by gridding point data intelligently
#' @description Data jittering reduces overplotting by adding small variances to values. gridJitter removes it entirely by fitting points to a custom grid and then reallocating duplicate points to the nearest vacant cell within a maximum tolerance threshold.  If no vacant cells are available the function will abort.
#' @param x Numeric vector or 2 column matrix or data.frame of data points to plot
#' @param y Numeric vector, y coordinates matching x (if a vector)
#' @param nx/ny Numeric, values for the grid dimensions
#' @param tol Numeric, the maximum distance overplotted points are allowed to move to the nearest vacant grid cell
#' @param plotresults Logical, output plots to illustrate point displacements and cell-reallocations
#' @param file String, if not NULL the result of plotresults will save to filename instead of rendering in R
#' @param w/h Numeric, dimensions (inches) of plotresults if output to file (PDF)
#' @return A 2 column matrix of grid-jittered point coordinates
#' @export
#' @example examples/grid_jitter_examples.R
grid_jitter = function(x, y=NULL, nx=50, ny=NULL, tol=5, plotresults=TRUE, file=NULL, w=10, h=10){
  suppressMessages(require(clue))
  suppressMessages(require(fields))
  suppressMessages(require(plyr))
  if(plotresults) suppressMessages(library(ggplot2))
  if(class(x) == class(y) & class(x) == "numeric"){
    d0 = cbind(x, y)
    colnames(d0) = c('x','y')
  } else{
    if(class(x) %in% c("data.frame","matrix")){
      d0 = x
    } else return("'x' and 'y' input classes do not match up.\nUse either matrix/dataframe x or numeric x/y")
  }
  if(is.null(ny)) ny = nx

  # auto grid
  xran = range(d0[,1]);         yran = range(d0[,2])
  xunit = diff(xran) / nx; yunit = diff(yran) / ny
  xseq = seq(xran[1]-xunit*tol, xran[2]+xunit*tol, length=nx+tol*2)
  yseq = seq(yran[1]-yunit*tol, yran[2]+yunit*tol, length=ny+tol*2)
  grd = as.matrix(expand.grid(x = xseq, y = yseq))
  grd_u = cbind(grd[,1]/xunit, grd[,2]/yunit)
  
  # use Hungarian algorithm to fit points to grid optimally
  u0 = cbind(d0[,1]/xunit, d0[,2]/yunit)
  dist.mat = rdist(u0, grd_u)
  dm_temp = dist.mat
  dm_temp[dm_temp > tol] = NA
  drop.rows = colSums(is.na(dm_temp)) < nrow(dm_temp)
  if(ncol(dist.mat) < nrow(dist.mat)) return("Insufficient grid to accomodate points\nTry again with larger grid (nx/ny) or tol")
  dist.mat = dist.mat[, drop.rows]
  grd_u = grd_u[drop.rows, ]
  grd = grd[drop.rows, ]
  
  hungarian.solution = solve_LSAP(dist.mat)
  d1 = grd[hungarian.solution, ]
  
  # report on displacements
  dists = sqrt(((d0[,1] - d1[,1])/xunit)^2 + ((d0[,2] - d1[,2])/yunit)^2)
  cat("point displacement summary in grid units (",nx," x ",ny,"):\n", sep='')
  print(summary(dists))
  cat('standard deviation:', round(sd(dists), 3), '\n')
  cat("grid unit lengths:   X-axis:", round((max(d0[,1]) - min(d0[,1]))/nx, 3), 
      "| Y-axis:", round((max(d0[,2]) - min(d0[,2]))/ny, 3), "\n")
  
  # plots to verify processes undertaken
  if(plotresults) plot_results(d0, d1, dists, file, filepath)
  
  dimnames(d1) = dimnames(d0)
  return(d1)
}
