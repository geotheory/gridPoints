#' Remove over-plotting by gridding point data intelligently
#' @description Data jittering reduces overplotting by adding small variances to values.  grid_jitter removes it entirely by fitting points to a custom grid.  This function applies Hungarian algorithm to match entire points set to the whole grid.  (By contrast grid_jitter2 applies Hungarian with small area constraints.)  This function works well for smaller grids and point sets, but will slow considerably with larger sets.
#' @param x Numeric vector of x-axis values
#' @param y Numeric vector of y-axis values
#' @param nx Numeric, grid x/y dimensions
#' @param ny Numeric
#' @param tol Numeric, the maximum distance overplotted points are allowed to move to the nearest vacant grid cell
#' @param plotresults Logical, output plots to illustrate point displacements and cell-reallocations
#' @param file String, if not NULL the result of plotresults will save to filename instead of rendering in R
#' @param w Numeric, width/height (inches) of plotresults if output to file (PDF)
#' @param h Numeric
#' @param verbose Logical, print displacement statistics
#' @return A 2 column matrix of grid-jittered point coordinates
#' @export
#' @example examples/grid_jitter_examples.R
grid_jitter = function(x, y, nx=50, ny=NULL, tol=5, plotresults=TRUE, file=NULL, w=20, h=20, verbose=TRUE){
  
  if(class(x) == 'integer') x = as.double(x)
  if(class(y) == 'integer') y = as.double(y)
  d0 = data.frame(x = x, y = y)
  if(is.null(ny)) ny = nx

  # auto grid
  xran = range(d0[,1]);       yran = range(d0[,2])
  xunit = diff(xran) / nx;    yunit = diff(yran) / ny
  xseq = seq(xran[1]-xunit*tol, xran[2]+xunit*tol, length=nx+tol*2)
  yseq = seq(yran[1]-yunit*tol, yran[2]+yunit*tol, length=ny+tol*2)
  grd = as.matrix(expand.grid(x = xseq, y = yseq))
  grd_u = cbind(grd[,1]/xunit, grd[,2]/yunit)
  
  # use Hungarian algorithm to fit points to grid optimally
  # first calculate distance matrix (grid units) and filter to tolerance
  u0 = cbind(d0[,1]/xunit, d0[,2]/yunit)
  dist.mat = fields::rdist(u0, grd_u)
  dm_temp = dist.mat
  dm_temp[dm_temp > tol] = NA
  drop.rows = colSums(is.na(dm_temp)) < nrow(dm_temp)
  dist.mat = dist.mat[, drop.rows]
  if(ncol(dist.mat) < nrow(dist.mat)) return(message("Insufficient grid to accomodate points\nTry again with larger grid (nx/ny) or tol\n"))
  # grd_u = grd_u[drop.rows, ]
  grd = grd[drop.rows, ]
  
  hungarian.solution = clue::solve_LSAP(dist.mat)
  d1 = grd[hungarian.solution, ]
  
  # report on displacements
  dists = sqrt(((d0[,1] - d1[,1])/xunit)^2 + ((d0[,2] - d1[,2])/yunit)^2)
  if(verbose){
    cat("point displacement summary in grid units (",nx," x ",ny,"):\n", sep='')
    print(summary(dists))
    cat('standard deviation:', round(sd(dists), 3), '\n')
    cat("grid unit lengths:   X-axis:", round((max(d0[,1]) - min(d0[,1]))/nx, 3), 
        "| Y-axis:", round((max(d0[,2]) - min(d0[,2]))/ny, 3), "\n")
  }
  
  # plots to verify processes undertaken
  if(plotresults) plot_results(d0, d1, dists, file, w, h)
  
  dimnames(d1) = dimnames(d0)
  return(as.data.frame(d1))
}
