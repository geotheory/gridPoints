#' Remove over-plotting by gridding point data intelligently
#' @description Data jittering reduces overplotting by adding small variances to values.  grid_jitter2 removes it entirely by first rounding points to a custom grid and then reallocating individual duplicate points to the nearest vacant cells within a maximum tolerance threshold.  If no vacant cells are available the function will abort.  This function applies Hungarian algorithm with small area constraints.  (By contrast grid_jitter applies Hungarian to match the entire points set to the whole grid.)  This function is much faster than grid_jitter with bigger point sets and grids.
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
#' @example examples/grid_jitter2_examples.R
grid_jitter2 = function(x, y, nx=50, ny=NULL, tol=5, plotresults=TRUE, file=NULL, w=20, h=20, verbose=TRUE){
  
  if(class(x) == 'integer') x = as.double(x)
  if(class(y) == 'integer') y = as.double(y)
  d0 = data.frame(x = x, y = y)
  if(is.null(ny)) ny = nx
  
  # grid coordinates
  grid_x = grid_vector(d0[, 1], nx-1)
  grid_y = grid_vector(d0[, 2], ny-1)
  
  # collate rescaled and rescaled/gridded coords
  dat = cbind(x0 = grid_x$rsc, y0 = grid_y$rsc, 
              x  = grid_x$rnd, y  = grid_y$rnd,
              id = 1:length(grid_x$rsc))
  
  # summarise and filter points that overplot
  sm = as.matrix(plyr::count(dat[, 3:4]))  # summarise gridded coords
  colnames(sm) = c('x','y','n')
  op = sm[sm[, 3] > 1, , drop=FALSE]      # filter overplots
  op = op[order(op[, 3], decreasing=TRUE), , drop=FALSE]   # rank by severity
  
  if(nrow(op) != 0){
    # iterate through list of duplicates
    for(i in 1:nrow(op)){
      message(i)
      p = op[i, 1:2, drop=FALSE]
      
      # list of vacant neighbouring cells
      v = as.matrix(expand.grid(x = (p[1]-tol):(p[1]+tol), y = (p[2]-tol):(p[2]+tol)))
      v = subset(v, fields::rdist(p, v)[1, ] <= tol)      # apply displacement limit tol
      # check which of v are already occupied
      fil = rowSums(outer(v[, 1], dat[, 3], "==") & outer(v[, 2], dat[, 4], "==")) == 0
      v = v[fil, , drop=FALSE]   # filter non-vacant cells
      v = rbind(p, v)            # re-append origin as 1st row
      
      # abort if insufficient local vacancies
      if(nrow(v) < op[i, 3]) return(message("Sorry, insufficient vacant neighbouring cells for point-cell reallocation.\nTry again with bigger grid (nx/ny) or higher displacement tolerance (tol)"))

      # subset data and allocate to neighbourhood using Hungarian algorithm
      dat_set = dat[dat[,3] == p[1] & dat[,4] == p[2], , drop=FALSE]
      dist_mx = fields::rdist(dat_set[, 1:2, drop=FALSE], v)
      soln = clue::solve_LSAP(dist_mx)
      stay = match(1, soln)      # index of point remaining in centroid cell
      dat[dat_set[-stay, 5], 3:4] = v[soln[-stay], 1:2]
    }
  }
  
  # report on displacements
  dists = sqrt((dat[,1] - dat[,3])^2 + (dat[,2] - dat[,4])^2)
  if(verbose){
    cat("point displacement summary in grid units (",nx," x ",ny,"):\n", sep='')
    print(summary(dists))
    cat('standard deviation:', round(sd(dists), 3), '\n')
    cat("grid unit lengths:   X-axis:", round(diff(range(d0[, 1]))/nx, 3), 
        "| Y-axis:", round(diff(range(d0[, 2]))/ny, 3), "\n")
  }
  
  # rescale to original 
  dat[,c(1,3)] = dat[,c(1,3)] * grid_x$scl
  dat[,c(2,4)] = dat[,c(2,4)] * grid_y$scl
  
  d1 = dat[, 3:4]
  dimnames(d1) = dimnames(d0)
  
  # plots to verify processes undertaken
  if(plotresults) plot_results(d0, d1, dists, file, w, h)
  
  return(as.data.frame(d1))
}
