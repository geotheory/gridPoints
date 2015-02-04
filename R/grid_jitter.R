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
  if(is.null(ny)) ny = nx
  col_names = row_names = NULL
  if(class(x) %in% c("data.frame","matrix")){
    col_names = colnames(x)
    if(is.null(col_names)) col_names = c('x','y')
    row_names = rownames(x)
    y = x[,2]
    x = x[,1]
  } else if(is.null(y)) return("Error: need y if x isn't a data.frame or matrix")
  
  # grid coordinates
  grid_x = grid_vector(x, nx-1)
  grid_y = grid_vector(y, ny-1)
  
  # collate rescaled and rescaled/gridded coords
  dat = cbind(x0 = grid_x$rsc, y0 = grid_y$rsc, 
              x  = grid_x$rnd, y  = grid_y$rnd,
              id = 1:length(grid_x$rsc))
  
  trackmovers = rep(FALSE, nrow(dat)) # TRUE for reallocated points
  
  # summarise and filter points that overplot
  sm = as.matrix(count(dat[,3:4]))                  # summarise gridded coords
  colnames(sm) = c('x','y','n')  
  op = sm[sm[,3] > 1, , drop=F]                     # filter overplots
  op = op[order(op[, 3], decreasing=T), , drop=F]   # rank by severit
  tsf = function(n) formatC(n, width=3, flag="0")
  
  # iterate through list of duplicates
  for(i in 1:nrow(op)){
    # p = c(op[i, 1], op[i, 2]) # current overplot coords
    p = op[i, 1:2, drop=F]
    
    # list of vacant neighbouring cells
    v = as.matrix(expand.grid(x = (p[1]-tol):(p[1]+tol), y = (p[2]-tol):(p[2]+tol)))
    v = subset(v, fields::rdist(p, v)[1,] < tol)      # displacement limit   
    fil = rowSums(outer(v[,1], dat[,3], "==") & outer(v[,2], dat[,4], "==")) == 0
    v = v[fil, , drop=F]  # filter non-vacant cells
    v = rbind(v, p)       # re-append origin
    
    # abort if insufficient local vacancies
    if(nrow(v) < op[i,3]) {
      cat("Sorry, insufficient vacant neighbouring cells for point-cell reallocation.\nTry again with bigger grid (nx/ny) or higher displacement tolerance (tol)")
      return()
    }
    
    # subset data and allocate to neighbourhood using Hungarian algorithm
    dat_set = dat[dat[,3] == p[1] & dat[,4] == p[2], ]
    dist_mx = fields::rdist(dat_set[, 1:2, drop=F], v)
    soln = clue::solve_LSAP(dist_mx)
    stay = match(nrow(v), soln) # point remaining in centroid cell
    trackmovers[dat_set[-stay, 5]] = TRUE
    for(j in 1:2) dat[dat_set[,5], j+2] = v[soln, j]
  }
  
  dists = sqrt((dat[,1] - dat[,3])^2 + (dat[,2] - dat[,4])^2)
  cat("point displacement summary in grid units (",nx," x ",ny,"):\n", sep='')
  print(summary(dists))
  cat('standard deviation:', round(sd(dists), 3), '\n')
  cat("grid unit lengths:   X-axis:", round((max(x) - min(x))/nx, 3), 
      "| Y-axis:", round((max(y) - min(y))/ny, 3), "\n")
  
  # rescale to original 
  dat[,c(1,3)] = dat[,c(1,3)] * grid_x$scl
  dat[,c(2,4)] = dat[,c(2,4)] * grid_y$scl
  
  # plots to verify processes undertaken
  if(plotresults){
    plotdat = data.frame(dat)
    displac = plotdat[!trackmovers, ]
    relocat = plotdat[trackmovers, ]
    
    thm = theme_bw()
    lab = labs(x = col_names[1], y = col_names[2])
    plot1 = ggplot(plotdat, aes(x0, y0)) + geom_point(size=1, alpha=.7) + labs(title='Original positions') + thm + lab
    plot2 = ggplot(plotdat, aes(x,  y )) + geom_point(size=1) + labs(title='Gridded positions') + thm + lab
    
    plot34 = ggplot() + geom_point(data=plotdat, aes(x0, y0), size=1, col='lightblue', shape=4) + 
      geom_point(data=plotdat, aes(x, y), size=1, col='pink', shape=15) + thm + lab
    
    plot3 = plot34 + labs(title='Point displacements to nearest cell') +
      geom_segment(data=displac, aes(x0, y0, xend=x, yend=y), col='red', alpha=.5, size=.4) +
      geom_point(data=displac, aes(x0, y0), col='blue', size=1, shape=4) +
      geom_point(data=displac, aes(x, y), size=1, col='red', alpha=.7, shape=15)
    
    plot4 = plot34 + labs(title='Points reallocated to new cells') +
      geom_segment(data=relocat, aes(x0, y0, xend=x, yend=y), col='red', alpha=.5, size=.4) +
      geom_point(data=relocat, aes(x0, y0), col='blue', size=1, shape=4) +
      geom_point(data=relocat, aes(x, y), size=1, col='red', alpha=.7, shape=15)
    
    if(!is.null(file)) pdf(filepath, width=w, height=h)
    gridExtra::grid.arrange(plot1, plot2, plot3, plot4, nrow=2, ncol = 2, main = NULL)
    if(!is.null(file)) dev.off()
  }
  
  final = dat[, 3:4]
  colnames(final) = col_names
  rownames(final) = row_names
  return(final)
}
