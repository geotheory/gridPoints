#' Remove over-plotting by gridding point data intelligently
#' @description Data jittering reduces overplotting by adding small variances to values.  grid_jitter2 removes it entirely by first rounding points to a custom grid and then reallocating individual duplicate points to the nearest vacant cell within a maximum tolerance threshold.  If no vacant cells are available the function will abort.  This function applies Hungarian algorithm with small area constraints.  By contrast grid_jitter applies Hungarian to match the entire points set to whole the grid.  This function is much faster than grid_jitter with bigger point sets and grids.
#' @param x Numeric vector or 2 column matrix or data.frame of data points to plot
#' @param y Numeric vector, y coordinates matching x (if a vector)
#' @param nx Numeric, values for the grid x dimension
#' @param ny Numeric, values for the grid x dimension
#' @param tol Numeric, the maximum distance overplotted points are allowed to move to the nearest vacant grid cell
#' @param plotresults Logical, output plots to illustrate point displacements and cell-reallocations
#' @param file String, if not NULL the result of plotresults will save to filename instead of rendering in R
#' @param w Numeric, dimensions (inches) of plotresults if output to file (PDF)
#' @param h Numeric, dimensions (inches) of plotresults if output to file (PDF)
#' @return A 2 column matrix of grid-jittered point coordinates
#' @export
#' @example examples/grid_jitter2_examples.R
grid_jitter2 = function(x, y=NULL, nx=50, ny=NULL, tol=5, plotresults=TRUE, file=NULL, w=10, h=10){
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
    } else return(message("'x' and 'y' input classes do not match up.\nUse either matrix/dataframe x or numeric x/y"))
  }
  if(is.null(ny)) ny = nx
  
  # grid coordinates
  grid_x = grid_vector(x, nx-1)
  grid_y = grid_vector(y, ny-1)
  
  # collate rescaled and rescaled/gridded coords
  dat = cbind(x0 = grid_x$rsc, y0 = grid_y$rsc, 
              x  = grid_x$rnd, y  = grid_y$rnd,
              id = 1:length(grid_x$rsc))
  
  trackmovers = rep(FALSE, nrow(dat)) # TRUE for reallocated points
  
  # summarise and filter points that overplot
  sm = as.matrix(count(dat[,3:4]))                         # summarise gridded coords
  colnames(sm) = c('x','y','n')  
  op = sm[sm[,3] > 1, , drop=FALSE]                        # filter overplots
  op = op[order(op[, 3], decreasing=TRUE), , drop=FALSE]   # rank by severit
  tsf = function(n) formatC(n, width=3, flag="0")
  
  if(nrow(op) != 0){
    # iterate through list of duplicates
    for(i in 1:nrow(op)){
      # p = c(op[i, 1], op[i, 2]) # current overplot coords
      p = op[i, 1:2, drop=FALSE]
      
      # list of vacant neighbouring cells
      v = as.matrix(expand.grid(x = (p[1]-tol):(p[1]+tol), y = (p[2]-tol):(p[2]+tol)))
      v = subset(v, fields::rdist(p, v)[1,] < tol)      # displacement limit   
      fil = rowSums(outer(v[,1], dat[,3], "==") & outer(v[,2], dat[,4], "==")) == 0
      v = v[fil, , drop=FALSE]   # filter non-vacant cells
      v = rbind(v, p)            # re-append origin
      
      # abort if insufficient local vacancies
      if(nrow(v) < op[i,3]) return(message("Sorry, insufficient vacant neighbouring cells for point-cell reallocation.\nTry again with bigger grid (nx/ny) or higher displacement tolerance (tol)"))

      # subset data and allocate to neighbourhood using Hungarian algorithm
      dat_set = dat[dat[,3] == p[1] & dat[,4] == p[2], ]
      dist_mx = fields::rdist(dat_set[, 1:2, drop=FALSE], v)
      soln = clue::solve_LSAP(dist_mx)
      stay = match(nrow(v), soln) # point remaining in centroid cell
      trackmovers[dat_set[-stay, 5]] = TRUE
      for(j in 1:2) dat[dat_set[,5], j+2] = v[soln, j]
    }
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
    
    plot4 = plot34 + labs(title='Points reallocated to new cells')
    if(nrow(relocat) != 0) plot4 = plot4 +
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
