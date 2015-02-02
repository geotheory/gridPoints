#' Aggregate point data to a grid
#' @description Aggregate points to a custom grid
#' @param x Numeric vector or 2 column matrix or data.frame of data points to plot
#' @param y Numeric vector, y coordinates matching x (if a vector)
#' @param z Numeric vector or multi-column matrix or data.frame of variables to be aggregated
#' @param nx/ny Numeric, values for the grid dimensions
#' @param grp Vector, matrix or dataframe of additional variables to aggregate by
#' @param FUN Character string/vector of in-built R aggregation function(s) or a callback function compatible with aggregate()'s FUN argument, to aggregate data input as 'z'.  E.g. "min", "max", "mean", "median".  For frequency counts use "length" (which will also work if no input for 'z' is provided).  A string vector can be passed to apply multiple functions to apply seperately to vector 'z' or individually to the columns of a matrix/data.frame.
#' @return A data.frame of grid-jittered point coordinates with point counts and/or data aggregated accordinaly
#' @export
#' @example examples/grid_points_examples.R
grid_points = function(x, y=NULL, z=NULL, grp=NULL, nx=50, ny=NULL, FUN=length){
  znames = colnames(z)
  if(is.null(ny)) ny = nx
  col_names = row_names = NULL
  if(class(x) %in% c("data.frame","matrix")){
    col_names = colnames(x)
    if(is.null(col_names)) col_names = c('x','y')
    row_names = rownames(x)
    y = x[,2]
    x = x[,1]
  } else if(is.null(y)){
    cat("error: need y if x isn't a data.frame or matrix\n")
    return()
  }
  
  # grid coordinates
  grid_x = grid_vector(x, nx-1)
  grid_y = grid_vector(y, ny-1)
  
  # collate rescaled and rescaled/gridded coords
  dat = cbind(x  = grid_x$rnd, y  = grid_y$rnd)
  
  # aggregation factors
  agg_dat = data.frame(dat)
  if(!is.null(grp)) agg_dat = cbind(agg_dat, grp)
  agg_dat = as.list(agg_dat)
  
  if(length(FUN) > 1){
    # multiple aggregate functions provided
    if(!is.null(ncol(z))){
      # z has columns so is a matrix or data.frame
      if(ncol(z) != length(FUN)) {
        cat("If length(FUN)>1, 'z' must be a vector or matrix of matching width\n")
        return()
      }
    } else{
      # z is single vector so duplicate it to apply to multiple functions
      z = data.frame(replicate(length(FUN), z))
      names(z) = FUN
    }
    results = list()
    for(i in 1:length(FUN)){
      results[[i]] = aggregate(z[[i]], by = agg_dat, FUN=FUN[i])
      names(results[[i]])[length(names(results[[i]]))] = names(z)[i]
    }
    output = results[[1]]
    for(i in 2:length(results)) output = merge(output, results[[i]])
  } else{
    # only 1 aggregate function provided
    if(is.null(z)) {
      # z not provided so only aggregation possible is point count
      if(!identical(FUN, length) & !identical(FUN, 'length')) return("FUN must be 'length' if 'z' is not provided" )
      output = plyr::count(dat[,1:2])  # summarise gridded coords
      colnames(output) = c('x','y','n')
    } else{
      # single aggregation function
      output = aggregate(z, by = agg_dat, FUN=FUN)
      if(is.null(ncol(z))) names(output)[ncol(output)] = 'z' # column name for vector z
    }
  }
  output[[1]] = output[[1]] * grid_x$scl
  output[[2]] = output[[2]] * grid_y$scl
  return(output)
}
