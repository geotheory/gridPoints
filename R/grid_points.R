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
grid_points = function (x, y = NULL, z = NULL, grp = NULL, nx = 50, ny = NULL, FUN = length){
  x_orig = x
  znames = colnames(z)
  if (is.null(ny)) 
    ny = nx
  col_names = row_names = NULL
  classx = class(x)[1]
  if (classx %in% c("data.frame", "matrix", "tbl_df")) {
    col_names = colnames(x)
    if (is.null(col_names)) 
      col_names = c("x", "y")
    row_names = rownames(x)
    y = x[, 2]
    x = x[, 1]
  }
  else if (is.null(y)) {
    return(message("Error: need y if x isn't a data.frame or matrix\n"))
  }
  grid_x = grid_vector(x, nx - 1)
  grid_y = grid_vector(y, ny - 1)
  dat = cbind(x = grid_x$rnd, y = grid_y$rnd)
  agg_dat = data.frame(dat)
  if (!is.null(grp)) 
    agg_dat = cbind(agg_dat, grp)
  if (length(FUN) > 1) {
    if (!is.null(ncol(z))) {
      if (ncol(z) != length(FUN)) {
        return(message("If length(FUN)>1, 'z' must be a vector or matrix of matching width\n"))
      }
    }
    else {
      z = data.frame(replicate(length(FUN), z))
      names(z) = FUN
    }
    results = list()
    for (i in 1:length(FUN)) {
      results[[i]] = aggregate(z[[i]], by = as.list(agg_dat), FUN = FUN[i])
      names(results[[i]])[length(names(results[[i]]))] = names(z)[i]
    }
    output = results[[1]]
    for (i in 2:length(results)) output = merge(output, results[[i]])
  }
  else {
    if (is.null(z)) {
      if (!identical(FUN, length) & !identical(FUN, "length")) 
        return("FUN must be 'length' if 'z' is not provided")
      output = plyr::count(agg_dat)
      colnames(output)[ncol(output)] = "count"
    }
    else {
      output = aggregate(z, by = as.list(agg_dat), FUN = FUN)
      if (is.null(ncol(z))) 
        names(output)[ncol(output)] = "val"
    }
  }
  output[[1]] = output[[1]] * grid_x$scl
  output[[2]] = output[[2]] * grid_y$scl
  if ("tbl_df" %in% class(x_orig)) 
    output = as_data_frame(output)
  return(output)
}
