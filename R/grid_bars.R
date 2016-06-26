#' Calculate polygons for a binned-categorical bar plot
#' @description Take gridded data with a categorical variable and calculate polygons for a binned categorical bar plot
#' @param x data.frame with first 2 columns as x/y axis points for gridded data, a 3rd categorical column, and a 4th column of values to aggregate
#' @param FUN Character string/vector of in-built R aggregation function(s) or a callback function compatible with aggregate()'s FUN argument, to aggregate data input as 'z'.  E.g. "min", "max", "mean", "median".  For frequency counts use "length" (which will also work if no input for 'z' is provided).  A string vector can be passed to apply multiple functions to apply seperately to vector 'z' or individually to the columns of a matrix/data.frame.
#' @param yscale Numeric, Multiplier for bar heights
#' @return A data.frame of polygon vertex coordinates and corresponding fields
#' @export
#' @example examples/grid_bars_examples.R
grid_bars = function(x, FUN = sum, yscale = 1){
  x_orig = x
  names(x) = c('x','y','grp','z')
  unique_x = unique(x$x)
  unique_y = unique(x$y)
  unit_x = unique_x[2] - unique_x[1]
  unit_y = unique_y[2] - unique_y[1]
  
  # identify groups in data lines
  frames = apply(unique(x[,1:2]), 1, function(i) paste(i[1],i[2]))
  x$f = match(paste(x[,1], x[,2]), frames)
  
  # combined cell data
  frame_heights = aggregate(z ~ f, x, FUN)
  max_value = max(frame_heights$z)
  yscaler = unit_y / max_value
  
  x = x[ order(x$f, x$grp), ] # to plot order
  x$x_prop = x$z /  frame_heights$z[ match(x$f, frame_heights$f) ]
  
  # bar widths
  x$x1 = x$x0 = NA
  for(i in frame_heights$f){
    f = x$f == i
    i0 = match(T,f)
    sel = i0:(i0 + length(f[f==T]) - 1)
    cumsums1 = cumsum(x$x_prop[sel])
    if(length(cumsums1)==1) cumsums0 = 0
    if(length(cumsums1)>1) cumsums0 = c(0, cumsums1[1:(length(cumsums1)-1)])
    x$x0[sel] = x$x[sel] + cumsums0 * unit_x
    x$x1[sel] = x$x[sel] + cumsums1 * unit_x
  }
  # bar heights
  amps = yscale * yscaler * frame_heights$z[match(x$f, frame_heights$f)] / 2
  x$y0 = x$y - amps
  x$y1 = x$y + amps
  
  # reformat as polygons
  do.call('rbind', lapply(1:nrow(x), function(i){
    r = x[i,]
    out = data.frame(id = i,
               x = c(r$x0, r$x0, r$x1, r$x1), 
               y = c(r$y0, r$y1, r$y1, r$y0), 
               grp = r$grp,
               z = r$z, stringsAsFactors = F)
    if('factor' %in% class(x$grp)) out$grp = factor(out$grp, levels = levels(x$grp))
    names(out) = c('id', names(x_orig))
    return(out)
  }))
}
