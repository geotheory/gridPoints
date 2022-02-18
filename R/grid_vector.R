grid_vector = function(vec, n, lims = NULL){
  if(is.null(lims)) lims = range(vec)
  dv = diff(lims)    # span of range
  scale_v = n / dv    # to scale data to grid size
  rescaled = vec * scale_v
  rescaled_rounded = round(rescaled)
  return(list(rnd = rescaled_rounded, rsc = rescaled, scl = 1/scale_v, dv=dv))
}
