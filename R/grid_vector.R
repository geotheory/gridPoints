grid_vector = function(vec, n){
  ran_v = range(vec)
  dv = diff(ran_v)    # span of range
  scale_v = n / dv    # to scale data to grid size
  rescaled = vec * scale_v
  rescaled_rounded = round(rescaled)
  return(list(rnd = rescaled_rounded, rsc = rescaled, scl = 1/scale_v, dv=dv))
}
