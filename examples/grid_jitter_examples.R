# 1. normal distribution
d = data.frame(x = rnorm(300), y = rnorm(300))
g = grid_jitter(d$x, d$y, nx=50, ny=50, tol=5, plotresults = FALSE, verbose = FALSE) # silent
g = grid_jitter(d$x, d$y, nx=50, ny=50, tol=5)

# 2. faithful dataset
g = grid_jitter(faithful$eruptions, faithful$waiting, nx=50, ny=50, tol=3)

# 3. US States
d = data.frame(lon = state.center$x, lat = state.center$y, state = state.abb, pop = state.x77[,1])
g = grid_jitter(d$lon, d$lat, nx=10, ny=8, tol=3, plotresults=FALSE, verbose = FALSE)
cbind(d, g) |> ggplot(aes(x, y, fill = pop, label = state)) + 
  geom_tile(col = 'white') + geom_text(col = 'white')

