# 1. normal distribution
d = data.frame(x = rnorm(300), y = rnorm(300))
g = grid_jitter2(d$x, d$y, nx=50, ny=50, tol=5)

# 2. faithful dataset
g = grid_jitter2(faithful$eruptions, faithful$waiting, nx=50, ny=50, tol=3)

# 3. US States
d = data.frame(lon = state.center$x, lat = state.center$y, state = state.abb, pop = state.x77[,1])
g = grid_jitter2(d$lon, d$lat, nx=10, ny=8, tol=3, plotresults=FALSE, verbose = FALSE)
cbind(d, g) |> ggplot(aes(x, y, fill = pop, label = state)) + 
  geom_tile(col = 'white') + geom_text(col = 'white')

# map points example - large grid with high over-plotting should use grid_jitter2
if(require(dplyr)){
  set.seed(42)
  N = 500
  d = tibble(x0 = 100*runif(N), y0 = 100*runif(N), size = rlnorm(N, sdlog = 1.5)) |> ceiling() |> 
    group_by(1:N) |> slice(rep(1:n(), each = size)) |> ungroup() |> select(1:2) |> sample_n(500)
  g = grid_jitter2(d$x0, d$y0, nx = 100, ny = 70, tol = 10, plotresults = FALSE, verbose = FALSE)
  g |> ggplot(aes(x, y)) + geom_tile(col = 'white') + theme_void()
}
