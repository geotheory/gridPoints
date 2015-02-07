# 1. normal distribution
d1 = data.frame(x = rnorm(500), y = rnorm(500))
d1g = grid_jitter(d1, nx=75, ny=75, tol=5)

# 2. faithful dataset
d2 = faithful
d2g = grid_jitter(d2, nx=50, ny=50, tol=3)

# 3. mpg example
d3 = ggplot2::mpg[,c(3,8)]
d3g = grid_jitter(d3, nx=100, ny=100, tol=3)

# 4. diamonds - carat vs price
d4 = ggplot2::diamonds[sample(1:53940, 500),c('carat','price')]
d4g = grid_jitter(d4, nx=300, ny=300, tol=2) # fails
d4g = grid_jitter(d4, nx=300, ny=300, tol=5, plotresults=F)

# 5. US States
d5 = data.frame(x=state.center$x, y=state.center$y, id=state.abb)
d5j = grid_jitter(d5[,1:2], nx=10, ny=8, tol=3, plotresults=T)
cols = sample(colorRampPalette(c('darkblue','blue','lightblue'))(50))
par(mai=rep(.6,4), mfrow=c(1,2))
plot(d5[,1:2], pch=15, cex=3, col=cols, asp=2.5, xlab=NA, ylab=NA)
text(d5[,1:2], labels=state.abb, col='white', cex=.7)
plot(d5j, pch=15, cex=3, col=cols, asp=2.5, xlab=NA, ylab=NA)
text(d5j, labels=d5$id, col='white', cex=.7)

# 6. to illuste cell reallocation 
d5 = data.frame(x = c(1,rep(50,500), 100), y = c(1,rep(50,500), 100))
d5g = grid_jitter(d5, nx=30, ny=30, tol=15)
