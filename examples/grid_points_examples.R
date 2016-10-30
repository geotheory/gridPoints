# dataset
d = faithful
d$cat = sample(c('A','A','A','A','B','B','C'), nrow(d), rep=TRUE)
d$size = round(10 * rlnorm(nrow(d)))
d$temp = round(100 * rlnorm(nrow(d)))
d$tempcat = c('cool','hot')[cut(d$temp, quantile(d$temp,c(0,.5,1)))]
if(require(dplyr)) d = d %>% as_data_frame()
head(d)

# point counts
d_ag = grid_points(d, nx = 20, FUN=length)
plot(d[,1:2], pch=16, cex=.3, col='red')
symbols(d_ag[,1:2], squares=sqrt(d_ag$n), inches=.3, add=TRUE)

# aggregating data to a spatial grid
grid_points(d[,1:2], z=d$temp, nx = 5, FUN=mean)
grid_points(d[,1:2], z=d$temp, nx = 5, FUN=c('min','max','median','length'))
grid_points(d[,1:2], z=d[,c('size','temp')], nx = 5, FUN = mean)
grid_points(d[,1:2], z=d[,c('size','temp')], nx = 5, FUN = c('mean','median'))
grid_points(d[,1:2], z=d[,c('size','temp')], nx = 5, FUN = c('mean','median','sum')) # error

# aggregating data to a spatial grid plus additional grouping variable(s)
grid_points(d[,1:2], grp=d[,'cat'], nx = 5)
grid_points(d[,1:2], grp=d[,c('cat','tempcat')], nx = 5)
grid_points(d[,1:2], z=d[,'size'], grp=d[,c('cat','tempcat')], nx = 5, FUN = mean)
grid_points(d[,1:2], z=d[,c('size','temp')], grp=d[,c('cat','tempcat')], nx = 5, FUN = mean)
grid_points(d[,1:2], z=d[,c('size','temp')], grp=d[,c('cat','tempcat')], nx = 5, FUN = c('median','mean'))

# most prevalent sub-category in each grid cell
topcase = function(v){
  counts = plyr::count(v)
  counts = counts[order(counts$freq, decreasing=TRUE),]
  counts[1,1]
}
d_ag = grid_points(d[,1:2], nx = 10, z=d$cat, FUN=topcase)
plot(d[,1:2], pch='.')
text(d_ag[,1:2], labels=d_ag$z, cex=.7, col='red')
