d = faithful
d$cat = sample(c('A','A','A','A','B','B','C'), nrow(d), rep=TRUE)
d$size = round(10 * rlnorm(nrow(d)))

gridded = grid_points(d[,1:2], z=d[,c('size')], grp=d[,c('cat')], nx = 20, FUN = mean)

polys = grid_bars(gridded)

# visualise results

polys_lst = split(polys, f = polys$id)
plot(polys[,2:3], col=NA)
for(l in polys_lst) polygon(l[,2:3], col=c('red','green','blue')[l$grp], border=NA)

if(require(ggplot2)){
  ggplot(polys, aes(x, y, group = id, fill = grp)) + geom_polygon()
}


d = iris

gridded = grid_points(iris[,1:2], z=iris$Petal.Length, grp=iris$Species, FUN = length)

polys = grid_bars(gridded)
ggplot(polys, aes(x, y, group = id, fill = grp)) + geom_polygon()

d = data.frame(state.x77, lon = state.center[[1]], lat = state.center[[2]])

gridded = grid_points(d[,c('lon','lat')], z=d$Income, grp=rep(1,nrow(d)), FUN = sum)
polys = grid_bars(gridded)
ggplot(polys, aes(x, y, group = id, fill = grp)) + geom_polygon(col='white')



