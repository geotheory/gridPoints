d = faithful
d$cat = sample(c('A','A','A','A','B','B','C'), nrow(d), rep=TRUE)
d$size = round(10 * rlnorm(nrow(d)))

gridded = grid_points(d[,1:2], z=d[,c('size')], grp=d[,c('cat')], nx = 20, FUN = mean)

polys = grid_bars(gridded)

require(ggplot2)
ggplot(polys) + geom_polygon(aes(x, y, group = id, fill = grp))
