plot_results = function(dat0, dat1, dists, file, filepath){
  dat0 = data.frame(dat0)
  dat1 = data.frame(dat1)
  names(dat0) = c('x','y')
  names(dat1) = c('x1','y1')
  thm = theme_bw()
  col_names = colnames(dat0)
  lab = labs(x = col_names[1], y = col_names[2])
  
  plot1 = ggplot(dat0, aes(x, y)) + geom_point(alpha=.7) + labs(title='Original positions') + thm + lab
  plot2 = ggplot(dat1, aes(x1, y1)) + geom_point(shape=15) + labs(title='Gridded positions') + thm + lab
  
  mids = data.frame(xm = apply(cbind(dat0[,1], dat1[,1]), 1, mean), ym = apply(cbind(dat0[,2], dat1[,2]), 1, mean))
  dat = cbind(dat0, mids, dat1)
  
  plot3 = ggplot(dat) + geom_point(aes(x, y), col='blue', shape=4) +
    geom_point(aes(x1, y1), col='red', alpha=.7, shape=15) +
    geom_segment(aes(x, y, xend=xm, yend=ym), col='red', alpha=.5, arrow=arrow(length = unit(0.2,"cm"))) +
    geom_segment(aes(xm, ym, xend=x1, yend=y1), col='red', alpha=.5) +
    thm + lab + labs(title='Point displacements')
  
  plot4 = ggplot(data.frame(d=dists)) + geom_density(aes(x=d), fill='grey') + thm +
    labs(title='Displacement summary', x = "distance (grid units)")
  
  if(!is.null(file)) pdf(filepath, width=w, height=h)
  gridExtra::grid.arrange(plot1, plot2, plot3, plot4, nrow=2, ncol = 2, main = NULL)
  if(!is.null(file)) dev.off()
}
