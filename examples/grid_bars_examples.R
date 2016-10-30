if(require(dplyr)){
  d = diamonds %>% as_data_frame() %>% select(carat, price, x, color) %>% 
    mutate(carat = log10(carat), price = log10(price),
           color = c(rep('DE',2),rep('FG',2),rep('HIJ',3))[match(color, LETTERS[4:10])])
  
  gridded = grid_points(d[,1:2], grp=d$color, nx = 20)
  polys = grid_bars(gridded, yscale = 1)
  ggplot(polys) + geom_polygon(aes(carat, price, group = id, fill = grp)) +
    labs(x = 'log10(carat)', y = 'log10(price)', fill = 'colour')
}

