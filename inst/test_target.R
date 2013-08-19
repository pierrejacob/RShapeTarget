rm(list = ls())

plot_polygon <- function(polygons){
  is_interior <- find_inner_polygons(polygons)
  l_outer <- list()
  l_inner <- list()
  for (index in which(is_interior)){
    l_inner[[length(l_inner)+1]] <- polygons[[index]]
  }
  for (index in which(!is_interior)){
    l_outer[[length(l_outer)+1]] <- polygons[[index]]
  }
  areas <- lapply(l_outer, areapl)
  areamax <- max(unlist(areas))
  l_outer <- lapply(l_outer, function(x) x %*% (diag(2)/areamax))
  l_inner <- lapply(l_inner, function(x) x %*% (diag(2)/areamax))
  p <- qplot(x=l_outer[[1]][,1], y = l_outer[[1]][,2], geom = "polygon")
  for (i in seq_along(l_inner)){
    p <- p + geom_polygon(aes_string(x = paste0('l_inner[[',i,']][,1]'), y = paste0('l_inner[[',i,']][,2]')), fill = "white")
  }
  p <- p + xlab("x") + ylab("y") + theme_bw()
  return(p)  
}
plot_polygon(extract_paths_from_letter("B"))
