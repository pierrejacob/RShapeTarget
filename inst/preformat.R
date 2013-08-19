# Format polygons 

# First we have polygons coming from letters, that can be nested (e.g. "B" has two inner polygons)
# or non-nested (e.g. "i" has disjoint polygons).

# Then we want to build words, which are disjoint letters, e.g. disjoints groups of polygons.

polygons_from_W <- extract_paths_from_letter("W")
polygons_from_B <- extract_paths_from_letter("B")
polygons_from_i <- extract_paths_from_letter("i")
shape <- precomputed_shape_from_polygons(polygons_from_i)

box <- shape$outer_polygons[[1]]
if (length(shape$outer_polygons)>1){
  for (index in 2:length(shape$outer_polygons)){
    box <- rbind(box, shape$outer_polygons[[index]])
  }
}
square <- sbox(box, xfrac=1.0,yfrac=0.1)
x <- csr(square, 25000)
lambda <- 0.1
logpdf <- rep(-Inf, dim(x)[1])
index_containing_polygon <- rep(0, dim(x)[1])
index_inner_polygon <- rep(0, dim(x)[1])
for (index in seq_along(shape$outer_polygons)){
  inside <- which(inout(x, shape$outer_polygons[[index]]))
  index_containing_polygon[inside] <- index
}
for (index in seq_along(shape$inner_polygons)){
  inside <- which(inout(x, shape$inner_polygons[[index]]))
  index_inner_polygon[inside] <- index
}

indices_outside_points <- which(index_containing_polygon==0)
indices_inside_points <- which(index_containing_polygon!=0)
# points outside any outer polygon
for (index in seq_along(shape$outer_polygons)){
  logpdf[indices_outside_points] <- pmax(logpdf[indices_outside_points],
                                         (-1/(2*lambda)) *
    dist_to_poly(x[indices_outside_points,], shape$outer_polygons[[index]]))
}
# points inside any inner polygon
for (index in seq_along(shape$inner_polygons)){
  if (length(which(index_inner_polygon == index))>0){
    logpdf[which(index_inner_polygon == index)] <- (-1/(2*lambda)) *
             dist_to_poly(x[which(index_inner_polygon == index),], shape$inner_polygons[[index]])
  }
}
# the rest are inside an outer polygon but not inside an inner polygon
# so it's on the surface
logpdf[is.infinite(logpdf)] <- 0

df <- data.frame(cbind(x[,1], x[,2], logpdf))
names(df) <- c("x", "y", "logd")
ggplot(df, aes(x = x, y = y, colour = logd, fill = logd, weight = logd)) + geom_point()

