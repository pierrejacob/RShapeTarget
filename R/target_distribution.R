#'@export
create_target_from_word <- function(word, lambda){
  polygons <- extract_paths_from_word(word)
  bounding_box <- get_box(polygons)
  shape <- precomputed_shape_from_polygons(polygons)
  algo_parameters <- list(shape=shape, lambda=lambda)
  f <- function(x, l) log_word_density(x, algo_parameters)
  return(list(f=log_word_density, algo_parameters=algo_parameters,
              bounding_box=bounding_box))
}

#'@export
log_word_density <- function(x, algo_parameters){
  shape <- algo_parameters$shape
  lambda <- algo_parameters$lambda
  
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
  return(logpdf)
}