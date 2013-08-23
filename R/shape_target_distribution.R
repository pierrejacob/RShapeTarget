#' @rdname create_target_from_shape
#' @name create_target_from_shape
#' @aliases create_target_from_shape
#' @title Create target density based on a shape
#' @description
#' This function takes a SVG file and a smoothness argument
#' and creates all the objects to compute a log target density function that looks like the shape.
#' The lambda parameter controls this decay of the density when going away from the shape.
#' @param file_name path to a SVG file with "path" items in it.
#' @param lambda smoothness parameter, default to 1.
#' @param close_paths Should the path be closed automatically, i.e. last point = first point ? Default to TRUE.
#' @return A list containing the log target density 'logd', a list 'algo_parameters'
#' to be given to 'logd' at each call, and a bounding box 'bounding_box' giving the coordinates
#' of a square encompassing the shape.
#' @export
create_target_from_shape <- function(file_name, lambda=1, close_paths = TRUE){
  polygons <- extract_paths_from_svg(file_name, close_paths)
  bounding_box <- get_box_polygon(polygons)
  shape <- shape_from_polygons(polygons)
  algo_parameters <- list(shape=shape, lambda=lambda)
  f <- function(x, l) log_shape_density(x, algo_parameters)
  return(list(logd=log_shape_density, algo_parameters=algo_parameters,
              bounding_box=bounding_box))
}

#' @rdname log_shape_density
#' @name log_shape_density
#' @aliases log_shape_density
#' @title Log target density function from shape
#' @description
#' This function takes a point and a list of arguments and returns the value of the log density function.
#' @param x a mx2 matrix of points at which to evaluate the log density function.
#' @param algo_parameters a list containing 'lambda', the smoothness parameter, and 'shape', the shape
#' corresponding to a shape and probably created using \code{\link{shape_from_polygons}}.
#' @return a vector of log density evaluations corresponding to the given points.
#' @export
log_shape_density <- function(x, algo_parameters){
  shape <- algo_parameters$shape
  lambda <- algo_parameters$lambda
  logpdf <- rep(-Inf, dim(x)[1])
  
  for (index in seq_along(shape$outer_polygons)){
    logpdf <- pmax(logpdf, (-1/(2*lambda)) *
                     dist_points_to_poly(x, shape$outer_polygons[[index]],
                                         shape$outer_polygons_ABC[[index]]))
  }
  for (index in seq_along(shape$inner_polygons)){
    logpdf <- pmax(logpdf, (-1/(2*lambda)) *
                     dist_points_to_poly(x, shape$inner_polygons[[index]],
                                         shape$inner_polygons_ABC[[index]]))
  }
  return(logpdf)
}