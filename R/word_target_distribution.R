#' @rdname create_target_from_word
#' @name create_target_from_word
#' @aliases create_target_from_word
#' @title Create target density based on a word
#' @description
#' This function takes a word and a smoothness argument
#' and creates all the objects to compute a log target density function that looks like the letter.
#' The target density function is equal to 1 on the shape of the letters, and decreases to 0 far away
#' from the letters. The lambda parameter controls this decay. 
#' @param word a character vector made of  \code{letters} or \code{LETTERS}.
#' @param lambda smoothness parameter, default to 1.
#' @return A list containing the log target density 'logd', a list 'algo_parameters'
#' to be given to 'logd' at each call, and a bounding box 'bounding_box' giving the coordinates
#' of a square encompassing the word.
#' @export
create_target_from_word <- function(word, lambda=1){
  polygons <- extract_paths_from_word(word)
  bounding_box <- get_box_polygon(polygons)
  shape <- shape_from_polygons(polygons)
  algo_parameters <- list(shape=shape, lambda=lambda)
  f <- function(x, l) log_word_density(x, algo_parameters)
  return(list(logd=log_word_density, algo_parameters=algo_parameters,
              bounding_box=bounding_box))
}

#' @rdname log_word_density
#' @name log_word_density
#' @aliases log_word_density
#' @title Log target density function from word
#' @description
#' This function takes a point and a list of arguments and returns the value of the log density function.
#' @param x a mx2 matrix of points at which to evaluate the log density function.
#' @param algo_parameters a list containing 'lambda', the smoothness parameter, and 'shape', the shape
#' corresponding to a word and probably created using \code{\link{shape_from_polygons}}.
#' @return a vector of log density evaluations corresponding to the given points.
#' @export
log_word_density <- function(x, algo_parameters){
  shape <- algo_parameters$shape
  lambda <- algo_parameters$lambda
  logpdf <- rep(-Inf, dim(x)[1])
  # Stores the index of the outer polygon containing the given point
  # (equals 0 is outside all outer polygons)
  index_containing_polygon <- rep(0, dim(x)[1])
  # Stores the index of the inner polygon containing the given point
  # (equals 0 is outside all inner polygons)
  index_inner_polygon <- rep(0, dim(x)[1])
  # In the sequel it's important that the polygons don't intersect,
  # and that the inner polygons are disjoints; that's the case for each latin letter.
  # Note that's not the case for instance for the greek capital theta, where there's a polygon inside
  # a polygon inside yet another polygon.
  for (index in seq_along(shape$outer_polygons)){
    inside <- which(inout(x, shape$outer_polygons[[index]]))
    index_containing_polygon[inside] <- index
  }
  for (index in seq_along(shape$inner_polygons)){
    inside <- which(inout(x, shape$inner_polygons[[index]]))
    index_inner_polygon[inside] <- index
  }
  # Where are the points outside everything?
  indices_outside_points <- which(index_containing_polygon==0)
  # For those we can compute the distance to the outer polygons only.
  # We have to compute the distance to each outer polygon because we don't know yet
  # which one is closest.
  for (index in seq_along(shape$outer_polygons)){
    logpdf[indices_outside_points] <- pmax(logpdf[indices_outside_points],
                 (-1/(2*lambda)) *
                   dist_points_to_poly(x[indices_outside_points,], shape$outer_polygons[[index]],
                                              shape$outer_polygons_ABC[[index]]))
  }
  # For each inner polygon, if some points are inside it,
  # compute the distance between them and the polygon and fill the logpdf vector accordingly.
  for (index in seq_along(shape$inner_polygons)){
    if (length(which(index_inner_polygon == index))>0){
      logpdf[which(index_inner_polygon == index)] <- (-1/(2*lambda)) *
        dist_points_to_poly(x[which(index_inner_polygon == index),], shape$inner_polygons[[index]],
                                   shape$inner_polygons_ABC[[index]])
    }
  }
  # The rest are inside an outer polygon but not inside an inner polygon
  # so they must be on the surface.
  logpdf[is.infinite(logpdf)] <- 0
  return(logpdf)
}
