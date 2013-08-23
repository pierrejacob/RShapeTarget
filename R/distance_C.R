#' @rdname dist_points_to_poly
#' @name dist_points_to_poly
#' @aliases dist_points_to_poly
#' @title Compute distance between multiple points and a polygon
#' @description
#' Taken from \url{http://www.mathworks.com/matlabcentral/fileexchange/19398-distance-from-a-point-to-polygon/content/p_poly_dist.m}
#' @param points mx2 matrix representing m points.
#' @param polygon nx2 matrix representing a polygon; 
#' it is assumed that the final point is equal to the first one, i.e. the first row of the matrix
#' equals the last one.
#' @param ABC precomputed quantities obtained by calling \code{\link{compute_ABC}}.
#' @return The vector of distances between the points and the polygon.
#' @export
dist_points_to_poly <- function(points, polygon, ABC){
  points <- matrix(points, ncol = 2)
  return(sapply(X=1:(dim(points)[1]), 
                FUN=function(x) dist_point_to_poly_C_(points[x,], polygon, ABC)))
}