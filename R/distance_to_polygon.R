#' @rdname dist_point_to_poly
#' @name dist_point_to_poly
#' @aliases dist_point_to_poly
#' @title Compute distance between a point and a polygon
#' @description
#' Taken from \url{http://www.mathworks.com/matlabcentral/fileexchange/19398-distance-from-a-point-to-polygon/content/p_poly_dist.m}
#' @param one_point 1x2 matrix representing one point.
#' @param polygon nx2 matrix representing a polygon; 
#' it is assumed that the final point is equal to the first one, i.e. the first row of the matrix
#' equals the last one.
#' @return A list with the distance between point and polygon and the corresponding point.
#' @export
dist_point_to_poly <- function(one_point, polygon){
  #   % linear parameters of segments that connect the vertices
  #   % Ax + By + C = 0
  x <- one_point[1]
  y <-  one_point[2]
  xv <- polygon[,1]
  yv <- polygon[,2]
  npoints <- dim(polygon)[1]
  A <- -diff(yv)
  B <-  diff(xv)
  C <- yv[2:npoints] * xv[1:(npoints-1)] - xv[2:npoints] * yv[1:(npoints-1)]
  #   % find the projection of point (x,y) on each rib
  AB <- 1/(A^2 + B^2)
  vv <- (A*x+B*y+C);
  xp <- x - (A*AB)*vv;
  yp <- y - (B*AB)*vv;
  # % Test for the case where a polygon rib is 
  # % either horizontal or vertical. From Eric Schmitz
  id <- which(B == 0)
  xp[id] <- xv[id]
  id <- which(A == 0)
  yp[id] <- yv[id]
  # % find all cases where projected point is inside the segment
  idx_x <- ((xp>=xv[1:(npoints-1)]) & (xp<=xv[2:npoints]) | ((xp>=xv[2:npoints]) & (xp<=xv[1:(npoints-1)])))
  idx_y <- ((yp>=yv[1:(npoints-1)]) & (yp<=yv[2:npoints]) | ((yp>=yv[2:npoints]) & (yp<=yv[1:(npoints-1)])))
  idx <- idx_x & idx_y
  # % distance from point (x,y) to the vertices
  dv <- sqrt((xv[1:(npoints-1)]-x)^2 + (yv[1:(npoints-1)]-y)^2)
  if(!(any(idx))) {
    #   all projections are outside of polygon ribs
    ind_min <- which.min(dv)
    x_poly <- xv[ind_min]
    y_poly <- yv[ind_min]
    d <- dv[ind_min]
  } else {
    #     % distance from point (x,y) to the projection on ribs
    dp <- sqrt((xp[idx]-x)^2 + (yp[idx]-y)^2)
    ind_min1 <- which.min(dv);
    ind_min2 <- which.min(dp);
    ind_min <- which.min(c(dv[ind_min1], dp[ind_min2]));
    if (ind_min == 1){
      # the closest point is one of the vertices
      x_poly <- xv[ind_min1]
      y_poly <- yv[ind_min1]
      d <- dv[ind_min1]
    } else {
      # the closest point is one of the projections
      x_poly <- xp[idx][ind_min2]
      y_poly <- yp[idx][ind_min2]
      d <- dp[ind_min2]
    }
  }
  return(list(point=matrix(c(x_poly, y_poly), ncol = 2), distance = d))
}

dist_point_to_poly_faster <- function(one_point, polygon, ABC){
  x <- one_point[1]
  y <-  one_point[2]
  xv <- polygon[,1]
  yv <- polygon[,2]
  npoints <- dim(polygon)[1]
  vv <- (ABC$A*x+ABC$B*y+ABC$C);
  xp <- x - (ABC$AAB)*vv;
  yp <- y - (ABC$BAB)*vv;
  # % Test for the case where a polygon rib is 
  # % either horizontal or vertical. From Eric Schmitz
  id <- which(ABC$B == 0)
  xp[id] <- xv[id]
  id <- which(ABC$A == 0)
  yp[id] <- yv[id]
  # % find all cases where projected point is inside the segment
  idx_x <- ((xp>=xv[1:(npoints-1)]) & (xp<=xv[2:npoints]) | ((xp>=xv[2:npoints]) & (xp<=xv[1:(npoints-1)])))
  idx_y <- ((yp>=yv[1:(npoints-1)]) & (yp<=yv[2:npoints]) | ((yp>=yv[2:npoints]) & (yp<=yv[1:(npoints-1)])))
  idx <- idx_x & idx_y
  # % distance from point (x,y) to the vertices
  dv <- sqrt((xv[1:(npoints-1)]-x)^2 + (yv[1:(npoints-1)]-y)^2)
  if(!(any(idx))) {
    #   all projections are outside of polygon ribs
    ind_min <- which.min(dv)
    x_poly <- xv[ind_min]
    y_poly <- yv[ind_min]
    d <- dv[ind_min]
  } else {
    #     % distance from point (x,y) to the projection on ribs
    dp <- sqrt((xp[idx]-x)^2 + (yp[idx]-y)^2)
    ind_min1 <- which.min(dv);
    ind_min2 <- which.min(dp);
    ind_min <- which.min(c(dv[ind_min1], dp[ind_min2]));
    if (ind_min == 1){
      # the closest point is one of the vertices
      x_poly <- xv[ind_min1]
      y_poly <- yv[ind_min1]
      d <- dv[ind_min1]
    } else {
      # the closest point is one of the projections
      x_poly <- xp[idx][ind_min2]
      y_poly <- yp[idx][ind_min2]
      d <- dp[ind_min2]
    }
  }
  return(d)
}

#' @rdname dist_points_to_poly
#' @name dist_points_to_poly
#' @aliases dist_points_to_poly
#' @title Compute distance between multiple points and a polygon
#' @description
#' Just calls \code{\link{dist_point_to_poly}} multiple times.
#' @param points mx2 matrix representing m points.
#' @param polygon nx2 matrix representing a polygon; 
#' it is assumed that the final point is equal to the first one, i.e. the first row of the matrix
#' equals the last one.
#' @return The vector of distances between the points and the polygon.
#' @export
dist_points_to_poly <- function(points, polygon){
  points <- matrix(points, ncol = 2)
  return(sapply(X=1:(dim(points)[1]), 
                FUN=function(x) dist_point_to_poly(one_point=points[x,], polygon=polygon)$distance))
}

dist_points_to_poly_faster <- function(points, polygon, ABC){
  points <- matrix(points, ncol = 2)
  return(sapply(X=1:(dim(points)[1]), 
                FUN=function(x) dist_point_to_poly_faster(one_point=points[x,], polygon=polygon, ABC=ABC)))
}
