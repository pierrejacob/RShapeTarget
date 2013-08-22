# remove all objects
rm(list = ls())
# try to detach the package if it was already loaded
try(detach(package:RShapeTarget, unload = TRUE), silent = TRUE)
# load the package
library(PAWL)
library(RShapeTarget)

word <- create_target_from_word(word="G", lambda=0.5)
# starting points for MCMC algorithms
rinit <- function(size)  csr(word$bounding_box, size)

x <- rinit(100)
polygon <- word$algo_parameters$shape$outer_polygons[[1]]
abc <- word$algo_parameters$shape$outer_polygons_ABC[[1]]
dist_point_to_poly_old <- function(one_point, polygon){
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

dist_points_to_poly_old <- function(points, polygon){
  points <- matrix(points, ncol = 2)
  return(sapply(X=1:(dim(points)[1]), 
                FUN=function(x) dist_point_to_poly_old(one_point=points[x,], polygon=polygon)$distance))
}

all(dist_points_to_poly(x, polygon, abc) == dist_points_to_poly_old(x, polygon))
library(rbenchmark)
tests = list(C=expression(dist_points_to_poly(x, polygon, abc)), 
             Rold=expression(dist_points_to_poly_old(points=x, polygon=polygon)))
do.call(benchmark,
        c(tests, list(replications=100,
                      columns=c('test', 'elapsed', 'replications', 'relative'),
                      order='elapsed')))

