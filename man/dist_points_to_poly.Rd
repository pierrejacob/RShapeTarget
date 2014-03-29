\name{dist_points_to_poly}
\alias{dist_points_to_poly}
\title{Compute distance between multiple points and a polygon}
\usage{
dist_points_to_poly(points, polygon, ABC)
}
\arguments{
  \item{points}{mx2 matrix representing m points.}

  \item{polygon}{nx2 matrix representing a polygon; it is
  assumed that the final point is equal to the first one,
  i.e. the first row of the matrix equals the last one.}

  \item{ABC}{precomputed quantities obtained by calling
  \code{\link{compute_ABC}}.}
}
\value{
The vector of distances between the points and the polygon.
}
\description{
Taken from
\url{http://www.mathworks.com/matlabcentral/fileexchange/19398-distance-from-a-point-to-polygon/content/p_poly_dist.m}
}

