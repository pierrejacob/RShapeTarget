\name{dist_points_to_poly}
\alias{dist_points_to_poly}
\title{Compute distance between multiple points and a polygon}
\usage{
  dist_points_to_poly(points, polygon)
}
\arguments{
  \item{points}{mx2 matrix representing m points.}

  \item{polygon}{nx2 matrix representing a polygon; it is
  assumed that the final point is equal to the first one,
  i.e. the first row of the matrix equals the last one.}
}
\value{
  The vector of distances between the points and the
  polygon.
}
\description{
  Just calls \code{\link{dist_point_to_poly}} multiple
  times.
}

