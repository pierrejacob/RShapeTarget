\name{dist_point_to_poly}
\alias{dist_point_to_poly}
\title{Compute distance between a point and a polygon}
\usage{
  dist_point_to_poly(one_point, polygon)
}
\arguments{
  \item{one_point}{1x2 matrix representing one point.}

  \item{polygon}{nx2 matrix representing a polygon; it is
  assumed that the final point is equal to the first one,
  i.e. the first row of the matrix equals the last one.}
}
\value{
  A list with the distance between point and polygon and
  the corresponding point.
}
\description{
  Taken from
  \url{http://www.mathworks.com/matlabcentral/fileexchange/19398-distance-from-a-point-to-polygon/content/p_poly_dist.m}
}

