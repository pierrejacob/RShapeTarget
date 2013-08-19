\name{get_box_polygon}
\alias{get_box_polygon}
\title{Get bounding box around polygons}
\usage{
  get_box_polygon(polygon, xfrac = 0.1, yfrac = 0.1)
}
\arguments{
  \item{polygon}{a list of list of matrices.}

  \item{xfrac}{From \code{splancs}'s doc: The fraction of
  the width of the point pattern by which the box will
  surround the point pattern to the left and right. Given
  to \code{\link{sbox}}.}

  \item{yfrac}{From \code{splancs}'s doc: The fraction of
  the height of the point pattern by which the box will
  surround the point pattern to the top and bottom. Given
  to \code{\link{sbox}}.}
}
\value{
  A polygon representing a square around the given polygon.
}
\description{
  Get bounding box around polygons
}

