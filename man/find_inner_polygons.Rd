\name{find_inner_polygons}
\alias{find_inner_polygons}
\title{Find which polygons are within another}
\usage{
  find_inner_polygons(polygons)
}
\arguments{
  \item{polygons}{a list of matrices with two columns
  representing polygons (i.e. last row equals first row).}
}
\value{
  A list with a boolean vector \code{is_interior}, of same
  length as the given list, specifying at each index
  whether the polygon is inside another one; and a integer
  vector \code{index_parent} specifying at each index which
  polygon is its "container". If 0 then the polygon is not
  contained in any other.
}
\description{
  This function takes a list of polygons (i.e. matrices
  with two columns), and for each polygon, checks if it
  lies completely within another polygon of the list.
}
\details{
  It also checks for intersection, which the package
  doesn't deal with. The core function is taken from the
  function \code{\link{inout}} from package \code{splancs}.
}
\seealso{
  The function is used in \code{\link{shape_from_polygons}}
}

