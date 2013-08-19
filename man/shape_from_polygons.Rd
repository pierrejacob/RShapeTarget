\name{shape_from_polygons}
\alias{shape_from_polygons}
\title{Convert polygons to shape}
\usage{
  shape_from_polygons(polygons)
}
\arguments{
  \item{polygons}{a list of matrices with two columns
  representing polygons (i.e. last row equals first row).}
}
\value{
  A list containing the outer and inner polygons in
  separate lists; some precomputed quantities
  'outer_polygons_ABC' and 'inner_polygons_ABC' and a list
  of vectors 'index_children' allowing to find in which of
  the outer polygons each inner polygon lies.
}
\description{
  This function takes a list of polygons (i.e. matrices
  with two columns), and put some order in it. It
  classifies the polygons into outer or inner polygons,
  using \code{\link{find_inner_polygons}}. It also
  precomputes some quantities to fasten the subsequent
  computations.
}
\seealso{
  The function is used in
  \code{\link{create_target_from_word}}
}

