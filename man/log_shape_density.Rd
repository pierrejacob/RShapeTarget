\name{log_shape_density}
\alias{log_shape_density}
\title{Log target density function from shape}
\usage{
  log_shape_density(x, algo_parameters)
}
\arguments{
  \item{x}{a mx2 matrix of points at which to evaluate the
  log density function.}

  \item{algo_parameters}{a list containing 'lambda', the
  smoothness parameter, and 'shape', the shape
  corresponding to a shape and probably created using
  \code{\link{shape_from_polygons}}.}
}
\value{
  a vector of log density evaluations corresponding to the
  given points.
}
\description{
  This function takes a point and a list of arguments and
  returns the value of the log density function.
}

