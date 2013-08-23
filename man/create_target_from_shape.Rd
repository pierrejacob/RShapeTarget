\name{create_target_from_shape}
\alias{create_target_from_shape}
\title{Create target density based on a shape}
\usage{
  create_target_from_shape(file_name, lambda = 1,
    close_paths = TRUE)
}
\arguments{
  \item{file_name}{path to a SVG file with "path" items in
  it.}

  \item{lambda}{smoothness parameter, default to 1.}

  \item{close_paths}{Should the path be closed
  automatically, i.e. last point = first point ? Default to
  TRUE.}
}
\value{
  A list containing the log target density 'logd', a list
  'algo_parameters' to be given to 'logd' at each call, and
  a bounding box 'bounding_box' giving the coordinates of a
  square encompassing the shape.
}
\description{
  This function takes a SVG file and a smoothness argument
  and creates all the objects to compute a log target
  density function that looks like the shape. The lambda
  parameter controls this decay of the density when going
  away from the shape.
}

