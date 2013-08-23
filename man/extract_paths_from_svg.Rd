\name{extract_paths_from_svg}
\alias{extract_paths_from_svg}
\title{Extract list of polygons from a SVG file}
\usage{
  extract_paths_from_svg(file_name, close_paths = TRUE)
}
\arguments{
  \item{file_name}{path to a SVG file with "path" items in
  it.}

  \item{close_paths}{Should the path be closed
  automatically, i.e. last point = first point ? Default to
  TRUE.}
}
\value{
  A list of matrices, each representing a polygon.
}
\description{
  This function takes SVG file name as argument, and
  returns a list of matrices, each representing a polygon.
  The list of matrices represent the geometry of the shape.
}
\seealso{
  \code{\link{extract_paths_from_word}}
}

