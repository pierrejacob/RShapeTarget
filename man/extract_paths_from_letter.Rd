\name{extract_paths_from_letter}
\alias{extract_paths_from_letter}
\title{Extract list of polygons from a letter}
\usage{
extract_paths_from_letter(letter)
}
\arguments{
  \item{letter}{a character that should belong to
  \code{letters} or \code{LETTERS}.}
}
\value{
A list of matrices, each representing a polygon.
}
\description{
This function takes a letter as argument, and returns a
list of matrices, each representing a polygon. The list of
matrices represent the geometry of the letter.
}
\details{
This function first reads a SVG file corresponding to the
letter given as argument (those SVG files were created
beforehand using \code{\link{create_svg_letter}}). Then it
processes it, e.g. to remove the Beziers curve handles and
stick with segments only.
}
\seealso{
\code{\link{extract_paths_from_word}}
}

