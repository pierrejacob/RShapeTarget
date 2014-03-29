\name{extract_paths_from_word}
\alias{extract_paths_from_word}
\title{Extract list of polygons from a words}
\usage{
extract_paths_from_word(word, spacing = 5)
}
\arguments{
  \item{word}{a character vector made of \code{letters} or
  \code{LETTERS}.}

  \item{spacing}{additional argument to add to the spacing
  between letters.}
}
\value{
A list of matrices, each representing a polygon.
}
\description{
This function takes a word as argument, and returns a list
of matrices, each representing a polygon. The list of
matrices represent the geometry of each letter of the word.
}
\details{
This function first calls
\code{\link{extract_paths_from_letter}} on each character.
Then it shifts each letter to the right, one after the
other so that they appear next to each other. Like in a
word.
}
\seealso{
\code{\link{extract_paths_from_letter}}
}

