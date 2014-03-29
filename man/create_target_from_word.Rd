\name{create_target_from_word}
\alias{create_target_from_word}
\title{Create target density based on a word}
\usage{
create_target_from_word(word, lambda = 1)
}
\arguments{
  \item{word}{a character vector made of \code{letters} or
  \code{LETTERS}.}

  \item{lambda}{smoothness parameter, default to 1.}
}
\value{
A list containing the log target density 'logd', a list
'algo_parameters' to be given to 'logd' at each call, and a
bounding box 'bounding_box' giving the coordinates of a
square encompassing the word.
}
\description{
This function takes a word and a smoothness argument and
creates all the objects to compute a log target density
function that looks like the letter. The target density
function is equal to 1 on the shape of the letters, and
decreases to 0 far away from the letters. The lambda
parameter controls this decay.
}

