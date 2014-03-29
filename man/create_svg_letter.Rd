\name{create_svg_letter}
\alias{create_svg_letter}
\title{Create SVG file with one letter}
\usage{
create_svg_letter(letter)
}
\arguments{
  \item{letter}{a character that should belong to
  \code{letters} or \code{LETTERS}.}
}
\value{
None.
}
\description{
This function creates a SVG file with one letter in it, and
calls Inkscape to convert it into a path. It is assumed
that Inkscape is configured to export SVG files WITHOUT
relative coordinates (File > Inkscape Preferences > Allow
relative coordinates).
}
\details{
Inkscape must be installed for this function to work.
Essentially it calls Inkscape from the command line with
the following command: \code{inkscape FILENAME --select=ID
--verb=ObjectToPath --verb=FileSave --verb=FileClose}
}

