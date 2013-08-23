#' @rdname extract_paths_from_letter
#' @name extract_paths_from_letter
#' @aliases extract_paths_from_letter
#' @title Extract list of polygons from a letter
#' @description
#' This function takes a letter as argument, and returns a list of matrices,
#' each representing a polygon. The list of matrices represent the geometry of the letter.
#' @param letter a character that should belong to \code{letters} or \code{LETTERS}.
#' @return A list of matrices, each representing a polygon.
#' @details
#' This function first reads a SVG file corresponding to the letter given as argument (those SVG files were created
#' beforehand using \code{\link{create_svg_letter}}). Then it processes it, e.g. to remove the Beziers curve handles
#' and stick with segments only.
#' @seealso \code{\link{extract_paths_from_word}}
#' @export
extract_paths_from_letter <- function(letter){
  if (!(letter %in% letters || letter %in% LETTERS)){
    stop("argument is not a letter")
  }
  if (is.na(str_locate(string=letter, pattern="[a-z]")[,1])){
    file_name <- suppressWarnings(normalizePath(paste0(system.file(package = "RShapeTarget", "extdata/"), letter, "_uppercase_path.svg")))
  } else {
    file_name <- suppressWarnings(normalizePath(paste0(system.file(package = "RShapeTarget", "extdata/"), letter, "_path.svg")))
  }
  # retrieve the SVG file corresponding to the letter
  top <- xmlInternalTreeParse(file=file_name, useInternalNodes=TRUE)
  # retrieve the paths
  els = xpathApply(doc=top, path="//svg:path[@d]", fun=xmlGetAttr, "d")
  # collapse them together
  els <- paste(els, collapse = " ")
  # unfortunately these paths can be a bit more complicated than a list of vertices, so we need to 
  # process them a bit with extract_paths_.
  paths <- extract_paths_(els)
  for (index in seq_along(paths)){
    # SVG y-axis seems to be directed downwards, 
    # which is the opposite of R's standard. Let's negate the y component
    paths[[index]][,2] <- -paths[[index]][,2]
  }
  return(paths)
}

#' @rdname extract_paths_from_word
#' @name extract_paths_from_word
#' @aliases extract_paths_from_word
#' @title Extract list of polygons from a words
#' @description
#' This function takes a word as argument, and returns a list of matrices,
#' each representing a polygon. The list of matrices represent the geometry of each letter
#' of the word.
#' @param word a character vector made of  \code{letters} or \code{LETTERS}.
#' @param spacing additional argument to add to the spacing between letters.
#' @return A list of matrices, each representing a polygon.
#' @details
#' This function first calls \code{\link{extract_paths_from_letter}} on each character.
#' Then it shifts each letter to the right, one after the other so that they appear next to each other.
#' Like in a word.
#' @seealso \code{\link{extract_paths_from_letter}}
#' @export
extract_paths_from_word <- function(word, spacing = 5){
  # I could allow for negative spacing but if the
  # polygons intersect, well the user is in trouble with the current implementation.
  if (spacing < 0){
    stop("'spacing' should be non-negative")
  }
  if (length(word) == 0 && !(class(word)=="character")){
    stop("'word' should be a non-empty string")
  }
  char_vector <- substring(word, seq(1,nchar(word),1), seq(1,nchar(word),1))
  seq_polygons <- list()
  for (char in char_vector){
    if (!(char %in% letters || char %in% LETTERS)){
      stop("argument contains non-letters")
    }
    seq_polygons[[length(seq_polygons)+1]] <- extract_paths_from_letter(char)
  }
  lbox <- get_box_polygon(polygon=seq_polygons[[1]], xfrac=0.0, yfrac=0.1)
  end_x <- max(lbox[,1])
  print(end_x)
  if (length(seq_polygons)>1){
    for (index in 2:length(seq_polygons)){
      lbox <- get_box_polygon(polygon=seq_polygons[[index]], xfrac=0.0, yfrac=0.1)
      shift <- end_x - min(lbox[,1]) + spacing
      cat("shift= ", shift, "\n")
      seq_polygons[[index]] <- shift_polygon_x(shift=shift, polygons=seq_polygons[[index]])
      lbox <- get_box_polygon(polygon=seq_polygons[[index]], xfrac=0.0, yfrac=0.1)
      end_x <- max(lbox[,1])
      print(end_x)
    }
  }
  return(combine_polygons(seq_polygons))
}