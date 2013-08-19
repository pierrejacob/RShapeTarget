# SVG Paths are documented here:
# http://www.w3.org/TR/SVG/paths.html
# essentially curve points are given by
# (x1,y1 x2,y2 x,y)
# and we want to keep only
# x,y
# so we jump over two points
curve_to_segment <- function(x){
  if (length(x) %% 3 != 0){
    stop("argument's length must be %%3 == 0")
  }
  return(x[2+seq(from=1, to=length(x), by=3)])
}

# this function removes the letters, and deals with the curve points
remove_letters <- function(x){
  indices_letter <- which(str_locate(x, "[A-Z]")[,1] == 1)
  indices_C <- which(str_locate(x, "[C]")[,1] == 1)
  index <- 1
  n <- length(x)
  res <- c()
  while (index <= n){
    if (index %in% indices_letter){
      if (index %in% indices_C){
        endC <- (c(indices_letter, n+1))[which(c(indices_letter,n+1) > index)[1]]
        seq_C <- x[(index+1):(endC-1)]
        seq_C <- curve_to_segment(seq_C)
        res <- c(res, seq_C)
        index <- endC+1
      }
    } else {
      res <- c(res, x[index])
      index <- index + 1
    }
  }
  return(res)
}
extract_paths_ <- function(s){
  # extract whatever is between "M" characaters in s
  between_m <- str_split(s, "M")
  # remove the empty bits
  between_m <- between_m[[1]][which(lapply(X=between_m, FUN= function(x) (x!=""))[[1]])]
  result <- list()
  for (ipath in seq_along(between_m)){
    ## convert curves to segments
    path <- str_split(between_m[ipath], "\\s")
    path <- path[[1]][which(sapply(path[[1]], function(x) (x!="")))]
    path <- sapply(remove_letters(path), function(x) as.numeric(str_split(x, ",")[[1]]), simplify=TRUE, USE.NAMES=FALSE)
    result[[ipath]] <- t(path)
  }
  return(result)
}

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
    file_name <- suppressWarnings(normalizePath(paste0(system.file(package = "RShapeTarget", "data/"), letter, "_uppercase_path.svg")))
  } else {
    file_name <- suppressWarnings(normalizePath(paste0(system.file(package = "RShapeTarget", "data/"), letter, "_path.svg")))
  }
  # retrieve the SVG file corresponding to the letter
  svg <- xmlTreeParse(file_name, useInternalNodes=TRUE, addAttributeNamespaces=TRUE, fullNamespaceInfo=FALSE)
  root <- xmlRoot(svg)
  # focus on the path corresponding to the letter
  s <- xmlAttrs(node=root[["g"]][["g"]][["path"]])[["d"]]
  # unfortunately this path is a bit more complicated than a list of vertices, so we need to 
  # process them a bit with extract_paths_.
  paths <- extract_paths_(s)
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
extract_paths_from_word <- function(word, spacing = 0){
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
  lbox <- lapply(X=seq_polygons, 
                 FUN=function(x) get_box_polygon(polygon=x, xfrac=0.5, yfrac=0.1))
  end_x <- range(lbox[[1]][,1])[2]
  if (length(lbox)>1){
    for (index in 2:length(lbox)){
      shift <- end_x - range(lbox[[index]][,1])[1] + spacing
      seq_polygons[[index]] <- shift_polygon_x(shift=shift, polygons=seq_polygons[[index]])
      end_x <- range(lbox[[index]][,1])[2] + shift
    }
  }
  return(combine_polygons(seq_polygons))
}