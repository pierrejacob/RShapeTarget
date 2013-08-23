#' @rdname extract_paths_from_svg
#' @name extract_paths_from_svg
#' @aliases extract_paths_from_svg
#' @title Extract list of polygons from a SVG file
#' @description
#' This function takes SVG file name as argument, and returns a list of matrices,
#' each representing a polygon. The list of matrices represent the geometry of the shape.
#' @param file_name path to a SVG file with "path" items in it.
#' @param close_paths Should the path be closed automatically, i.e. last point = first point ? Default to TRUE.
#' @return A list of matrices, each representing a polygon.
#' @seealso \code{\link{extract_paths_from_word}}
#' @export
extract_paths_from_svg <- function(file_name, close_paths = TRUE){
  top <- xmlInternalTreeParse(file=file_name, useInternalNodes=TRUE)
  els = xpathApply(doc=top, path="//svg:path[@d]", fun=xmlGetAttr, "d")
  els <- paste(els, collapse = " ")
  
  paths <- extract_paths_(els)
  #   print(paths)
  for (index in seq_along(paths)){
    # SVG y-axis seems to be directed downwards, 
    # which is the opposite of R's standard. Let's negate the y component
    paths[[index]][,2] <- -paths[[index]][,2]
    if (close_paths && all(head(paths[[index]], 1) != tail(paths[[index]], 1))){
      paths[[index]] <- rbind(paths[[index]], head(paths[[index]], 1))
    }
  }
  return(paths)
}