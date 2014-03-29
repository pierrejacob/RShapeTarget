#' @rdname shift_polygon_x
#' @name shift_polygon_x
#' @aliases shift_polygon_x
#' @title Shift a polygon horizontally
#' @param shift value of the shift (positive value = to the right).
#' @param polygons a list of matrices representing the polygons.
#' @return A list of matrices representing the shifted polygons
#' @export
shift_polygon_x <- function(shift, polygons){
  for (index in seq_along(polygons)){
    polygons[[index]][,1] <- polygons[[index]][,1] + shift
  }
  return(polygons)
}
#' @rdname shift_polygon_y
#' @name shift_polygon_y
#' @aliases shift_polygon_y
#' @title Shift a polygon vertically
#' @param shift value of the shift (positive value = to the top).
#' @param polygons a list of matrices representing the polygons.
#' @return A list of matrices representing the shifted polygons
#' @export
shift_polygon_y <- function(shift, polygons){
  for (index in seq_along(polygons)){
    polygons[[index]][,2] <- polygons[[index]][,2] + shift
  }
  return(polygons)
}

#' @rdname combine_polygons
#' @name combine_polygons
#' @aliases combine_polygons
#' @title Combine polygons
#' @param sequence_of_polygons a list of list of matrices.
#' @param shift Optional argument specifying how much the successive elements should be
#' horizontally shifted one from the other. Default is 0.
#' @return A list of matrices taken from sequence_of_polygons.
#' @export
combine_polygons <- function(sequence_of_polygons, shift = 0){
  result <- list()
  lbox <- lapply(X=sequence_of_polygons, 
                 FUN=function(x) get_box_polygon(polygon=x, xfrac=0.5, yfrac=0.1))
  end_x <- range(lbox[[1]][,1])[2]
  for (index_poly in seq_along(sequence_of_polygons)){
    if (index_poly > 1 && shift != 0){
      shift <- end_x - range(lbox[[index_poly]][,1])[1] + shift
      sequence_of_polygons[[index_poly]] <- shift_polygon_x(shift=shift, 
                                                  polygons=sequence_of_polygons[[index_poly]])
      end_x <- range(lbox[[index_poly]][,1])[2] + shift
    }
    for (index_path in seq_along(sequence_of_polygons[[index_poly]])){
      result[[length(result)+1]] <- sequence_of_polygons[[index_poly]][[index_path]]
    }
  }
  return(result)
}

#' @rdname get_box_polygon
#' @name get_box_polygon
#' @aliases get_box_polygon
#' @title Get bounding box around polygons
#' @param polygon a list of list of matrices.
#' @param xfrac From \code{splancs}'s doc: The fraction of the width of the point pattern by which the box will surround the point pattern to the left and right. Given to \code{\link{sbox}}.
#' @param yfrac From \code{splancs}'s doc: The fraction of the height of the point pattern by which the box will surround the point pattern to the top and bottom. Given to \code{\link{sbox}}. 
#' @return A polygon representing a square around the given polygon.
#' @export
get_box_polygon <- function(polygon, xfrac = 0.1, yfrac = 0.1){
  ymin <- min(unlist(lapply(polygon, function(x) apply(X=x, 2, min)[2])))
  ymax <- max(unlist(lapply(polygon, function(x) apply(X=x, 2, max)[2])))
  xmin <- min(unlist(lapply(polygon, function(x) apply(X=x, 2, min)[1])))
  xmax <- max(unlist(lapply(polygon, function(x) apply(X=x, 2, max)[1])))
  xw <- xmax - xmin
  xmin <- xmin - xfrac * xw
  xmax <- xmax + xfrac * xw
  yw <- ymax - ymin
  ymin <- ymin - yfrac * yw
  ymax <- ymax + yfrac * yw
  return(t(matrix(c(xmin, ymin, xmax, ymin, xmax, ymax, xmin, ymax), nrow = 2)))
}
# get_box_polygon <- function(polygon, xfrac = 0.1, yfrac = 0.1){
#   box <- polygon[[1]]
#   if (length(polygon)>1){
#     for (index in 2:length(polygon)){
#       box <- rbind(box, polygon[[index]])
#     }
#   }
#   return(sbox(box, xfrac=xfrac, yfrac=yfrac))
# }
