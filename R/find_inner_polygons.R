#' @rdname find_inner_polygons
#' @name find_inner_polygons
#' @aliases find_inner_polygons
#' @title Find which polygons are within another
#' @description
#' This function takes a list of polygons (i.e. matrices with two columns),
#' and for each polygon, checks if it lies completely within another polygon of the list.
#' @param polygons a list of matrices with two columns representing polygons (i.e. last row equals first row).
#' @return A list with a boolean vector \code{is_interior}, of same length as the given list,
#' specifying at each index whether the polygon is inside another one; and a integer vector \code{index_parent} specifying
#' at each index which polygon is its "container". If 0 then the polygon is not contained in any other.
#' @details
#' It also checks for intersection, which the package doesn't deal with.
#' The core function is taken from the function \code{\link{inout}} from package \code{splancs}.
#' @seealso The function is used in \code{\link{shape_from_polygons}}
#' @export
find_inner_polygons <- function(polygons){
  is_interior <- rep(FALSE, length(polygons))
  index_parent <- rep(0, length(polygons))
  for (i in seq_along(polygons)){
    one_poly <- polygons[[i]]
    for (j in seq_along(polygons)){
      if (i != j){
        another_poly <- polygons[[j]]
        io <- inout(one_poly, another_poly)
        if (any(io) && (!any(io))){
          stop("polygons intersect, can't create target distribution")
        }
        if (all(io)){
          is_interior[i] <- is_interior[i] || TRUE
          index_parent[i] <- j
        }
      }
    }
  }
  return(list(is_interior=is_interior,index_parent=index_parent))
}
