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
