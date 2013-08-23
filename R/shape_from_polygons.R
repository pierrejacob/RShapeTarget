#' @rdname compute_ABC
#' @name compute_ABC
#' @aliases compute_ABC
#' @title Pre compute quantities
#' @description
#' precomputes some quantities specific to the given polygon, to optimize subsequent distance computations.
#' @param polygon nx2 matrix representing a polygon; 
#' @return a list of precomputed quantities
#' @export
compute_ABC <- function(polygon){
  xv <- polygon[,1]
  yv <- polygon[,2]
  npoints <- dim(polygon)[1]
  A <- -diff(yv)
  B <-  diff(xv)
  C <- yv[2:npoints] * xv[1:(npoints-1)] - xv[2:npoints] * yv[1:(npoints-1)]
  AB <- 1/(A^2 + B^2)
  AAB <- A*AB
  BAB <- B*AB
  return(list(A=A, B=B, C=C, AB=AB, AAB=AAB, BAB=BAB))
}
#' @rdname shape_from_polygons
#' @name shape_from_polygons
#' @aliases shape_from_polygons
#' @title Convert polygons to shape
#' @description
#' This function takes a list of polygons (i.e. matrices with two columns),
#' and put some order in it. It classifies the polygons into outer or inner polygons, using
#' \code{\link{find_inner_polygons}}. It also precomputes some quantities to fasten the subsequent
#' computations.
#' @param polygons a list of matrices with two columns representing polygons (i.e. last row equals first row).
#' @return A list containing the outer and inner polygons in separate lists; some precomputed quantities 
#' 'outer_polygons_ABC' and 'inner_polygons_ABC' and a list of vectors 'index_children' allowing
#' to find in which of the outer polygons each inner polygon lies.
#' @seealso The function is used in \code{\link{create_target_from_word}}
#' @export
shape_from_polygons <- function(polygons){
  io <- find_inner_polygons(polygons)
  index_parent <- io$index_parent
  is_inner <- io$is_interior
  index_children <- list()
  outer_polygons <- list()
  inner_polygons <- list()
  if (all(!is_inner)){
    outer_polygons <- polygons
  } else {
    for (index in which(is_inner)){
      inner_polygons[[length(inner_polygons)+1]] <- polygons[[index]]
    }
    for (index in which(!is_inner)){
      outer_polygons[[length(outer_polygons)+1]] <- polygons[[index]]
      index_children[[length(index_children)+1]] <- which(index_parent == index)
    }
  }
  outer_polygons_ABC <- list()
  inner_polygons_ABC <- list()
  for (index in seq_along(outer_polygons)){
    outer_polygons_ABC[[index]] <- compute_ABC(outer_polygons[[index]])
  }
  for (index in seq_along(inner_polygons)){
    inner_polygons_ABC[[index]] <- compute_ABC(inner_polygons[[index]])
  }
  return(list(outer_polygons=outer_polygons, outer_polygons_ABC=outer_polygons_ABC,
              inner_polygons=inner_polygons, inner_polygons_ABC=inner_polygons_ABC,
              index_children=index_children))
}


