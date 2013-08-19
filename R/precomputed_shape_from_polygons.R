compute_ABC <- function(polygon){
  xv <- polygon[,1]
  yv <- polygon[,2]
  npoints <- dim(polygon)[1]
  A <- -diff(yv)
  B <-  diff(xv)
  C <- yv[2:npoints] * xv[1:(npoints-1)] - xv[2:npoints] * yv[1:(npoints-1)]
  return(list(A=A, B=B, C=C))
}

#'@export
precomputed_shape_from_polygons <- function(polygons){
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