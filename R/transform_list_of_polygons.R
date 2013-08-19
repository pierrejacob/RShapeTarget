#'@export
shift_x <- function(shift, polygon){
  for (index in seq_along(polygon)){
    polygon[[index]][,1] <- polygon[[index]][,1] + shift
  }
  return(polygon)
}
#'@export
shift_y <- function(shift, polygon){
  for (index in seq_along(polygon)){
    polygon[[index]][,2] <- polygon[[index]][,2] + shift
  }
  return(polygon)
}

#'@export
combine <- function(sequence_of_polygons){
  result <- list()
  for (index_poly in seq_along(sequence_of_polygons)){
    for (index_path in seq_along(sequence_of_polygons[[index_poly]])){
      result[[length(result)+1]] <- sequence_of_polygons[[index_poly]][[index_path]]
    }
  }
  return(result)
}

#'@export
get_box <- function(polygon, xfrac = 0.1, yfrac = 0.1){
  box <- polygon[[1]]
  if (length(polygon)>1){
    for (index in 2:length(polygon)){
      box <- rbind(box, polygon[[index]])
    }
  }
  return(sbox(box, xfrac=xfrac, yfrac=yfrac))
}

# #'@export
# extract_paths_from_word <- function(word, spacing = NULL){
#   if (length(word) == 0){
#     stop("'word' should be a non-empty string")
#   }
#   char_vector <- substring(word, seq(1,nchar(word),1), seq(1,nchar(word),1))
#   seq_polygons <- c()
#   for (char in char_vector){
#     seq_polygons <- c(seq_polygons, extract_paths_from_letter(char))
#   }
#   
# }
