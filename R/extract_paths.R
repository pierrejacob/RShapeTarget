curve_to_segment <- function(x){
  if (length(x) %% 3 != 0){
    stop("argument's length must be %%3 == 0")
  }
  return(x[2+seq(from=1, to=length(x), by=3)])
}

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

#'@export
extract_paths_from_letter <- function(letter){
  if (is.na(str_locate(string=letter, pattern="[a-z]")[,1])){
    file_name <- suppressWarnings(normalizePath(paste0(system.file(package = "RLetter", "data/"), letter, "_uppercase_path.svg")))
  } else {
    file_name <- suppressWarnings(normalizePath(paste0(system.file(package = "RLetter", "data/"), letter, "_path.svg")))
  }
  svg <- xmlTreeParse(file_name, useInternalNodes=TRUE, addAttributeNamespaces=TRUE, fullNamespaceInfo=FALSE)
  root <- xmlRoot(svg)
  s <- xmlAttrs(node=root[["g"]][["g"]][["path"]])[["d"]]
  paths <- extract_paths_(s)
  for (index in seq_along(paths)){
    paths[[index]][,2] <- -paths[[index]][,2]
  }
  return(paths)
}

#'@export
extract_paths_from_word <- function(word, spacing = NULL){
  if (length(word) == 0){
    stop("'word' should be a non-empty string")
  }
  char_vector <- substring(word, seq(1,nchar(word),1), seq(1,nchar(word),1))
  seq_polygons <- list()
  for (char in char_vector){
    seq_polygons[[length(seq_polygons)+1]] <- extract_paths_from_letter(char)
  }
  lbox <- lapply(X=seq_polygons, 
                 FUN=function(x) get_box(polygon=x, xfrac=0.5, yfrac=0.1))
  end_x <- range(lbox[[1]][,1])[2]
  if (length(lbox)>1){
    for (index in 2:length(lbox)){
      shift <- end_x - range(lbox[[index]][,1])[1]
      seq_polygons[[index]] <- shift_x(shift=shift, seq_polygons[[index]])
      end_x <- range(lbox[[index]][,1])[2] + shift
    }
  }
  return(combine(seq_polygons))
}