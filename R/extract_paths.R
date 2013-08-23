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
  # I remove the "z" at the end if any
  x <- x[x != "Z"]
  x <- x[x != "z"]
  indices_letter <- which(str_locate(x, "[A-Za-z]")[,1] == 1)
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
      } else {
        # I just drop the letter and do nothing
        index <- index + 1
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
