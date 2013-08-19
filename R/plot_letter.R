
#'@export
plot_letter <- function(letter){
  if (!(letter %in% letters || letter %in% LETTERS)){
    stop("argument is not a letter")
  }
  paths <- extract_paths_from_letter(letter)
  p <- qplot(x=c(), y = c(), geom = "blank")
  for (index in 1:length(paths)){
    p <- p + geom_polygon(aes_string(x = paste0("paths[[", index,"]][,1]"), y = paste0("paths[[", index,"]][,2]")), alpha = 0.2)
  }
  return(p)  
}

