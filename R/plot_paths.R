#' @rdname plot_paths
#' @name plot_paths
#' @aliases plot_paths
#' @title Plot paths
#' @param paths a list of list of matrices, each representing a path.
#' @return a ggplot2 object
#' @export
plot_paths <- function(paths){
  if (class(paths) != "list"){
    stop("'plot_paths' takes a list as argument")
  }
  g <- qplot(x = paths[[1]][,1], y = paths[[1]][,2], geom = "blank") + geom_path(colour = "black")
  if (length(paths) > 1){
    for (index in 2:length(paths)){
      g <- g + geom_path(aes_string(x = paste0("paths[[", index, "]][,1]"),
                                    y = paste0("paths[[", index, "]][,2]")), colour = "black")
    }
  }
  g <- g + xlab("x") + ylab("y") + theme_minimal()
  return(g)
}