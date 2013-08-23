# remove all objects
rm(list = ls())
# try to detach the package if it was already loaded
try(detach(package:RShapeTarget, unload = TRUE), silent = TRUE)
# load the package
library(RShapeTarget)

# create a shape from a word
paths <- extract_paths_from_word("Hodor")
# what does it look like
plot_paths(paths)
# Now let us generate points on the plane
# and their associated log density
# First create target with some smoothness parameter lambda
word <- create_target_from_word(word="Hodor", lambda=1)
# generate in a square surrounding the shape
rinit <- function(size)  csr(word$bounding_box, size)
x <- rinit(10000)
# evaluate the log densities associated to these points
logdensities <- word$logd(x, word$algo_parameters)
# plot the points with colour as a function of the density
plot_paths(paths) + geom_point(aes(x=x[,1], y=x[,2], colour=exp(logdensities)))
