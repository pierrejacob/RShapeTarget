# remove all objects
rm(list = ls())
# try to detach the package if it was already loaded
try(detach(package:RShapeTarget, unload = TRUE), silent = TRUE)
# load the package
library(RShapeTarget)

# create a shape from a spiral shape, and do not close the path (it's not a polygon it's a spiral)
file_name <- system.file(package = "RShapeTarget", "extdata/spiral.svg")
paths <- extract_paths_from_svg(file_name, close_paths=FALSE)
# what does it look like
plot_paths(paths)
# Now let us generate points on the plane
# and their associated log density
# First create target with some smoothness parameter lambda
spiral <- create_target_from_shape(file_name = file_name, lambda=10, close_paths=FALSE)
# generate in a square surrounding the shape
rinit <- function(size)  csr(spiral$bounding_box, size)
x <- rinit(10000)
# evaluate the log densities associated to these points
logdensities <- spiral$logd(x, spiral$algo_parameters)
# plot the points with colour as a function of the density
plot_paths(paths) + geom_point(aes(x=x[,1], y=x[,2], colour=exp(logdensities)))

