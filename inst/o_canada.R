# This script is to pay my respects to the great country of Canada
# remove all objects
rm(list = ls())
# try to detach the package if it was already loaded
try(detach(package:RShapeTarget, unload = TRUE), silent = TRUE)
# load the package
library(RShapeTarget)
# load PAWL for adaptive MCMC 
library(PAWL)
# target 
shape <- create_target_from_shape(file_name=system.file(package = "RShapeTarget", "extdata/canada.svg"), lambda=2)
# starting points for MCMC algorithms
rinit <- function(size)  csr(shape$bounding_box, size)
# creating the target object
canada_target <- target(name = "canada", dimension = 2,
                        rinit = rinit, logdensity = shape$logd,
                        parameters = shape$algo_parameters)
#######
## Adaptive Metropolis-Hastings
#######
mhparameters <- tuningparameters(nchains = 10, niterations = 5000, storeall = TRUE)
Rprof(tmp <- tempfile())
amhresults <- adaptiveMH(canada_target, mhparameters)
Rprof()
# display profiling results
print(summaryRprof(tmp))
unlink(tmp)

chains <- ConvertResults(amhresults)
theme_set(theme_bw())
theme_update(axis.ticks = element_blank(), 
             axis.text.x = element_blank(), axis.text.y = element_blank())
g <- ggplot(chains, aes(x=X1, y = X2, alpha = exp(logdens))) + geom_point(colour = "red") +
  theme(legend.position="none") + xlab("X") + ylab("Y")
print(g)
# ggsave(g, filename="~/Dropbox/RLetter/RShape_o_canada.png")