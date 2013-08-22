# remove all objects
rm(list = ls())
# try to detach the package if it was already loaded
try(detach(package:RShapeTarget, unload = TRUE), silent = TRUE)
# load the package
library(PAWL)
library(RShapeTarget)

shape <- create_target_from_shape(file_name=system.file(package = "RShapeTarget", "data/canada.svg"), lambda=2)
# starting points for MCMC algorithms
rinit <- function(size)  csr(shape$bounding_box, size)
# creating the target object
canada_target <- target(name = "canada", dimension = 2,
                        rinit = rinit, logdensity = shape$logd,
                        parameters = shape$algo_parameters)
#######
## Adaptive Metropolis-Hastings
#######
mhparameters <- tuningparameters(nchains = 10, niterations = 25000, storeall = TRUE)
Rprof(tmp <- tempfile())
amhresults <- adaptiveMH(canada_target, mhparameters)
Rprof()
# display profiling results
print(summaryRprof(tmp))
unlink(tmp)

# check that it's working
chains <- ConvertResults(amhresults)

head(chains)
library(ggthemes)
theme_set(theme_solarized(light=FALSE))
ggplot(chains, aes(x=X1, y = X2, alpha = exp(logdens))) + geom_point(colour = "lightblue") +
  theme(legend.position="none")

