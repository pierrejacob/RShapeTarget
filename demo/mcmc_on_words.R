# remove all objects
rm(list = ls())
# try to detach the package if it was already loaded
try(detach(package:RShapeTarget, unload = TRUE), silent = TRUE)
# load the package
library(PAWL)
library(RShapeTarget)

word <- create_target_from_word(word="Louise", lambda=0.5)
# starting points for MCMC algorithms
rinit <- function(size)  csr(word$bounding_box, size)
# creating the target object
letter_target <- target(name = "word", dimension = 2,
                        rinit = rinit, logdensity = word$logd,
                        parameters = word$algo_parameters)
#######
## Adaptive Metropolis-Hastings
#######
mhparameters <- tuningparameters(nchains = 10, niterations = 5000, storeall = TRUE)
Rprof(tmp <- tempfile())
amhresults <- adaptiveMH(letter_target, mhparameters)
Rprof()
# display profiling results
print(summaryRprof(tmp))
unlink(tmp)
# Plots showing exploration over time

# check that it's working
chains <- ConvertResults(amhresults)

head(chains)
library(ggthemes)
theme_set(theme_solarized(light=FALSE))
ggplot(chains, aes(x=X1, y = X2, alpha = logdens)) + geom_point(colour = "lightblue") +
  theme(legend.position="none")

ggplot(chains, aes(x=X1, y = X2, alpha = logdens)) + geom_density2d(colour = "lightblue") +
  theme(legend.position="none")
