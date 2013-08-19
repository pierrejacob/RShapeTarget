# remove all objects
#graphics.off()
#rm(list = ls())
# try to detach the package if it was already loaded
try(detach(package:PAWL, unload = TRUE), silent = TRUE)
# load the package
library(PAWL)
library(RLetter)
library(mvtnorm)
library(splancs)
# starting points for MCMC algorithms


# get the letter's polygon
outer_polygon <- extract_paths("C")[[1]]
rinit <- function(size) rmvnorm(size, mean = apply(outer_polygon, 2, mean), sigma = cov(outer_polygon))
lambda <- 1
parameters <- list(lambda=lambda, outer_polygon=outer_polygon)
# target log density function
logdensity <- function(x, parameters){
  logtargetdensity <- rep(0, dim(x)[1])
  inside <- inout(x, parameters$outer_polygon)
  logtargetdensity[inside] <- 0
  logtargetdensity[!inside] <- (-1/(2*parameters$lambda))*dist_to_poly(x[!inside,], parameters$outer_polygon)
  return(logtargetdensity)
}
# creating the target object
letter_target <- target(name = "letter", dimension = 2,
                         rinit = rinit, logdensity = logdensity,
                         parameters = parameters)
# setting a seed for the RNG
set.seed(17)

#######
## Adaptive Metropolis-Hastings
#######
mhparameters <- tuningparameters(nchains = 10, niterations = 1000, storeall = TRUE)
amhresults <- adaptiveMH(letter_target, mhparameters)
# check that it's working
chains <- ConvertResults(amhresults)
head(chains)
ggplot(chains, aes(x=X1, y = -X2, colour = indexchain)) + geom_point() + xlim(250,310) + ylim(-500, -430)
# geom_density2d() + xlim(range(amhresults$allchains[,,1] + c(-10,10))) +
#   ylim(range(amhresults$allchains[,,2])+c(-10,10))
