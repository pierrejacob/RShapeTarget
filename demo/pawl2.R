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

word <- create_target_from_word(word="Jacob", lambda=0.5)
rinit <- function(size)  csr(word$bounding_box, size)
# creating the target object
letter_target <- target(name = "word", dimension = 2,
                        rinit = rinit, logdensity = word$f,
                        parameters = word$algo_parameters)

#######
## Adaptive Metropolis-Hastings
#######
mhparameters <- tuningparameters(nchains = 10, niterations = 5000, storeall = TRUE)
amhresults <- adaptiveMH(letter_target, mhparameters)
# check that it's working
chains <- ConvertResults(amhresults)

head(chains)
library(ggthemes)
theme_set(theme_solarized(light=FALSE))
ggplot(chains, aes(x=X1, y = X2, alpha = logdens)) + geom_point(colour = "lightblue") +
  theme(legend.position="none")

