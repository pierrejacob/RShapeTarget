# This script shows how to create and sample from "word distributions"
# remove all objects
rm(list = ls())
# try to detach the package if it was already loaded
try(detach(package:RShapeTarget, unload = TRUE), silent = TRUE)
# load the package
library(RShapeTarget)
# load PAWL for adaptive MCMC 
library(PAWL)
word <- create_target_from_word(word="Hodor", lambda=0.5)
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

chains <- ConvertResults(amhresults)
theme_set(theme_bw())
theme_update(axis.ticks = element_blank(), 
             axis.text.x = element_blank(), axis.text.y = element_blank())
g <- ggplot(chains, aes(x=X1, y = X2, colour = exp(logdens), alpha = exp(logdens))) + geom_point() +
  theme(legend.position="none") + xlab("X") + ylab("Y")
print(g)
# ggsave(g, filename="~/Dropbox/RLetter/RShape_hodor.png")
