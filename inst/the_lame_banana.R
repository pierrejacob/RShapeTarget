# This script shows how to sample from the lame distribution 
# logdensity <- function(x) {
#   B <- 0.03 # controls ’bananacity’
#   -x[1]^2/200 - 1/2*(x[2]+B*x[1]^2-100*B)^2
# }
# remove all objects
rm(list = ls())
# load PAWL for adaptive MCMC 
library(PAWL)
# target
logdensity <- function(x, parameters) {
  -x[,2]^2/200 - 1/2*(x[,1]+parameters$B*x[,2]^2-100*parameters$B)^2
}

# starting points for MCMC algorithms
rinit <- function(size)  rmvnorm(n=size, mean = c(0,0))
# creating the target object
lame_banana_target <- target(name = "lame_banana", dimension = 2,
                       rinit = rinit, logdensity = logdensity,
                       parameters = list(B = 0.03))
#######
## Adaptive Metropolis-Hastings
#######
mhparameters <- tuningparameters(nchains = 10, niterations = 5000, storeall = TRUE)
Rprof(tmp <- tempfile())
amhresults <- adaptiveMH(lame_banana_target, mhparameters)
Rprof()
# display profiling results
print(summaryRprof(tmp))
unlink(tmp)

chains <- ConvertResults(amhresults)
theme_set(theme_bw())
theme_update(axis.ticks = element_blank(), 
             axis.text.x = element_blank(), axis.text.y = element_blank())
g <- ggplot(chains, aes(x=X1, y = X2, alpha = exp(logdens))) + geom_point(colour = "orange") +
  theme(legend.position="none") + xlab("X") + ylab("Y")
print(g)
# ggsave(g, filename="~/Dropbox/RLetter/RShape_lame_banana.png")