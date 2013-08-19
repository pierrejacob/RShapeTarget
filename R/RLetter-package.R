#'@name RLetter-package
#'@aliases RLetter
#'@docType package
#'@title Letter Target Distributions
#'@author Pierre E. Jacob <pierre.jacob.work@@gmail.com>
#'@description This package allows to create 2-dimensional probability distributions from letters and words. 
#'@details It's kinda fun and it allows to create multimodal target densities easily.
#'
#'The idea is that a letter is a shape, and we can define a probability density function proportional
#'to 
#'\itemize{
#'\item 1 when x is inside the shape
#'\item 0 when x is outside the shape
#'}
#'This density function is very discontinuous, so we can smooth it using a Gaussian kernel
#'to define
#'
#' f_lambda(x) = sup_theta f(theta) exp(-1/2lambda* distance(x,theta))
#' 
#' The parameter lambda controls the smoothness
#' @seealso \code{\link{create_target_from_word}}
#' @keywords package
#' @useDynLib RLetter
NULL