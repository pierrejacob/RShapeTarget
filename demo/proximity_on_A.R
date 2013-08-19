library(mvtnorm)
library(splancs)
# get the letter's polygon
outer_polygon <- extract_paths("B")[[1]]
# inner_polygon <- extract_paths("A")[[1]]

plot_polygon <- function(polygon){
  p <- qplot(x=polygon[,1], y = -polygon[,2], geom = "polygon")
  return(p)  
}
lambda <- 0.1
val0 <- -log(2*pi*lambda)
target_density <- function(points){
  logtargetdensity <- rep(0, dim(points)[1])
  inside <- inout(points, outer_polygon)
  logtargetdensity[inside] <- 0
  logtargetdensity[!inside] <- (-1/(2*lambda))*dist_to_poly(points[!inside,], outer_polygon)
  return(logtargetdensity)
}
square <- sbox(outer_polygon, xfrac=0.5,yfrac=0.5)
rAsquare <- csr(square, 10000)

distances <- target_density(rAsquare)
plot_polygon(outer_polygon) + geom_point(aes(x=rAsquare[,1], y=-rAsquare[,2], colour = distances))

df <- data.frame(cbind(rAsquare[,1], rAsquare[,2], distances))
names(df) <- c("x", "y", "logd")
