

set.seed(111)
generateDataset <- function(size){
  return( runif(size, min=-1, max=1) )
}

targetFunction <- function(size){
  y <- sapply(0.8*runif(size, min=-1, max=1), function(t) t + rnorm(1,0,1))
  return(y)
}

g1 <- function(size){
  y <- lm(targetFunction(size) ~ runif(size, min=-1, max=1), offset=rep(0.5,size))
}

plot(g1(30))
