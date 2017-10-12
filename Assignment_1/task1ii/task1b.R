library(pracma)

lspace <- linspace(-1, 1, n = 100)
lspace <- seq(-1,1,by=0.01)

###########
# Formula 1
###########
f1 <- function(x, q){
  tmp <- 0
  for ( k in 0:q ){
    tmp <- tmp + alpha_q[k+1] * x^k
  }
  tmp
}

###########
# Formula 2
###########
L <- function( q, k ){
  choose(q, k) *
  choose((q+k-1)/2, q)
}

L_x <- function(x,q){
  s <- rep(0, length(x))
  beta_q <- runif(6, min=-1, max=1)
  for (k in 0:q){
    s <- s + x^k * L( q=q, k=k ) * beta_q[k+1]
  }
  return (2^q * s)
}


for (s in 1:3){
 sol <- rep(0,201)
 alpha_q <<- runif(6, min=-1, max=1)
 for ( i in 1:length(sol) ){
   sol[i] <- f1(lspace[i], 5)
 }
 write.table(sol, sprintf("alpha_%d.csv",s), col.names=FALSE,row.names=FALSE, sep=",")
 plot(lspace, sol, type="l", ylab="$L_{1}$", xlab="$x$", main="alpha")
}


for (s in 1:3){
  #plot(lspace, L_x(lspace, 5), type="l", ylab="$L_{1}$", xlab="$x$", main="beta")
  write.table(L_x(lspace, 5), sprintf("beta_%d.csv",s), col.names=FALSE,row.names=FALSE, sep=",")
}
