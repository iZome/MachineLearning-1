library(pracma)


L <- function( q, k ){
  choose(q, k) *
  choose((q+k-1)/2, q)
}

L_x <- function(x,q){
  s <- rep(0, length(x))
  for (k in 0:q){
    s <- s + x^k * L( q=q, k=k )
  }
  return (2^q * s)
}

sol <- L_x(linspace(-1, 1, n = 500), 0)
write.table(sol, "0.csv", col.names=FALSE,row.names=FALSE, sep=",")


#svg(filename="./test.svg")
#plot(linspace(-1, 1, n = 100), sol, type="l", xlab="$x$", ylab="$L{_1}(x)$")
