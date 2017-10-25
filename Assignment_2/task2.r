

generateDataset <- function(n){
  x <- runif(n, min=-1, max=1)
  x <- x[order(x)]
  y <- sapply(sin(pi*x), function(t) t + rnorm(1, mean = 0, sd = 1))

  return(data.frame(x=x, y=y))
}


# Legendre polynomials
legendre <- function(x,n){
	val=0
	for(i in 0:n){
		val=val+((x^i)*choose(n,i)*choose((n+i-1)/2,n))
	}
	return((2^n)*val)
}

################################################################################
################################################################################
# Legendre polynomials with betas
l <- function(x,q,k){
      x**k * choose(q,k) * choose((q+k-1)/2,q)
    }

legendres <- function(x,q){
    func <- rep(0,length(x))
        for(k in 0:q){
            func <- func + l(x,q,k)
        }
    func <- 2**q * func
    }

generateLegendre <- function(x, Qf, betas){
      #betas <- generateUniformValues(Q_f+1)
      func <- rep(0,length(x))
      for(q in 0:Qf){
          func <- func + betas[q+1] * legendres(x, q)
      }
      return(func)
    }

################################################################################
################################################################################

reguralizedEstimate <- function(data, order, lambda){
  y <- matrix(data$y, nrow=nrow(data), byrow=T)
  Z <- matrix(NA, nrow=nrow(data), ncol=order+1, byrow=T)

  for( row in 1:nrow(Z)){
    Z[row,1] <- 1

    for(col in 1:ncol(Z)){
      Z[row,col] <- legendre(data$x[row], col-1)
    }
  }
  w_hat =solve((t(Z)%*%Z)+(lambda*diag(order+1)))%*%(t(Z)%*%y)
  return(w_hat)
}

betas <- runif(11, min=-1, max=1)
reguralizedModel <- function(data, lambda, order){
  w_hat <- reguralizedEstimate(data=data, order=order, lambda=lambda)
  j <- w_hat[1]

  for(i in 2:11){
    j <- j + (w_hat[i]*(generateLegendre(data$x, 10, betas=betas)))
  }
  return(j)
}

buildModels <- function(){
  data <- generateDataset(50)
  m1 <- reguralizedModel(data, 0, 10)
  m2 <- reguralizedModel(data, 10, 10)
  plot(data$x, data$y)
  lines(data$x, sin(data$x*pi), col="red")
  lines(data$x, m1, type="l", col="blue")
  lines(data$x, m2, type="l")
}

buildModels()



#plot(legendre(data$x, 3), type="l")
#plot(data$x, data$y)
#lines(data$x, sin(pi*data$x))
