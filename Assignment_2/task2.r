
set.seed(420)
func2 <- function(x, eps){
  return(sin(pi * x) + eps)
}

generateDataset <- function(n){
  x <- runif(n, min = -1, max = 1)
  x <- x[order(x)]
  e <- rnorm(n, mean = 0, sd = 1)
  y <- func2(x, e)
  return (data.frame(x = x, y = y))
}


data <- generateDataset(50)

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

reguralizedModel <- function(data, lambda, order, est=data){
  w_hat <- reguralizedEstimate(data=data, order=order, lambda=lambda)
  j <- w_hat[1]

  for(i in 2:11){
    j <- j + (w_hat[i]*(legendre(est$x, i-1)))
  }
  return(j)
}

buildModels <- function(){

  write.table(sin(data$x*pi), "data/sin.csv", col.names=FALSE,row.names=FALSE, sep=",", append=F)
  write.table(data$y, "data/y.csv", col.names=FALSE,row.names=FALSE, sep=",", append=F)
  write.table(data$x, "data/x.csv", col.names=FALSE,row.names=FALSE, sep=",", append=F)

  m <- reguralizedModel(data, 5, 10)
  write.table(m, sprintf("data/model_reg_%s.csv", 5), col.names=FALSE,row.names=FALSE, sep=",", append=F)
  m <- reguralizedModel(data, 0, 10)
  write.table(m, sprintf("data/model_reg_%s.csv", 0), col.names=FALSE,row.names=FALSE, sep=",", append=F)


  #m <- reguralizedModel(data, 0.1, 10)
  #plot(data$x, data$y)
  #lines(data$x, sin(data$x*pi), type="l")
  #lines(data$x, m, col="red", type="l")
}

buildModels()

task2ii <- function(){

  #Folds
  k <- 10

  #Sample the data
  data$id <- sample(1:k, nrow(data), replace=TRUE)
  list <- 1:k
  cv <- rep(NA, length(seq(0.1,10,by=0.1)))
  s <- 1
  for(lambda in seq(0.1,10,by=0.1)){
    prediction <- data.frame()
    testsetCopy <- data.frame()


    for(i in 1:k){
      #remove rows with id i from dataframe to create training set
      training_set <- subset(data, id %in% list[-i])
      val_set <- subset(data, id %in% c(i))

      temp <- as.data.frame(reguralizedModel(training_set, lambda, 10, val_set))
      prediction <- rbind(prediction, temp)

      testsetCopy <- rbind(testsetCopy, as.data.frame(val_set$y))
    }

    result <- cbind(prediction, testsetCopy[, 1])

    colnames(result) <- c("Predicted", "Actual")
    result$Difference <- (abs(result$Actual - result$Predicted))^2

    cv_error <- sum((result$Difference))/(nrow(result))
    cv[s] <- cv_error
    s <- s + 1
  }
  return(cv)
}

#res <- task2ii()
#write.table(res, sprintf("data/cv_error.csv"), col.names=FALSE,row.names=FALSE, sep=",")
#plot(seq(0.1,10,by=0.1), res, xlab="lambda", ylab="error", type="l")


#plot(legendre(data$x, 3), type="l")
#plot(data$x, data$y)
#lines(data$x, sin(pi*data$x))
