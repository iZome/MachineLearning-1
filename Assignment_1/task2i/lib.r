#library(orthopolynom)
#library(pracma)

#set.seed(111)


#N = 50*0.6
#Qf = 10
#Q_POLY = 10
#x <- 1:N

#set.seed(111)
#x <- runif(N/0.6, min=-1, max=1)
#x <- x[order(x)]

l <- function(
    x,q,k
    ){
      x**k * choose(q,k) * choose((q+k-1)/2,q)
    }

legendres <- function(
    x,q
    )
    {
    func <- rep(0,length(x))
        for(k in 0:q){
            func <- func + l(x,q,k)
        }
    func <- 2**q * func
    }

generateLegendre <- function(
    x,
    Qf,
    betas
    )
    {
        #betas <- generateUniformValues(Q_f+1)
        func <- rep(0,length(x))
        for(q in 0:Qf){
            func <- func + betas[q+1] * legendres(x, q)#(func / (betas[q] * 2**(q-1)) + l(x,q,q)
        }
        func
    }

#y <- generateLegendre(x, Qf, beta_q)

#train <- sample(x, length(x)*0.6)
#test <- x[!(x %in% train)]

#y <- sapply(y, function(t) t + rnorm(1,0,0.2^2))


#train <- as.data.frame(cbind(train, y[which(x %in% train)]))
#test <- as.data.frame(cbind(test, y[which(x %in% test)]))

#colnames(test) <- c("x", "y")
#colnames(train) <- c("x", "y")


#m <- matrix(0, length(train$x), Q_POLY+1)

L_no_beta <- function( q, k ){
  choose(q, k) *
  choose((q+k-1)/2, q)
}

L_x_no_beta <- function(x,q){
  s <- rep(0, length(x))
  for (k in 0:q){
    s <- s + x^k * L_no_beta( q=q, k=k )
  }
  return (2^q * s)
}

#for (i in 0:Q_POLY){
#  m[,i+1] <- L_x_no_beta(train$x[order(train$x)], i)
#}

#m <- m[,2:ncol(m)]

#model <- lm(train$y ~ m)
#coeffs <- unname(coef(model))
#pred <- generateLegendre(test$x, Q_POLY, coeffs)

#print(sum(pred - test$y)^2)

#plot(x,y, type="b")
#lines(test$x, pred, col='red')



calulateError <- function(del_N, del_sigma, order, Q_POLY=10){
  beta_q <<- runif(order+1, min=-1, max=1)
  d_sizes <- seq(20, 110, del_N)
  s_sizes <- seq(0.2, 1.1, del_sigma)
  row <- 1
  col <- 1
  error_matrix <-  matrix(0, length(d_sizes), length(s_sizes), byrow=T)

  for( size in d_sizes ){
    x <- runif(size, min=-1, max=1)
    x <- x[order(x)]
    col <- 1
    for( sigma in s_sizes ){

      y <- generateLegendre(x, order, beta_q)

      train <- sample(x, length(x)*0.6)
      test <- x[!(x %in% train)]

      y <- sapply(y, function(t) t + rnorm(1,0,sigma^2))

      train <- as.data.frame(cbind(train, y[which(x %in% train)]))
      test <- as.data.frame(cbind(test, y[which(x %in% test)]))

      colnames(test) <- c("x", "y")
      colnames(train) <- c("x", "y")


      m <- matrix(0, length(train$x), Q_POLY+1)
      for (i in 0:Q_POLY){
        m[,i+1] <- L_x_no_beta(train$x[order(train$x)], i)
      }

      m <- m[,2:ncol(m)]

      model <- lm(train$y ~ m)
      coeffs <- unname(coef(model))
      pred <- generateLegendre(test$x, Q_POLY, coeffs)


      m_2 <- matrix(0, length(train$x), 2+1)
      for (i in 0:2){
        m_2[,i+1] <- L_x_no_beta(train$x[order(train$x)], i)
      }

      m_2 <- m[,2:ncol(m)]

      model_2 <- lm(train$y ~ m_2)
      coeffs_2 <- unname(coef(model_2))
      pred_2 <- generateLegendre(test$x, 2, coeffs)

      plot(x,y, type="b")
      lines(test$x, pred, col='red')
      lines(test$x, pred_2, col="blue")

      err_10 <- sum(pred - test$y)^2 / length(pred)
      err_2 <- sum(pred_2 - test$y)^2 / length(pred_2)

      error_matrix[row, col] <- err_10 - err_2
      col <- col + 1
    }
    row <- row + 1
  }
  return(error_matrix)
}

for( i in 1:100) {
  cat(sprintf("%.2f\r", i/100))
  #calulateError(del_N=1, del_sigma=0.01, order=10, Q_POLY=10)
  write.table(calulateError(del_N=1, del_sigma=0.001, order=10, Q_POLY=10), sprintf("matrix_1/matrix%d.csv", i), col.names=FALSE,row.names=FALSE, sep=",")
}
