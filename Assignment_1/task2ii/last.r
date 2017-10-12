#library(orthopolynom)
#library(pracma)

l <- function(x,q,k){
      x**k * choose(q,k) * choose((q+k-1)/2,q)
    }

legendres <- function(x,q)
    {
    func <- rep(0,length(x))
        for(k in 0:q){
            func <- func + l(x,q,k)
        }
    func <- 2**q * func
    }

generateLegendre <- function(x, Qf, betas)
    {
        #betas <- generateUniformValues(Q_f+1)
        func <- rep(0,length(x))
        for(q in 0:Qf){
            func <- func + betas[q+1] * legendres(x, q)#(func / (betas[q] * 2**(q-1)) + l(x,q,q)
        }
        func
    }

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

difference <- function(
    x,
    betas10order,
    betas_g_fitted
    )
    {
        diff <- 0
        t <- betas_g_fitted

        if( length(betas10order) > length(betas_g_fitted) ){
          t <- rep(0, length(betas10order))
          t[1:length(betas_g_fitted)] <- betas_g_fitted

        }else if(length(betas10order) < length(betas_g_fitted)){
          t <- rep(0, length(betas_g_fitted))
          t[1:length(betas_g_fitted)] <- betas10order
        }

        for(i in 1:length(betas10order)){
            l <- legendres(x,i)
            diff <- diff + t[i] * l - betas10order[i] * l
        }
        diff^2
    }


calulateError <- function(del_N, del_Q, Q_POLY=10){
  beta_q <<- runif(40+1, min=-1, max=1)
  d_sizes <- seq(20, 60, del_N)
  s_sizes <- seq(1, 40, del_Q)
  row <- 1
  col <- 1
  error_matrix <-  matrix(0, length(d_sizes), length(s_sizes), byrow=T)

  for( size in d_sizes ){
    x <- runif(size, min=-1, max=1)
    x <- x[order(x)]
    col <- 1
    print(row)
    for( n_Q in s_sizes ){
      sigma <- 0.2

      y <- generateLegendre(x, n_Q, beta_q)
      y <- sapply(y, function(t) t + rnorm(1,0,sigma^2))

      data <- data.frame(x,y)

      m <- matrix(0, length(data$x), Q_POLY+1)
      for (i in 0:Q_POLY){
        m[,i+1] <- L_x_no_beta(data$x[order(data$x)], i)
      }

      m <- m[,2:ncol(m)]

      model <- lm(data$y ~ m)
      coeffs <- unname(coef(model))

      m_2 <- matrix(0, length(data$x), 2+1)
      for (i in 0:2){
        m_2[,i+1] <- L_x_no_beta(data$x[order(data$x)], i)
      }

      m_2 <- m[,2:ncol(m)]

      model_2 <- lm(data$y ~ m_2)
      coeffs_2 <- unname(coef(model_2))

      biasg2 <- integrate(difference,-1,1,beta_q,coeffs_2, rel.tol=0.1)
      biasg2 <- biasg2$value

      biasg10 <- integrate(difference,-1,1,beta_q,coeffs, rel.tol=0.1)
      biasg10 <- biasg10$value

      err_g10 <- biasg10 + sigma^2
      err_g2 <- biasg2 + sigma^2

      error_matrix[row, col] <- err_g10 - err_g2
      col <- col + 1
    }
    row <- row + 1
  }
  return(error_matrix)
}

a <- 100
for( i in 1:a) {
  cat(sprintf("%.2f\r", i/a))
  o <- sample(c(500:100000),1)
  write.table(calulateError(del_N=1, del_Q=1, Q_POLY=10), sprintf("matrix/matrix%d.csv", o), col.names=FALSE,row.names=FALSE, sep=",")
}
