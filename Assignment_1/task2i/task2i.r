#library(pracma)
#set.seed(111)

L <- function( q, k ){
  choose(q, k) *
  choose((q+k-1)/2, q)
}

L_x <- function(x,q){
  s <- rep(0, length(x))
  beta_q <- runif(q+1, min=-1, max=1)
  for (k in 0:q){
    s <- s + x^k * L( q=q, k=k ) * beta_q[k+1]
  }
  return (2^q * s)
}

# params: linspace
generateLegendre <- function(lspace, q){
  return(L_x(x=lspace, q))
}

#######################
# Halvor
######################

legendre_polynomial <- function(x, q){
 result = 0; # initialize variable

 for (k in 0:q){
   result = result + x^k * choose(q,k) * choose(((q + k - 1)/2), q)
 }

 result  = result * 2^q;
}

data_Qf = 10
betas = runif(data_Qf + 1, min=-1,max=1)

func3_with_err <- function(x, max_q, sigma, betas){
 result = 0;
 errors <- rnorm(length(x), mean = 0, sd = sigma^2)

 for (q in 0:max_q){
   beta_q <- betas[q+1]
   L_q <- legendre_polynomial(x, q)
   result = result + beta_q * L_q
 }
 return (result + errors)
}

##########################
##########################

# l <- runif(110,min=-1, max=1)
# l <- l[order(l)]
# target_function <- func3_with_err(l, 10, 0.2, betas)
# plot(target_function, type="l")
#
# quit()


generateDatasets <- function(del_N, del_sigma, order){
  d_sizes <- seq(22, 100, del_N)
  s_sizes <- seq(0.2, 1.1, del_sigma)
  row <- 0
  col <- 0

  index <- 1
  datasets <- list()
  row <- 1
  error_matrix <-  matrix(0, length(d_sizes), length(s_sizes), byrow=T)
  for( size in d_sizes ){

    x <- runif(size, min=-1, max=1)
    x <- x[order(x)]
    #y <- func3_with_err(x, order, 0, betas)
    dataset <- matrix(0, length(d_sizes)+1, length(x), byrow=T)
    col <- 1
    for ( sigma in s_sizes ) {
      #model <- sapply(y, function(t) t + rnorm(1,0,sigma^2))

      target_function <- func3_with_err(x, 10, sigma, betas)
      target_function <- as.data.frame(cbind(x,target_function))
      colnames(target_function) <- c("x", "y")
      #quit()
      train <- sample(x, (length(x)*0.8))
      #train <- train[order(train)]
      test <- x[!(x %in% train)]
      #test <- test[order(test)]

      train <- as.data.frame(cbind(train, target_function$y[which(x %in% train)]))
      test <- as.data.frame(cbind(test, target_function$y[which(x %in% test)]))
      #quit()

      colnames(test) <- c("x", "y")
      colnames(train) <- c("x", "y")

      fitted_model <- lm(y ~ poly(x, order), data=train)
      test_pred <- data.frame(x = test$x)
      predicted <- as.data.frame(predict(fitted_model, newdata=test_pred))
      colnames(predicted) <- "Predicted"

      error <- sum(abs(test$y - predicted$Predicted))
      error_matrix[row, col] <- error

      col <- col + 1

      plot(predicted$Predicted, type="l")

    }
    row <- row + 1
  }
  return (error_matrix)
}

#data <- generateDatasets(2,0.1, 10)

#generateDatasets(2,0.1, 10)
for( i in 1:1) {
  cat(sprintf("%.2f\r", i/100))
  write.table(generateDatasets(1,0.005, 10), sprintf("matrix%d.csv", i), col.names=FALSE,row.names=FALSE, sep=",")
}
#warnings()


#fitted_model <- lm(target_function, train)


 # l <- runif(20000, min=-1, max=1)
 # l <- l[order(l)]
 # plot(l,generateLegendre(l, 10), type="l")


# e <- function(){
#     n = 60
#     error_matrix <- matrix(0, n-19, 90, byrow=TRUE)
#     varlinspace <- linspace(0.2, 1.1, 90)
#
#     for( N in 20:n ){
#       x <- runif(N, min=-1, max=1)
#       x <- x[order(x)]
#       analytical_legendre <- generateLegendre(lspace=x, q=10)
#
#       k <- 1
#       for( sd in varlinspace ){
#
#         # Estimate and add noise
#         estimated_legendre_10 <- sapply(generateLegendre(lspace=x, q=10), function(t) t + rnorm(1,0,sd^2))
#         estimated_legendre_2 <- sapply(generateLegendre(lspace=x, q=2), function(t) t + rnorm(1,0,sd^2))
#         error_matrix[N-19, k] <- abs(sum(analytical_legendre - estimated_legendre_10)^2 - sum(analytical_legendre - estimated_legendre_2)^2)
#         #error_matrix[N-19, k] <- (sum(estimated_legendre_10 - analytical_legendre)^2)/N
#
#         k <- k + 1
#         plot(x,estimated_legendre_10, type="l")
#         lines(x,analytical_legendre, col="red")
#         lines(x,estimated_legendre_2, col="green")
#         legend("topright", legend=c("10", "analytical", "2"), col=c("black", "red", "green"), lwd=3)
#       }
#       }
#
#     return (error_matrix)
#   }

#e_10 <- e()
#e_2 <- generateXYpair(Q=2)

#generateXYpair()
#write.table(e_10, "matrix.csv", col.names=FALSE,row.names=FALSE, sep=",")

# generateDatasets <- function(){
#   set.seed(111)
#   datasets <- list()
#   for (i in 1:11){
#     datasets[[i]] <- generateXYpair(N=i+19, q=10)
#   }
#   return (datasets)
# }
#
# set.seed(111)
#model <- generateDatasets()[[10]][,10]


#generateDatasets()[[1]]
#plot(unlist(generateDatasets()[[1]]))

# # Legendre
# f2 <- function(x,q){
#   tmp <- 0
#   for( i in 0:q ){
#     tmp <- tmp + beta_q[i+1] * L_x(x=x,q=i)
#   }
#   tmp
# }
# generateLegendre10 <- function(){
#   beta_q <<- runif(11, min=-1, max=1)
#   sol <- rep(0,100)
#   for ( i in 1:length(sol) ){
#     sol[i] <- f2(lspace[i], 10)
#   }
#   return(sol)
# }
#
# a <- generateLegendre10()
# plot(lspace, a, type="l", ylab="$L_{1}$", xlab="$x$", main="alpha")
