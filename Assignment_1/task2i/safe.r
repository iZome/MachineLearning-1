library(pracma)
set.seed(111)

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


# l <- runif(110,min=-1, max=1)
# l <- l[order(l)]
# target_function <- generateLegendre(l, 10)


generateDatasets <- function(del_N, del_sigma, order){
  d_sizes <- seq(20, 100, del_N)
  s_sizes <- seq(0.2, 1.1, del_sigma)
  row <- 0
  col <- 0

  index <- 1
  datasets <- list()
  for( size in d_sizes ){

    x <- runif(size, min=-1, max=1)
    x <- x[order(x)]
    y <- generateLegendre(x, 10)
    dataset <- matrix(0, length(d_sizes)+1, length(x), byrow=T)
    row <- 1

    for ( sigma in s_sizes ) {
      dataset[row, ] <- sapply(generateLegendre(x, 10), function(t) t + rnorm(1,0,sigma^2))
      row <- row + 1
    }
    datasets[[index]] <- dataset
    index <- index + 1

    # model
    model <- datasets[[1]][1,]

    target_function <- y
    target_function <- as.data.frame(cbind(y, x))


    train <- sample(x, round(length(x)/2)+1)
    train <- train[order(train)]
    test <- x[!(x %in% train)]
    test <- test[order(test)]

    train <- as.data.frame(cbind(model[which(x %in% train)], train))
    test <- as.data.frame(cbind(model[which(x %in% train)], test))
    #print(target_function)
    #print(train)
    fitted_model <- lm(train$V1 ~ poly(train$train, 10))
    predict <- predict(fitted_model, test[,-1])
    plot(predict)
    plot(y)
    #predict(lin_reg_model, test_set[,-1])
    #lm(training_set$Hazard ~., data=training_set)

    quit()


  }
  return (datasets)
}

datasets <- generateDatasets(10,0.1, 10)


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



'''

m <- matrix(0, length(x), Qf)

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

for (i in 1:Qf){
  m[,i] <- L_x_no_beta(x, i)
}

nrow(m)
quit()

'''
