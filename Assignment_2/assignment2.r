

set.seed(11)
generateX <- function(size){
  x <- runif(size, min=-1, max=1)
  return( x[order(x)] )
}

targetFunction <- function(x){
  y <- sapply(0.8*x, function(t) t + rnorm(1,0,1))
  return(y)
}

g1 <- function(x){
  y <- lm(tf ~ 0 + x, offset=rep(0.5,length(x)))
  return(y)
}

g2 <- function(x){
  y <- lm(tf ~ 0 + x, offset=rep(-0.5,length(x)))
  return(y)
}

o <- function(x){
  y <- lm(tf ~ 0 + x)
  return(y)
}

task1i <- function(size){
  x <- generateX(size=size)
  y <- x * 0.8
  tf <<- targetFunction(x)
  plot(x, targetFunction(x=x))
  abline(o(x=x), col="red")
  abline(g1(x=x), col="green")
  abline(g2(x=x), col="blue")
}

# Run task 1i
#task1i(size=30)

calculateEval <- function(val, target_function, model){
  e_val <- rep(0, length(val))

  for( i in 1:4){
    e_val[i] <- (sum((target_function[1:i] - model[1:i])^2)) / i
  }

  for( i in 5:length(model)){
    e_val[i] <- (sum((target_function[1:i] - model[1:i])^2)) / i
  }

  return(e_val)
}


task2i <- function(size){
  x_tar <- generateX(size)
  target_function <- targetFunction(x_tar)
  best_model_error <- 1000
  best_model <- NULL
  e_val <- rep(0,9)

  for( i in 1:1 ){
    x <- generateX(size)
    train <- subset(x, x %in% x[c(5:25)])
    val  <- subset(x, !(x %in% train))

    g1_train <- lm(target_function[5:25] ~ 0 + train, offset=rep(0.5,length(train)))
    g2_train <- lm(target_function[5:25] ~ 0 + train, offset=rep(-0.5,length(train)))

    ord <- lm(target_function ~ 0 + x)

    newdata <- data.frame(train = val)
    g1_pred <- predict(g1_train, newdata=newdata)
    g2_pred <- predict(g2_train, newdata=newdata)

    mse_1 <- (sum((target_function[1:4] - g1_pred[1:4])^2) +
             sum((target_function[26:length(target_function)] - g1_pred[5:length(g1_pred)])^2)) / length(g1_pred)

    mse_2 <- (sum((target_function[1:4] - g2_pred[1:4])^2) +
             sum((target_function[26:length(target_function)] - g2_pred[5:length(g2_pred)])^2)) / length(g2_pred)


    best_model <- if (mse_1 < mse_2) g1_pred else g2_pred

    e_val <- e_val + calculateEval(val=val, target_function=target_function, model = best_model)


    #print(mse_1 - mse_2)
    plot(x_tar, target_function)
    abline(g1_train)
    abline(g2_train)
  }

  e_val <- e_val / i
  print(e_val)

}

task2i(30)
