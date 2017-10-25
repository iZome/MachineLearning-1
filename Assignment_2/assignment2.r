

set.seed(420)

targetFunction <- function(size){
  x <- runif(size, min=-1, max=1)
  x <- x[order(x)]
  y <- sapply(0.8*x, function(t) t + rnorm(1,0,1))

  return(data.frame(x=x, y=y))
}

g1 <- function(data){
  y <- lm(I(y - (-0.5)) ~ 0 + x, data=data)
  return(y)
}

g2 <- function(data){
  y <- lm(I(y - 0.5) ~ 0 + x, data=data)
  return(y)
}

o <- function(data){
  y <- lm(y ~ 0 + x, data=data)
  return(y)
}

task1i <- function(size){
  tf <- targetFunction(30)

  plot(tf$x, tf$y)
  abline(0.5, coef(g1(tf)), col="red")
  abline(-0.5, coef(g2(tf)), col="green")
  abline(o(tf), col="black")
}


# Run task 1i
#task1i(size=30)

fdiff <- function(x, coef_underlying, coef_bestModel, offset){
  return((0-offset + (coef_bestModel - coef_underlying)*x)^2)
}

task2i <- function(size){

  e_out <- rep(0,21)
  e_val <- rep(0,21)

  for( index in 1:100 ){
    target_function <- targetFunction(size)

    for(i in 5:25){
      # Split to train and validation set
      train <- subset(target_function, target_function$x %in% target_function$x[c((nrow(target_function) - i):5)])
      val  <- subset(target_function, !(target_function$x %in% train$x))

      # Train a linear model
      g1_train <- g1(train)
      g2_train <- g2(train)
      underlying_function <- lm(y ~ 0 + x, data=target_function)

      # Predict
      newdata <- data.frame(x = c(val$x))
      g1_pred <- predict(g1_train, newdata=newdata)
      g2_pred <- predict(g2_train, newdata=newdata)

      mse_1 <- mean((g1_pred - val$y)^2)
      mse_2 <- mean((g2_pred - val$y)^2)

      # Best model
      best_model <- if(mse_1 < mse_2) g1_train else g2_train
      offset <- if(mse_1 < mse_2) 0.5 else -0.5

      eOut <- function(x){
        fdiff(x, coef(underlying_function), coef(best_model), offset=offset)
      }

      # Append e_val
      if(mse_1 < mse_2) e_val[i-4] = e_val[i-4] + mse_1 else e_val[i-4] = e_val[i-4] + mse_2

      # Append e_out
      e_out[i-4] <- e_val[i-4] + integrate(eOut, -1, 1)$value + 1

    }

  }
  e_val <- e_val / index
  e_out <- e_out / index

  return(list(e_val, e_out))
}

res <- task2i(30)
plot(res[[1]], type="l", ylim=c(0,2))
lines(res[[2]], type="l", col="red")

#warnings()
