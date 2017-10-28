
# Set seed for repeatability
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

task1i <- function(size){
  N <- 20000
  rel <- rep(0,N)

  coef_1 <- rep(0,N)
  coef_2 <- rep(0,N)

  diff1 <- function(x, coef_underlying=0.8, coef_est, offset){
    return((0-offset + (coef_est - coef_underlying)*x)^2)
  }

  for(i in 2:N){
    tf <- targetFunction(30)
    g1 <- coef(g1(tf))
    g2 <- coef(g2(tf))

    coef_1[i] <- g1
    coef_2[i] <- g2

    eOut_1 <- function(x){
      diff1(x, coef_underlying=0.8, coef_est=g1, offset=0.5)
    }

    eOut_2 <- function(x){
      diff1(x, coef_underlying=0.8, coef_est=g2, offset=0.5)
    }

    biassq1 <- integrate(eOut_1, -1, 1)$value + 1
    biassq2 <- integrate(eOut_2, -1, 1)$value + 1
    rel[i] <- rel[i-1] + (biassq2 - biassq1)/N

  }
}


# Run task 1i
task1i(size=30)

fdiff <- function(x, coef_underlying, coef_bestModel, offset){
  return((0-offset + (coef_bestModel - coef_underlying)*x)^2)
}

task2i <- function(size){

  e_out <- rep(0,21)
  e_val <- rep(0,21)

  for( index in 1:10000 ){
    target_function <- targetFunction(size)

    for(i in 5:25){

      # Split to train and validation set
      train <- subset(target_function, target_function$x %in% target_function$x[sample(seq_len(nrow(target_function)),  30 - i)])
      val  <- subset(target_function, !(target_function$x %in% train$x))

      # Train a linear model
      g1_train = g1(train)
      g2_train = g2(train)

      # Predict
      newdata <- data.frame(x = c(val$x))
      g1_pred <- predict(g1_train, newdata=newdata)
      g2_pred <- predict(g2_train, newdata=newdata)

      mse_1 <- mean((g1_pred - val$y)^2)
      mse_2 <- mean((g2_pred - val$y)^2)

      # Best model
      best_model <- if(mse_1 < mse_2) g1_train else g2_train

      eOut <- function(x){
        fdiff(x, 0.8, coef(best_model), offset=0.5)
      }

      # Append e_val
      if(mse_1 < mse_2) e_val[i-4] <- e_val[i-4] + mse_1 else e_val[i-4] <- e_val[i-4] + mse_2

      # Append e_out
      e_out[i-4] <- e_out[i-4] + integrate(eOut, -1, 1)$value + 1

    }

  }
  e_val <- e_val / index
  e_out <- e_out / index

  write.table(e_val, sprintf("eval.csv"), col.names=FALSE,row.names=FALSE, sep=",")
  write.table(e_out, sprintf("eout.csv"), col.names=FALSE,row.names=FALSE, sep=",")
}

task2i(30)
