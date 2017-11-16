library(doParallel)
set.seed(100)

data <- read.csv("Train_Digits_20171108.csv", header=TRUE)
data$Digit <- data$Digit%%2
data$Digit <- as.factor(data$Digit)

k <- 10 #Folds

#Sample the data
data$id <- sample(1:k, nrow(data), replace=TRUE)
list <- 1:k

prediction <- data.frame()
testsetCopy <- data.frame()

#Parallelize
cl <- makeCluster(4, outfile="")
registerDoParallel(cl)

n_runs <- k
res <- foreach(i = 1:n_runs, .combine=rbind) %do% {
  
  #remove rows with id i from dataframe to create training set
  #select rows with id i to create test set
  training_set <- subset(data, id %in% list[-i])
  test_set <- subset(data, id %in% c(i))
  
  training_set <- data.frame(y = training_set$Digit, x = training_set[, 2:ncol(training_set)])
  
  #run a linear regression model
  lin_reg_model <- lm(y ~., data=training_set)
  model_summary <- summary(lin_reg_model)
  
  #remove response column 1, Hazard
  prediction <- as.data.frame(predict(lin_reg_model, test_set[,-1]))
  
  result <- cbind(prediction, as.data.frame(test_set[,1]))
  result
}

stopCluster(cl)

colnames(res) <- c("Predicted", "Actual")
head(res)
res$Difference <- (abs(res$Actual - res$Predicted))^2

cv_error <- sum((res$Difference))/(length(res$Difference))
cv_error
