rm(list=ls())
library(randomForest)
library(caret)

ntrees = 150
cvfolds = 10
intdepth = 2
set.seed(100)

data <- read.csv("Train_Digits_20171108.csv", header=TRUE)
data$Digit <- data$Digit%%2
data$Digit <- as.factor(data$Digit)

#Split to training and test set
sample <- sample(nrow(data)*0.8)
train <- data[sample,]
test <- data[-sample,]

#test <- read.csv("Test_Digits_20171108.csv", header=TRUE)
#test$Digit <- test$Digit%%2
#test$Digit <- as.factor(test$Digit)


nzr <- nearZeroVar(train[,-1],saveMetrics=T,freqCut=10000/1,uniqueCut=1/7)


cutvar <- rownames(nzr[nzr$nzv==TRUE,])
var <- setdiff(names(train),cutvar)
train <- train[,var]

rf <- randomForest(Digit ~ ., data=train, ntree=ntrees,importance=TRUE, na.action = na.exclude)

varImpPlot(rf, type=2, main="Random Forest Variable Importance Plot")

#write.csv(rf$mse, file=paste(paste("data/rf_mse", Sys.time(), sep=""), ".csv", sep=""))
#write.csv(rf$rsq, file=paste(paste("data/rf_rsq", Sys.time(), sep=""), ".csv", sep=""))

#write.csv(rf, file=paste(paste("data/rf_summary", Sys.time(), sep=""), ".csv", sep=""))

err <- data.frame(x = c(1:ntrees), error = rf$err.rate)
plot(rf$err.rate[,"OOB"],type="l", xlab="Number of trees", ylab="OOB error")

label_train <- train$Digit
train$Digit <- NULL
pred_train <- predict(rf, newdata=train)

label_test <- test$Digit
test <- test[,var[-1]]
pred_test <- predict(rf, newdata=test)
confusionMatrix(pred_test, label_test)

#write.table(rf$err.rate[,"OOB"], sprintf("data/bag_cv_error_%s_trees.csv", ntrees), col.names=FALSE,row.names=FALSE, sep=",")
#write.table(pred_test, sprintf("data/rf_prediction_%s_trees.csv", ntrees), col.names=FALSE,row.names=FALSE, sep=",")

