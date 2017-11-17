rm(list=ls())
library(gbm)
library(caret)

data <- read.csv("Train_Digits_20171108.csv", header=TRUE)
data$Digit <- data$Digit%%2
data$Digit <- as.factor(data$Digit)

#Split to training and test set
sample <- sample(nrow(data)*0.8)
train <- data[sample,]
test <- data[-sample,]

nzr <- nearZeroVar(train[,-1],saveMetrics=T,freqCut=10000/1,uniqueCut=1/7)
cutvar <- rownames(nzr[nzr$nzv==TRUE,])
var <- setdiff(names(train),cutvar)
train <- train[,var]

ntrees = 15000
cvfolds = 10
intdepth = 2

boost <- gbm(train$Digit ~., data=train, distribution="multinomial", n.trees=ntrees, interaction.depth=intdepth, shrinkage=0.001, bag.fraction=1, cv.folds=cvfolds, n.cores=4)

mean(boost$cv.error)

plot(boost$cv.error, type="l", ylab="CV error", xlab="Number of trees", main="Boosted Model")

label_test <- test$Digit
test <- test[,var[-1]]
pred_test <- predict(boost, newdata=test, type="response")
prediction <- data.frame(ImageId=1:nrow(test),Label=pred_test)
prediction$est <- as.numeric(prediction[,2] < 0.5)
print(confusionMatrix(prediction$est, label_test))

makeConfusionMatrix <- function(pred, true){
  mod <- confusionMatrix(pred,true)
  confM <- mod$table
  confM <- rbind(confM, Totals = rowSums(confM))
  sums <- rowSums(confM)
  confM <- cbind(confM, Error = 0)
  for (row in 1:(nrow(confM)-1)){
    correct <- confM[row,row]
    wrong <- sums[row] - confM[row,row]
    confM[row,"Error"] = round(wrong/(correct+wrong),5)
  }
  confM[nrow(confM), "Error"] <- 1 - mod$overall["Accuracy"]
  return(confM)
}

write.table(boost$cv.error, sprintf("data/boosting_cv_error_%s_trees.csv", ntrees), col.names=FALSE,row.names=FALSE, sep=",")
write.table(mean(boost$cv.error), sprintf("data/boosting_cv_%s_mean.csv", ntrees), col.names=FALSE,row.names=FALSE, sep=",")
write.table(prediction$est, sprintf("data/boosting_prediction_%s_trees.csv", ntrees), col.names=FALSE,row.names=FALSE, sep=",")
write.table(makeConfusionMatrix(prediction$est, label_test), sprintf("data/boosting_confusion_matrix_%s_trees.csv", ntrees), col.names=FALSE,row.names=FALSE, sep=",")


#label_train <- train$Digit
#train$Digit <- NULL
#pred_train <- predict(boost, newdata=train)
