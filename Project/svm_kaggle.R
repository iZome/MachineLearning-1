library(readr)
library(caret)

data <- read.csv("Train_Digits_20171108.csv")
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

label <- as.factor(train[[1]])
train$Digit <- NULL
train <- train/255
covtrain <- cov(train)

train_pc <- prcomp(covtrain)
varex <- train_pc$sdev^2/sum(train_pc$sdev^2)
varcum <- cumsum(varex)
result <- data.frame(num=1:length(train_pc$sdev),
                     ex=varex,
                     cum=varcum)

plot(result$num,result$cum,type="b",xlim=c(0,100),
     main="Variance Explained by Top 100 Components",
     xlab="Number of Components",ylab="Variance Explained")
abline(v=25,lty=2)

train_score <- as.matrix(train) %*% train_pc$rotation[,1:8]
train <- cbind(label,as.data.frame(train_score))

colors <- rainbow(length(unique(train$label)))
names(colors) <- unique(train$label)
plot(train$PC1,train$PC2,type="n",main="First Two Principal Components")
text(train$PC1,train$PC2,label=train$label,col=colors[train$label])

svm_mdl <- train(label~.,data=train,
                 method="svmRadial",
                 trControl=trainControl(method="cv",
                                        number=10),
                 tuneGrid=data.frame(sigma = 0.01104614,
                                     C = 3.5))
svm_mdl

pred <- predict(svm_mdl$finalModel,train_score,type="response")
prediction <- data.frame(ImageId=1:nrow(train),Label=pred)
confusionMatrix(pred, label)

label_test <- test$Digit
test <- test[,var[-1]]/255
test <- as.matrix(test) %*% train_pc$rotation[,1:8]
test <- as.data.frame(test)

pred <- predict(svm_mdl$finalModel,test,type="response")
prediction <- data.frame(ImageId=1:nrow(test),Label=pred)
confusionMatrix(pred, label_test)
