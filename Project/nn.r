rm(list=ls())
library(h2o)

set.seed(111)

localH2O = h2o.init(nthreads=-1,            ## -1: use all available threads
                    max_mem_size = "2G")
h2o.removeAll()

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
test <- test[,var]

# To H2O
df_train <- as.h2o(train)
df_test <- as.h2o(test)

# Declare hyper params
hyper_params <- list(
  hidden = list(c(512, 128), c(212, 42), c(32, 32)),
  epochs = c(10),
  rate = c(0.005),
  input_dropout_ratio = c(0,0.1), 
  stopping_rounds = c(3),
  stopping_metric = "misclassification", # could be "MSE","logloss","r2"
  stopping_tolerance = c(0.01, 0.02)
)


# Create grid
grid <- h2o.grid(
  algorithm="deeplearning",
  x=2:length(df_train[1,]),
  y=1,
  training_frame=df_train,
  nfolds=5,
  seed=111,
  hyper_params=hyper_params
)

# Get grid
grid <- h2o.getGrid(grid@grid_id, sort_by="mean_per_class_error", decreasing=F)
summary(grid)


best_model <- h2o.getModel(grid@model_ids[[1]])
best_model

#Test the best model on new data
print(h2o.performance(best_model, newdata = df_test))

plot(grid@summary_table$mean_per_class_error)
write.table(as.numeric(grid@summary_table$mse), sprintf("data/mse_grid_nn.csv"), col.names=FALSE,row.names=FALSE, sep=",")


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
