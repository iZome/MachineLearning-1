data <- read.csv("Train_Digits_20171108.csv")
mm1 <- as.matrix(data)
mm2 <- matrix(mm1, ncol = ncol(data), dimnames = NULL)
mm2 <- mm2[,-1]
pca <- prcomp(cov(mm2))
plot(summary(pca)$importance[3,])
