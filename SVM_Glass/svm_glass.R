https://cran.r-project.org/web/packages/e1071/vignettes/svmdoc.pdf

library(e1071)
library(rpart)
data(Glass, package="mlbench")

nrow(Glass)
dim(Glass)
summary(Glass)
str(Glass)
names(Glass)

## split data into a train and test set
index <- 1:nrow(Glass)
testindex <- sample(index, trunc(length(index)/3))
testset <- Glass[testindex,]
trainset <- Glass[-testindex,]

## svm
svm.model <- svm(Type ~ ., data = trainset, cost = 100, gamma = 1)
svm.pred <- predict(svm.model, testset[,-10])

## compute svm confusion matrix
table(pred = svm.pred, true = testset[,10])

(
