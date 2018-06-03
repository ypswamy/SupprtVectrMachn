http://rstatistics.net/support-vector-machines/

library(e1071)
library(MASS)
data(cats)

dim(cats)
nrow(cats)
summary(cats)
str(cats)
names(cats)

# response as factor
inputData <- data.frame(cats[, c (2,3)], response = as.factor(cats$Sex)) 

# linear SVM

# linear svm, scaling turned OFF
svmfit <- svm(response ~ ., data = inputData, kernel = "linear", cost = 10, scale = FALSE) 

print(svmfit)

plot(svmfit, inputData)

compareTable <- table (inputData$response, predict(svmfit))  # tabulate

mean(inputData$response != predict(svmfit)) # 19.44% misclassification error

# radial SVM
# radial svm, scaling turned OFF

svmfit <- svm(response ~ ., data = inputData, kernel = "radial", cost = 10, scale = FALSE) 

print(svmfit)
plot(svmfit, inputData)
compareTable <- table (inputData$response, predict(svmfit))  # tabulate
mean(inputData$response != predict(svmfit)) # 18.75% misclassification error

### Tuning
# Prepare training and test data

set.seed(100) # for reproducing results
rowIndices <- 1 : nrow(inputData) # prepare row indices
sampleSize <- 0.8 * length(rowIndices) # training sample size
trainingRows <- sample (rowIndices, sampleSize) # random sampling
trainingData <- inputData[trainingRows, ] # training data
testData <- inputData[-trainingRows, ] # test data
tuned <- tune.svm(response ~., data = trainingData, gamma = 10^(-6:-1), cost = 10^(1:2)) 

# tune
summary (tuned) # to select best gamma and cost

#Turns out cost value of 100 and a gamma value of 0.001 yields the least error. Lets fit #a radial SVM with these parameters.

# radial svm, scaling turned OFF
svmfit <- svm (response ~ ., data = trainingData, kernel = "radial", cost = 100, gamma=0.001, scale = FALSE) 

print(svmfit)

plot(svmfit, trainingData)

compareTable <- table (testData$response, predict(svmfit, testData))  # comparison table

mean(testData$response != predict(svmfit, testData)) # 13.79% misclassification error


