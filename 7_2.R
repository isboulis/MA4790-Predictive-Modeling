##7.2##
library(mlbench)
library(caret)
set.seed(200)
trainingData <- mlbench.friedman1(200, sd = 1)
trainingData$x <- data.frame(trainingData$x)
featurePlot(trainingData$x, trainingData$y)
testData <- mlbench.friedman1(5000, sd = 1)
testData$x <- data.frame(testData$x)
knnModel <- train(x=trainingData$x, y=trainingData$y, method="knn", preProc=c("center","scale"), tuneLength = 10)
knnModel
knnPred <- predict(knnModel, newdata=testData$x)
postResample(pred=knnPred, obs=testData$y)

##A##
mars <- train(x=trainingData$x, y=trainingData$y, method = "earth", preProc = c("center","scale"), tuneLength=10)
mPred <- predict(mars, newdata = testData$x)
postResample(pred=mPred, obs = testData$y)

##B##
varImp(mars)
