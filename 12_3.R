##12.3##
install.packages("modeldata")
library(modeldata)
data(mlc_churn)
library(caret)
library(AppliedPredictiveModeling)
library(pROC)
churn <- mlc_churn
table(churn$churn)
plot(churn)
train <- churn[,-c(1,3,4,5)]
test <- churn[,c(1,3,4,5)]
y <- train[,16]
y <- data.frame(y)
x <- train[,-16]
x <- x[,-nearZeroVar(x)]
partition <- createDataPartition(churn$churn, p=4/5, list=FALSE)
trP <- x[partition,]
trR <- y[partition]



##Logistic Regression##
ctrl=trainControl(summaryFunction=twoClassSummary, classProbs=TRUE)
LR <- train(trP, trR, method="glm", preProc=c("center","scale"), trControl=ctrl)
LR
confusionMatrix(LR$pred$pred, LR$pred$obs)
##Linear Discriminant Analysis##
LDA <- train(trP, trR, method="lda", preProc=c("center","scale"),metric="ROC", trControl=ctrl)
LDA
plot(LDA)
##Partial LEast Squares Discriminant Analysis##
PLSDA <- train(trP, trR, method="pls", tuneGrid=expand.grid(.ncomp=1:10), preProc=c("center","scale"), metric="ROC", trControl=ctrl)
PLSDA
plot(PLSDA)
##Penalized##
grid = expand.grid(.alpha=c(0, 0.2, 0.4, 0.6, 0.8, 1.0), .lambda=seq( 0.01, 0.2, length=20))
PM <- train(trP, trR, method="glmnet", tuneGrid=grid, preProc=c("center","scale"), metric="ROC", trControl=ctrl)
PM
plot(PM)
##Nearest Shrunken Centroids##
NGrid = expand.grid(.threshold=0:25)
NSC <- train(trP, trR, method="pam", tuneGrid=NGrid, preProc=c("center","scale"), metric="ROC", trControl=ctrl)
NSC
plot(NSC)
