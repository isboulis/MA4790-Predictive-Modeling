##Chapter 12##
##12.1##
##C##
library(caret)
library(glmnet)
library(pamr)
library(AppliedPredictiveModeling)
data(hepatic)
nzv = nearZeroVar(bio)
bio = bio[,-nzv]
damage = as.character( injury )
damage[ damage=="Mild" ] = "yes"
damage[ damage=="Severe" ] = "yes"
damage[ damage=="None" ] = "none"
table( damage )
partition = createDataPartition(damage, p=4/5, list=FALSE)
damage <- factor(damage, levels=c("yes","none"))
ctrlLR <- trainControl(method="boot", 25)
##Logistic Regression##
LR <- train(bio, damage, method="glm", preProc=c("center","scale"), trControl=ctrlLR)
LR
confusionMatrix(LR$pred$pred, LR$pred$obs)
LRcon <- confusionMatrix(data = LR$pred$pred, reference = LR$pred$obs)
##Linear Discriminant Analysis##
ctrlLDA <- trainControl(method="LGOCV")
LDA <- train(bio, damage, method="lda", preProc=c("center","scale"), trControl=ctrlLDA)
LDA
##PArtial Least Squares Discriminant Analysis
ctrlPLSDA <- trainControl(summaryFunction = twoClassSummary,
                     classProbs = TRUE)
PLSDA <- train(bio, damage, method = "pls", tuneGrid = expand.grid(.ncomp = 1:4), preProc = c("center","scale"), metric = "ROC", trControl = ctrlPLSDA)
PLSDA
plot(PLSDA)
##Penalized Model##
ctrlPM <- trainControl(method = "LGOCV", summaryFunction = twoClassSummary, classProbs = TRUE)
PMGrid <- expand.grid(.alpha = c(0, .1, .2, .4, .6, .8, 1), .lambda = seq(.01, .2, length = 10))
PM <- train(bio, damage, method = "glmnet", tuneGrid = PMGrid, preProc = c("center", "scale"), metric = "ROC", trControl = ctrlPM)
PM
plot(PM)
##Nearest Shrunken Centroids##
nscGrid <- data.frame(.threshold = seq(0,4, by=0.1))
NSC <- train(bio, damage, method = "pam", preProc = c("center", "scale"), tuneGrid = nscGrid, metric = "ROC", trControl = ctrlPLSDA)
NSC
plot(NSC)
##D##
varImp(LDA)
