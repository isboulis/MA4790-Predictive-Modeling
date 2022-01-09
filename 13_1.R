##13.1##
library(caret)
library(glmnet)
library(pamr)
library(AppliedPredictiveModeling)
library(mda)
data(hepatic)
nzv = nearZeroVar(bio)
bio = bio[,-nzv]
damage = as.character( injury )
damage[ damage=="Mild" ] = "yes"
damage[ damage=="Severe" ] = "yes"
damage[ damage=="None" ] = "none"
damage = factor(damage, levels=c("yes","none"))
ctrl = trainControl(summaryFunction=twoClassSummary, classProbs=TRUE)
##MDA##
mda=train(bio, damage, method="mda", tuneGrid=expand.grid(subclasses=1:3), metric="ROC", trControl=ctrl)
mdapred = predict(mda, bio, type="prob")
mdaroc = pROC::roc( response=damage, predictor=mdapred[,1] )
##Neural Network##
nngrid = expand.grid(size=1:3, decay =c(0,0.1,1,2))
nnet = train(bio, damage, method="nnet", preProcess=c("center","scale","spatialSign"), tuneGrid=nngrid, metric="ROC", trControl=ctrl)
nnetpred = predict(nnet, bio, type="prob")
nnetroc = pROC::roc(damage, nnetpred[,1])
##SVM##
svm = train(bio, damage, method="svmRadial", preProc=c("center","scale"), metric="ROC", trControl=ctrl )
svmpred = predict( svm, bio, type="prob" )
svmroc = pROC::roc(damage, svmpred[,1] )
##KNN##
knn = train(bio, damage, method="knn", tuneLength=20, preProc=c("center","scale"), metric="ROC", trControl=ctrl )
knnpred = predict(knn, bio, type="prob" )
knnroc = pROC::roc(damage, knnpred[,1] )
##Naive Bayes##
bayes = train(bio, damage, method="nb", metric="ROC", trControl=ctrl ) 
bayespred = predict(bayes, bio, type="prob" )
bayesroc = pROC::roc(damage, bayespred[,1] )
##C##
varImp(svm)





