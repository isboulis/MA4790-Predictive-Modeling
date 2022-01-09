##6.3##
##A##
library(AppliedPredictiveModeling)
library(caret)
library(RANN)
data("ChemicalManufacturingProcess")

##B##
response <- subset(ChemicalManufacturingProcess, select="Yield")
predict <- subset(ChemicalManufacturingProcess, select=-Yield)
partition <- createDataPartition(response$Yield, p=4/5, list=FALSE)
trp <- predict[partition,]
trr <- response[partition,]
tep <- predict[-partition,]
ter <- response[-partition,]
impute <- preProcess(trp, method=c("center","scale","BoxCox","knnImpute"))
impute

##c##D##E##
trainP <- predict(impute,trp)
nzv <- nearZeroVar(trainP)
trainP <- trainP[-nzv]
corr <- cor(trainP)
hc <- findCorrelation(corr)
trainP <- trainP[, -hc]

timpute <- preProcess(tep, method=c("BoxCox","center","scale","knnImpute"))
tep <- predict(timpute,tep)
nzv <- nearZeroVar(tep)
tep <- tep[-nzv]
corr <- cor(tep)
hc <- findCorrelation(corr)
tep <- tep[, -hc]


ctrl <- trainControl(method="cv", 3)
lm <- train(x=trainP, y=trr, method="lm", trControl=ctrl) 
ridge <- train(x=trainP, y=trr, method="ridge", trControl=ctrl)
lasso <- train(x=trainP, y=trr, method="lasso", trControl=ctrl)
enet <- train(x=trainP, y=trr, method="enet", trControl=ctrl)
tlm <- train(x=tep, y=ter, method="lm", trControl=ctrl)   ##tlm = test linear model##
tridge <- train(x=tep, y=ter, method="ridge", trControl=ctrl)
tlasso <- train(x=tep, y=ter, method="lasso", trControl=ctrl)
tenet <- train(x=tep, y=ter, method="enet", trControl=ctrl)
##F##
plot(varImp(tenet))
