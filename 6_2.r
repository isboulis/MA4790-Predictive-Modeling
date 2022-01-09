##6.2##
##A##
library(AppliedPredictiveModeling)
data("permeability")
library(pls)
##B##
nzr <- nearZeroVar(fingerprints)
nzv <- fingerprints[,-nzr]
##C##
partition <- createDataPartition(permeability, p=4/5, list=FALSE)
trF <- nzv[partition,]
trP <- permeability[partition,]
teF <- nzv[-partition,]
teP <- permeability[-partition]
ctrl <- trainControl(method="repeatedcv",3,3)
pls <- train(trF,trP, method="pls", tuneGrid=expand.grid(ncomp=1:20), trControl=ctrl, preProc = c("center", "scale"))
plot(pls)
pls
##D##
test <- predict(pls,teF)
dapa <- data.frame(obs=teP, pred=test)
defaultSummary(dapa)
