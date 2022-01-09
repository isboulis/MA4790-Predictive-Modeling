##6.1##
##A##
library(AppliedPredictiveModeling)
library(caret)
data(tecator)
##B##
pca <- prcomp(absorp, center=TRUE, scale=TRUE, tol=0.95)
screeplot(pca)
varpca <- pca$sdev^2/sum(pca$sdev^2)*100
head(varpca)
##C##
absorp <- data.frame(absorp)
endpoints <- data.frame(endpoints)
partition <- createDataPartition(endpoints$X2, p=4/5, list=FALSE)
trabsorp <- absorp[partition, ]
teabsorp <- absorp[-partition, ]
trpro <- endpoints$X2[partition]
tepro <- endpoints$X2[-partition ]
ctrl <- trainControl(method="cv", number=3)
Linmod <- train(trabsorp,trpro, method = "lm", trControl = ctrl, preProcess=c("center","scale"))
Linmod
##D##
##test <- train(teabsorp, tepro, method="lm", trControl=ctrl)##
pred <- predict(Linmod, teabsorp)
dapa <- data.frame(obs=tepro, pred=pred)
defaultSummary(dapa)
