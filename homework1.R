###3.1###
library(AppliedPredictiveModeling)
library(mlbench)
data(Glass)
str(Glass)
dim(Glass)
x <- Glass[,1:9]      #Compacting the data into a variable to make it easier to work with#
par(mfrow = c(3,3))
for (i in 1:ncol(x))  #Looping through the data so there is less code to write#
  pairs(x, main ="Scatterplot Matrix")#   #Scatterplot to show relationship between predictors
  boxplot(x[ ,i], main = paste(names(x[i]), "Boxplot")) #Boxplot to see for any outliers
library(e1071)
skewness(Glass$RI)    #Calculating skewness for each variable
skewness(Glass$Na)
skewness(Glass$Mg)
skewness(Glass$Al)
skewness(Glass$Si)
skewness(Glass$K)
skewness(Glass$Ca)
skewness(Glass$Ba)
skewness(Glass$Fe)
YY = scale(x)       #Scaling the data
head(colMeans(YY))
var(YY[,1:9])  
library(caret)
NaTrans <- BoxCoxTrans(x$Na) #performing a transformation on the first predictor
NaTrans 
histogram(x$Na)
Na_log <- log(x$Na)
histogram(Na_log)
GlassTrans <- BoxCoxTrans(x$Al) #performing a transformation on the second predictor
GlassTrans
histogram(x$Al)
Al_root <-sqrt(x$Al)
histogram(Al_root)

###3.2###
data(Soybean)
str(Soybean)
nearZeroVar(Soybean, names=TRUE)  #using nearZeroVar to see which predictors have a near zero variance
library(naniar)
vis_miss(Soybean, sort_miss = TRUE)    #visualization of missing data




###3.3###
library(caret)
data(BloodBrain)       
x <- bbbDescr[,1:50]    #reassigning the data were working with into a single character variable
pairs(x[,1:10])         #Visualization for all the variables at the same time was pretty un-optimized so I split them
pairs(x[,10:20])        #up into groups of 10
pairs(x[,20:30])
pairs(x[,30:40])
pairs(x[,40:50])
