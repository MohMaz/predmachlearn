library(ggplot2)
library(dplyr)
library(caret)
set.seed(223)
# Load Train and Test Data
X <- read.csv('pml-training.csv')
YLevels <- levels(X[,dim(X)[2]])
subsetIndex <- createDataPartition(X[,dim(X)[2]], p = 0.01, list = FALSE)
X <- X[subsetIndex,]
correctNAs = function(df){df<- sapply(df, function(x){x[x==""] <- NA;x[x=="#DIV/0"] <- NA;x}); data.frame(df)}
X <- correctNAs(X)
# summary(X)
XTest <- read.csv('pml-testing.csv')
XTest <- correctNAs(XTest)
YTest <- XTest[,dim(XTest)[2]]
XTest <- XTest[,-dim(XTest)[2]]
# Remove features that are NAN in more than 0.7 of cases
nanCount <- sapply(X, function(x){sum(is.na(x))})
highNanCols <- which(nanCount > 0.7*dim(X)[1])
X <- X[,-highNanCols]
XTest <- XTest[,-highNanCols]
# Impute other missin values
# if(sum(is.na(X))> 0){
#       preComp <- preProcess (X, method = c("knnImpute"), k = 10)
#       X <- predict (pp, newdata = X)
# }

Y <- X[,dim(X)[2]]
Y <- factor(Y, labels = YLevels)
X <- X[,-dim(X)[2]]
# Select Features that proveide 0.95 of Variance in dataset using Principal Component Analysis
preProc <- preProcess(X, method = 'pca', thresh = 0.95)
X <- predict(preProc, X)
XTest <- predict(preProc,XTest)

trCtrl <- trainControl(method = 'cv',number = 2)
model <- train(X, Y,trainControl = trCtrl,method = 'svmRadial', metric = 'Accuracy' )
# predictions <- predict(model,X[-n])
# predictions <- sapply(predictions, function(x){min(max(round(x),1),5)})
# confRes <- confusionMatrix(round(predictions),Y)
# predictions  <- predict(model, XTest)
# confusionMatrix(predictions, YTest)