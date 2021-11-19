#/Users/hezhihao/hku-work/Financial Fraud/dataset/enron.csv
library(ggplot2)
install.packages('GGally')
library(GGally)
install.packages("randomForest")
library(randomForest)
library(scales)
library(caret)
install.packages('memisc')
library(memisc)
library(dplyr)
library(corrplot)
install.packages('PerformanceAnalytics')
library(PerformanceAnalytics)
library(knitr)
install.packages(c("VIM","mice")) 
library(VIM)
library(mice)
install.packages('Hmisc')
install.packages("tensr")
install.packages("DescTools")
install.packages("DMwR")
library(Hmisc)
library("mice")
library("tensr")
library(DescTools)
library(DMwR)
library(ROSE)
require(caret)
install.packages("party")
library(party)

warnings("on")
set.seed(123)
use_pca = FALSE
partition = 3

backup_options <- options()

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
path = "./enron.csv"
dataset = read.csv(path)

# use data table
newdata<-dataset

library(data.table)
dim(newdata)
sum(is.na(newdata))
dft <- data.table(newdata)

#2 Missing value Handling
#Remove employee with >= 17 attributes with missing values
dim(dft)
removeEmp = c()
res = 0
sum(is.na(dft))
dim(dft)
for (row in 1:146){
  k <- sum(is.na(dft[row]))
  res = res+k
  if(sum(is.na(dft[row]))>=17) {
    removeEmp <- append(removeEmp,row)
  }
}
print(res)
length(removeEmp)
if (length(removeEmp) !=0){
  dft <- dft[-removeEmp]
}

#Remove attribute which have so many NAN value(>75%) 
threshold = 0.75*dim(dft)[1]
dft <- as.data.frame(dft)
dim(dft)
dft <- dft[,(colSums(is.na(dft)) < 100)]
naCols = colSums(is.na(dft))
shres = sd(naCols)+mean(naCols)
naCols[naCols>sd(naCols)
       +mean(naCols)]
l <- Large(naCols,5)
newdata <- dft
str(newdata)
summary(newdata)

# numData = data only numeric and poi
nums <- unlist(lapply(newdata, is.numeric)) # only the numeric is used to predict
numData <- newdata[nums]
summary(numData)
poi <- dft$poi
poi[poi=="False"] <- FALSE
poi[poi=="True"] <- TRUE
poi <- factor(poi,levels=c(TRUE,FALSE))
poi
numData$poi <- poi

colSums(is.na(numData))


# Feature engineering





# MICE together
numData_MICED <- mice(numData,m=5,maxit=20,meth='mean')
numData_MICED <- complete(numData_MICED,1)
colSums(is.na(numData_MICED))

# create folds
fold <- createFolds(numData$poi,k=partition,list=TRUE,returnTrain = FALSE)
i
# k fold cross validation
# Do MICE and ROSE on training folds, and MICE on test
for (i in 1:5){
  oldName <- names(fold)[i]
  names(fold)[i] <- "test"
  test_unscaled <- numData_MICED[fold$test,]
  train_unsampled_unscaled <- numData_MICED[-fold$test,]
  names(fold)[i] <- oldName
  
  # scale test data
  test <- test_unscaled
  ind <- sapply(test,is.numeric)
  test[ind] <- lapply(test[ind],scale)
  summary(test)
  
  # scale training data
  train_unsampled <- train_unsampled_unscaled
  ind <- sapply(train_unsampled,is.numeric)
  train_unsampled[ind] <- lapply(train_unsampled[ind],scale)
  summary(train_unsampled)
  # sample training data
  train <- ovun.sample(poi ~ ., data=train_unsampled, p = 0.5, seed=123, method = "both", N = 2000)$data
  
  summary(train)
  summary(test)
  sum(is.na(train))
  sum(is.na(test))
  # ============================= MODEL ==================================#
  # =============================  LR ====================================#
  logistic <- glm(poi~.,data=train,family="binomial")
  summary(logistic)
  # logistic <- step(object=logistic,trace=0)
  # summary(logistic)

  # predict class now
  prob <- predict(object=logistic,newdata = test)
  prob
  # anova(object=logistic,test="Chisq") # don't understand
  # test LR
  pred<-ifelse(prob<0.5,FALSE,TRUE)
  length(pred)
  
  pred <- factor(pred,levels=c(FALSE,TRUE),order=TRUE)
  table(pred)
  table(test$poi)
  f <- table(test$poi,pred)
  f
  result_LR <- confusionMatrix(reference=test$poi,data=pred)
  # print(result_LR)
  
  
  # RANDOM FOREST
  model_rf<-randomForest(poi ~ ., data=train_unsampled_unscaled, importance = TRUE)
  print(model_rf)
  plot(model_rf)
  
  prediction_rf = predict(model_rf,test_unscaled)
  confusionMatrix(prediction_rf,test_unscaled$poi)
  
  
  # DECISION TREE MODEL
  output.tree<-ctree(train_unsampled_unscaled$poi~.,data = train_unsampled_unscaled)
  summary(output.tree)
  plot(output.tree)
}
  
