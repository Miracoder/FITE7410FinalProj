#/Users/hezhihao/hku-work/Financial Fraud/dataset/enron.csv
library(ggplot2)
install.packages('GGally')
library(GGally)
library(class)
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
library(factoextra)
install.packages('Hmisc')
install.packages("tensr")
install.packages("DescTools")
install.packages("DMwR")
library(Hmisc)
library(e1071)
library("mice")
library("tensr")
library(DescTools)
library(DMwR)
library(ROSE)
require(caret)
install.packages("party")
library(party)

set.seed(123)
use_pca = TRUE
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

# MICE together
numData_MICED <- mice(numData,m=5,maxit=20,meth='mean')
numData_MICED <- complete(numData_MICED,1)
colSums(is.na(numData_MICED))

## (Xie) correlation based on processedData 
data1 = numData_MICED
data1$poi = as.integer(as.logical(data1$poi))
data1 = select_if(data1, is.numeric)
corr_mat = cor(data1)
corrplot(corr_mat, tl.cex=0.6, method="number", number.cex=0.5, order="hclust")

# (Xie) feature engineering  
## Data Transformation - Normalization 
processedData_X = select_if(numData_MICED, is.numeric)
data.scaled.X = as.data.frame(scale(processedData_X))
data.scaled = data.scaled.X

## (Xie) PCA
data.pca = prcomp(data.scaled, scale = FALSE)
summary(data.pca)
prop_var_explained = data.pca$sdev^2 / sum(data.pca$sdev^2)

# Show the percentage of variances explained by each principal component.
p <- fviz_eig(data.pca,
              addlabels = T, 
              barcolor = "#E7B800", 
              barfill = "#E7B800", 
              linecolor = "#00AFBB", 
              choice = "variance",
              ylim = c(0, 65))

y = cumsum(prop_var_explained)*100/1.5
df <- data.frame(x=1:10,
                 y=y[1:10])
p <- p + 
  geom_point(data=df, aes(x, y), size=2, color="#00AFBB") +
  geom_line(data=df, aes(x, y), color="#00AFBB") +
  scale_y_continuous(sec.axis = sec_axis(~ . * 1.5, 
                                         name = "Cumulative proportion of Variance Explained") )
print(p)

# Graph of variables. Positive correlated variables point to the same side of the plot. 
# Negative correlated variables point to opposite sides of the graph.
options(ggrepel.max.overlaps = Inf)
fviz_pca_var(data.pca, labelsize = 3,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
options(backup_options)

# create folds
fold <- createFolds(numData$poi,k=partition,list=TRUE,returnTrain = FALSE)
lengths(fold)

# Self defined function
evaluate_cm <- function(cm) {
  tmpList <- c(cm$byClass[1],cm$byClass[2],cm$byClass[11],cm$byClass[5],cm$byClass[6],cm$byClass[7],cm$table[1],cm$table[3])
  return (tmpList)
}

# k fold cross validation
# Do MICE and ROSE on training folds, and MICE on test
result_lr = c()
result_rf = c()
result_nb = c()
result_knn = c()
result_svm = c()

for (i in 1:partition){
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

  # Train pca and test pca
  if (use_pca) {
    data.onlyNumeric <- select_if(train,is.numeric)
    data.pca = prcomp(data.onlyNumeric, scale = FALSE)
    prop_var_explained = data.pca$sdev^2 / sum(data.pca$sdev^2)
    upperBound <- 0.9
    curV <- 0
    pcaIdx <- 1
    while(curV<0.9) {
      curV = curV + prop_var_explained[pcaIdx]
      pcaIdx <- pcaIdx+1
    }
    pcaIdx = pcaIdx -1 
    train_pca <- data.frame(poi=train[,"poi"],data.pca$x[,1:pcaIdx])
    train <- train_pca
    
    test_pca <- predict(data.pca,newdata=test)
    test <- data.frame(poi=test[,"poi"],test_pca[,1:pcaIdx])
  }
  
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
  pred<-ifelse(prob<0.5,FALSE,TRUE)
  pred <- factor(pred,levels=c(FALSE,TRUE))
  
  f <- table(test$poi,pred)
  f
  cm_lr <- confusionMatrix(reference=test$poi,data=pred)
  cm_lr
  tmp_result_lr <- evaluate_cm(cm_lr)
  result_lr <- cbind(result_lr,tmp_result_lr)

  # Random Forest doesn't work
  model_rf<-randomForest(poi ~ ., data=train, importance = TRUE)
  print(model_rf)

  prediction_rf = predict(model_rf,test)
  cm_rf <- confusionMatrix(prediction_rf,test$poi)
  tmp_result_rf <- evaluate_cm(cm_rf)
  result_rf <- cbind(result_rf,tmp_result_rf)

  # Naive Bayes
  model_nb <- naiveBayes(poi~.,data=train)
  pred_nb <- predict(model_nb, newdata=test)
  pred_nb <- factor(pred_nb,levels=c(TRUE,FALSE))
  cm_nb <- confusionMatrix(table(test$poi,pred_nb))
  tmp_result_nb <- evaluate_cm(cm_nb)
  result_nb <- cbind(result_nb, tmp_result_nb)

  # KNN Model need to select best k
  if (use_pca) {
    model_knn <- knn(train = train[-c(1)],
                          test = test[-c(1)],
                          cl = train$poi,
                          k = 25)
  } else {
    model_knn <- knn(train = train[-c(16)],
                     test = test[-c(16)],
                     cl = train$poi,
                     k = 25)
  }
  model_knn <- factor(model_knn,levels=c(TRUE,FALSE))
  summary(model_knn)
  cm_knn <- confusionMatrix(model_knn, test$poi)
  tmp_result_knn <- evaluate_cm(cm_knn)
  result_knn <- cbind(result_knn, tmp_result_knn)
  cm_knn$byClass


  # SVM Model
  model_svm <- svm(formula = poi ~ .,
                   data = train)
  summary(model_svm)
  pred_svm_test <- predict(model_svm, test)
  pred_svm_test <- factor(pred_svm_test,levels=c(TRUE,FALSE))

  cm_tmp <- confusionMatrix(table(real=test$poi,predict=pred_svm_test))
  cm_tmp
  tmp_result_svm <- evaluate_cm(cm_tmp)
  result_svm <- cbind(result_svm, tmp_result_svm)
}


# Transpose
lr <- t(result_lr)
rf <- t(result_rf)
nb <- t(result_nb)
knn <- t(result_knn)
svm <- t(result_svm)

# Print result
summary(lr)
summary(rf)
summary(nb)
summary(knn)
summary(svm)
