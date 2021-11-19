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
set.seed(123)

# Buttons
use_pca = FALSE

backup_options <- options()

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
path = "./enron.csv"
dataset = read.csv(path)

#1 EDA part
dim(dataset)  
summary(dataset)
str(dataset)

#We have 21 features and most of them are numeric information besides the email_address 
#that represents the email of the user.
#Numbers of POI's and non-poi
table(dataset$poi)
table(dataset$poi)/length(dataset$poi)
attributes(dataset)
# Analysis of distribution of poi labels
ggplot(data =dataset, aes(x= factor(poi),
                          y=prop.table(stat(count)),fill= factor(poi),
                          label=scales::percent(prop.table(stat(count)))))+
  geom_bar(position ="dodge")+ 
  geom_text(stat = 'count',
            position=position_dodge(.9), 
            vjust=-0.5,size=3)+
  scale_x_discrete(labels=c("False","True"))+ 
  scale_y_continuous(labels =scales::percent)+
  labs (x='isFraud' ,y='Percentage')+
  ggtitle("Distribution of poi labels")


#view the NAN distribution
dataset[!complete.cases(dataset),]
sum(is.na(dataset))
aggr(dataset,prop=FALSE,numbers=T)

# use data table
newdata<-dataset
library(data.table)
dim(newdata)
sum(is.na(newdata))
dft <- data.table(newdata)

# view the graph, rows with many NAN distribution
NANEmp = c()
for (row in 1:146){
  k <- sum(is.na(dft[row]))
  NANEmp<-append(NANEmp,k)
}
hist(NANEmp)

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
length(removeEmp)
if (length(removeEmp) !=0){
  dft <- dft[-removeEmp]
}
dim(dft)

#Remove attribute which have so many NAN value(>75%) 
threshold = 0.75*dim(dft)[1]
dft <- as.data.frame(dft)
dim(dft)
dft <- dft[,(colSums(is.na(dft)) < 109)]
naCols = colSums(is.na(dft))
shres = sd(naCols)+mean(naCols)
naCols[naCols>sd(naCols)
       +mean(naCols)]
l <- Large(naCols,5)
newdata <- dft
dim(dft)

# numData = data only numeric and poi
nums <- unlist(lapply(newdata, is.numeric)) # only the numeric is used to predict
numData <- newdata[nums]
summary(numData)
poi <- dft$poi
poi[poi=="False"] <- FALSE
poi[poi=="True"] <- TRUE
poi <- as.factor(poi)
class(numData)
numData$poi <- poi
str(numData)


# MICE, solve missing data. 
# processedData = data no missing, only numeric and poi
# test = numeric and poi data, scaled
naCols <- colSums(is.na(numData))
naCols[naCols!=0]
attributes(naCols)
summary(numData)
processedData <- mice(numData,m=5,maxit=20,meth='mean')
processedData <- complete(processedData,3)
summary(processedData)
colSums(is.na(processedData))
test <- processedData
ind <- sapply(test,is.numeric)
test[ind] <- lapply(test[ind],scale)

# 3 Data balance
#No more missing data. Start SMOTE,sampled_data is the data sampled using MICE
# train = sampled scaled data for training
#sampled_data <- ovun.sample(poi ~ ., data=processedData, p = 0.5, seed=123, method = "both", N = 2000)$data
#train <- sampled_data
#ind <- sapply(train,is.numeric)
#train[ind] <- lapply(train[ind],scale)
set.seed(9560)
smote_train <- SMOTE(poi ~ ., data=processedData,perc.over = 600,perc.under = 100)
train <- smote_train
ind <- sapply(train,is.numeric)
train[ind] <- lapply(train[ind],scale)
table(train$poi)

# 4 salary and bonus outliers detection
ggplot(train,aes(x=salary,y=bonus))+
  geom_point()

# ======================== Feature Engineering  ===========================
# processedData = no missing data
# sampled_data = sampled, no missing data
# train = sampled, no missing, scaled data
# test = original but scaled, no missing data



## (Xie) correlation based on processedData 
data1 = processedData
data1$poi = as.integer(as.logical(data1$poi))
data1 = select_if(data1, is.numeric)
corr_mat = cor(data1)

corrplot(corr_mat, tl.cex=0.6, method="number", number.cex=0.5, order="hclust")
# observe two groups of variables - financial / email-related


# (Xie) feature engineering  
## Data Transformation - Normalization 
numeric_sampled_data = select_if(sampled_data, is.numeric)
data.scaled = scale(numeric_sampled_data)
data.scaled
summary(data.scaled)

## (Xie) PCA
library(factoextra)
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
data.pca


# ======================= Detection model ===========================
# processedData = no missing data
# sampled_data = sampled, no missing data
# train = sampled, no missing, scaled data
# test = original but scaled, no missing data
# Please notice some method doesn't need sampling or scaling.


# logistic regression
summary(train)

if (use_pca) {
  logistic_train_pca <- data.frame(poi=train[,"poi"],data.pca$x[,1:7])
  
  logistic <- glm(poi~.,data=logistic_train_pca,family="binomial")
  summary(logistic)
  logistic <- step(object=logistic,trace=0)
  summary(logistic)
  
  test.logisticPCA <- predict(data.pca, newdata = test)
  head(test.logisticPCA)
  
  # predict class now
  prob <- predict(logistic, newdata = data.frame(test.logisticPCA[,1:7]),type="response")
  
  #anova(object=test.logisticPCA,test="Chisq") # don't understand
} else {
  logistic <- glm(poi~.,data=train,family="binomial")
  summary(logistic)
  logistic <- step(object=logistic,trace=0)
  summary(logistic)
  
  # predict class now
  prob <- predict(object=logistic,newdata = test,type="response")
  
  anova(object=logistic,test="Chisq") # don't understand  
}


# test LR
pred<-ifelse(prob<0.5,FALSE,TRUE)
length(pred)
pred <- factor(pred,levels=c(FALSE,TRUE),order=TRUE)
table(pred)
table(test$poi)
f <- table(test$poi,pred)
f
confusionMatrix(reference=test$poi,data=pred)

# support vector machine


# RANDOM FOREST

# data.rfImputed <- rfImpute(poi~.,data=numData,iter=20)
# data.balanced_rfImputed <- ovun.sample(poi ~ ., data=data.rfImputed, p = 0.5, seed=123, method = "both", N = 10000)$data
data.rfImputed <- processedData
rfModel <- randomForest(poi~.,data=data.rfImputed,proximity=TRUE)
rfModel

oob.error.data <- data.frame(
  Trees=rep(1:nrow(rfModel$err.rate), times=3),
  Type=rep(c("OOB", "TRUE", "FALSE"), each=nrow(rfModel$err.rate)),
  Error=c(rfModel$err.rate[,"OOB"], 
          rfModel$err.rate[,"TRUE"], 
          rfModel$err.rate[,"FALSE"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))

distance.matrix <- as.dist(1-rfModel$proximity)

mds.stuff <- cmdscale(distance.matrix, eig=TRUE, x.ret=TRUE)

## calculate the percentage of variation that each MDS axis accounts for...
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)

## now make a fancy looking plot that shows the MDS axes and the variation:
mds.values <- mds.stuff$points
mds.data <- data.frame(Sample=rownames(mds.values),
                       X=mds.values[,1],
                       Y=mds.values[,2],
                       Status=data.rfImputed$poi)

ggplot(data=mds.data, aes(x=X, y=Y, label=Sample)) + 
  geom_text(aes(color=Status)) +
  theme_bw() +
  xlab(paste("MDS1 - ", mds.var.per[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per[2], "%", sep="")) +
  ggtitle("MDS plot using (1 - Random Forest Proximities)")

# more detection model
