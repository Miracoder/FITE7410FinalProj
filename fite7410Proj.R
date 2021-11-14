#/Users/hezhihao/hku-work/Financial Fraud/dataset/enron.csv
library(ggplot2)
install.packages('GGally')
library(GGally)
library(scales)
install.packages('memisc')
library(memisc)
library(dplyr)
library(corrplot)
install.packages('PerformanceAnalytics')
library(PerformanceAnalytics)
library(knitr)
#å®‰è£…VIMå’ŒmiceåŒ?
install.packages(c("VIM","mice")) 
# è½½å…¥VIMå’ŒmiceåŒ?
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

backup_options <- options()

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
path = "./enron.csv"
dataset = read.csv(path)
# Load the data
#dataset = read.csv("/Users/hezhihao/hku-work/Financial Fraud/dataset/enron.csv")

#1 EDA part
dim(dataset)  #æŸ¥çœ‹æ•°æ®è¡Œåˆ—æ•?
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


#we can distribute it into 3 categories:
#Financial : [â€˜salaryâ€?, â€˜bonusâ€?, â€˜long_term_incentiveâ€?, â€˜deferred_incomeâ€?, â€˜deferral_paymentsâ€?, â€˜loan_advancesâ€?, â€˜otherâ€?, â€˜expensesâ€?, â€˜director_feesâ€™]
#Stock : [â€˜exercised_stock_optionsâ€?, â€˜restricted_stockâ€?, â€˜restricted_stock_deferredâ€™]
#Total Payments : [â€˜total_paymentsâ€?,â€™total_stock_valueâ€™]

#view the NAN distribution
dataset[!complete.cases(dataset),]
sum(is.na(dataset))
aggr(dataset,prop=FALSE,numbers=T)

# insert mean value to each column that is important
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


#Remove attribute which have so many NAN value(>75%) â€deferral_paymentsâ€?, â€œloan_advancesâ€?, â€restricted_stock_deferredâ€?, â€director_feesâ€?
threshold = 0.75*dim(dft)[1]
dft <- as.data.frame(dft)
dim(dft)
dft <- dft[,(colSums(is.na(dft)) < 123)]
dim(a)
md.pattern(a)
naCols = colSums(is.na(dft))
shres = sd(naCols)+mean(naCols)
naCols[naCols>sd(naCols)
       +mean(naCols)]
l <- Large(naCols,5)
newdata <- dft

# MICE, solve missing data

naCols <- colSums(is.na(newdata))
naCols[naCols!=0]
attributes(naCols)
summary(newdata)
processedData <- mice(newdata,m=5,maxit=50,meth='mean')
processedData <- complete(processedData,3)
summary(processedData)
colSums(is.na(processedData))


# 
# 
# salary_dis <- aggregate(x=newdata[c('salary')], by = list(newdata$poi), FUN=mean)
# bonus_dis <- aggregate(x=newdata[c('bonus')], by = list(newdata$poi), FUN=mean)
# long_term_incentive_dis <- aggregate(x=newdata[c('long_term_incentive')], by = list(newdata$poi), FUN=mean)
# deferred_income_dis <- aggregate(x=newdata[c('deferred_income')], by = list(newdata$poi), FUN=mean)
# deferral_payments_dis <- aggregate(x=newdata[c('deferral_payments')], by = list(newdata$poi), FUN=mean)
# expenses_dis <- aggregate(x=newdata[c('expenses')], by = list(newdata$poi), FUN=mean)
# loan_advances_dis <- aggregate(x=newdata[c('loan_advances')], by = list(newdata$poi), FUN=mean)

#we can see that:
#poi        salary     bonus       long_term_incentive     deferred_income     deferral_payments
#False     467888.2    1634285        900750.8               -459969.6           647226.8
#True       376586.6    1929930         950586.6             -694832.9          308683.8

#expenses   loan_advances
#90984.12    41458105
#59873.83    43971528


#from the above analysis, we could guess person of interest tend to have the higher values comparing to the non-poi's persons.



# No more missing data. Start SMOTE
sampled_data <- ovun.sample(poi ~ ., data=processedData, p = 0.5, seed=123, method = "both", N = 10000)$data

# test data
nums <- unlist(lapply(processedData, is.numeric)) # only the numeric is used to predict 
tmpTestData <- processedData[nums]
tmpPoi <- processedData$poi
tmpPoi <- gsub("False","0",tmpPoi)
tmpPoi <- gsub("True","1",tmpPoi)
tmpPoi=as.factor(tmpPoi)
tmpPoi
tmpTestData$tmpPoi <- tmpPoi
test <- tmpTestData



## (Xie) correlation based on processedData 
data1 = processedData
data1$poi = as.integer(as.logical(data1$poi))
data1 = select_if(data1, is.numeric)
corr_mat = cor(data1)

corrplot(corr_mat, tl.cex=0.6, method="number", number.cex=0.5, order="hclust")
# observe two groups of variables - financial / email-related


# (Xie) feature engineering, skip  
## Data Transformation - Normalization 
sampled_data = select_if(sampled_data, is.numeric)
data.scaled = scale(sampled_data)
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
 




# Linear regression
dim(sampled_data)
nums <- unlist(lapply(sampled_data, is.numeric)) # only the numeric is used to predict 
numData <- sampled_data[nums]
tmpPoi <- sampled_data$poi
tmpPoi <- gsub("False","0",tmpPoi)
tmpPoi <- gsub("True","1",tmpPoi)


length(tmpPoi)
tmpPoi=as.factor(tmpPoi)
# tmpPoi <- as.numeric(tmpPoi)

tmpPoi

numData$tmpPoi <- tmpPoi
# numData <- cbind(numData,tmpPoi)
# numData$tmpPoi[numData$tmpPoi=="True"] <- as.numeric(1) # change character to numeric
# numData$tmpPoi[numData$tmpPoi=="False"] <- as.numeric(0)

# split data now
# sampleSize <- floor(0.8*nrow(numData))
# train_ind <- sample(seq_len(nrow(numData)),size=sampleSize)
# train <- numData[train_ind,]
# test <- numData[-train_ind,]
train <- numData


# lmTest = lm(tmpPoi~ .,data=train) # Linear regression
# summary(lmTest)
# distPred <- predict(lmTest,test)
# actual_preds <- data.frame(cbind(actuals=numData$tmpPoi,predicteds=distPred))
# correlation_accuracy <- cor(x=as.numeric(actual_preds$actuals),y=as.numeric(actual_preds$predicteds))
# (correlation_accuracy)







# logistic regression
summary(train)
train[1,]

logistic <- glm(tmpPoi~.,data=train,family="binomial")
summary(logistic)
model2 <- step(object=logistic,trace=0)
summary(model2)

anova(object=model2,test="Chisq") # don't understand
prob <- predict(object=model2,newdata = test,type="response")
pred<-ifelse(prob>=0.5,"yes","no")
length(pred)
pred <- factor(pred,levels=c("no","yes"),order=TRUE)
table(pred)
talbe(test$tmpPoi)
f <- table(test$tmpPoi,pred)
f



# support vector machine


# random forest




