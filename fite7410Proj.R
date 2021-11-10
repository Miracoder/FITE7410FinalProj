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
#安装VIM和mice包
install.packages(c("VIM","mice")) 
# 载入VIM和mice包
library(VIM)
library(mice)
install.packages('Hmisc')
library(Hmisc)
setwd("C:/Users/70886/Desktop")
path = "./enron.csv"
dataset = read.csv(path)
# Load the data
#dataset = read.csv("/Users/hezhihao/hku-work/Financial Fraud/dataset/enron.csv")

#1 EDA part
dim(dataset)  #查看数据行列数
summary(dataset)
str(dataset)

#We have 21 features and most of them are numeric information besides the email_address 
#that represents the email of the user.
#Numbers of POI's and non-poi
table(dataset$poi)
table(dataset$poi)/length(dataset$poi)

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
#Financial : [‘salary’, ‘bonus’, ‘long_term_incentive’, ‘deferred_income’, ‘deferral_payments’, ‘loan_advances’, ‘other’, ‘expenses’, ‘director_fees’]
#Stock : [‘exercised_stock_options’, ‘restricted_stock’, ‘restricted_stock_deferred’]
#Total Payments : [‘total_payments’,’total_stock_value’]

#view the NAN distribution
dataset[!complete.cases(dataset),]
sum(is.na(dataset))
aggr(dataset,prop=FALSE,numbers=T)

# insert mean value to each column that is important
newdata<-dataset
# newdata$salary <- impute(newdata$salary,median)
# newdata$bonus <- impute(newdata$bonus,median)
# newdata$deferral_payments <- impute(newdata$deferral_payments,median)
# newdata$total_payments <- impute(newdata$total_payments,median)
# newdata$restricted_stock_deferred <- impute(newdata$restricted_stock_deferred,median)
# newdata$deferred_income <- impute(newdata$deferred_income,median)
# newdata$total_stock_value <- impute(newdata$total_stock_value,median)
# newdata$expenses <- impute(newdata$expenses,median)
# newdata$exercised_stock_options <- impute(newdata$exercised_stock_options,median)
# newdata$long_term_incentive <- impute(newdata$long_term_incentive,median)
# newdata$restricted_stock <- impute(newdata$restricted_stock,median)
# newdata$loan_advances <- impute(newdata$loan_advances,median)
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

library("mice")

#Remove attribute which have so many NAN value(>75%) ”deferral_payments”, “loan_advances”, ”restricted_stock_deferred”, ”director_fees”
threshold = 0.75*dim(dft)[1]
dft <- as.data.frame(dft)
dim(dft)
a <- dft[,(colSums(is.na(dft)) <= threshold)]
dim(a)
nhanes
md.pattern(nhanes)


