#rm(list=ls())
setwd("D:/R")
#Reading the data into R
data<-read.csv("Patient Adherence - Data.csv",header=T,sep=",")
#Studying the sturcture and summary of the attributes
View(data)
str(data)
summary(data)
names(data)
sum(is.na(data)) # we have one NA value in the dataset
data[!complete.cases(data),] # list rows of data that have missing values
tail(data)

data[is.na(data)] <- 205 # impute previous value in place of NA
tail(data)

library(dplyr)

data$Date=as.Date(as.character(data$Date))
#data$formatdate=format(data$Date,format="%d-%m-%Y")
str(data)
data1<-data %>%  
  mutate(date_diff=as.numeric(data$Date)-lag(as.numeric(data$Date),default=data$Date[1])) %>% 
  group_by(Medication,PatientID,date_diff) 

as.data.frame(data1)
data1$date_diff
names(data1)
str(data1)

data1$date_diff<- as.numeric(as.character(data1$date_diff))
head(data1)
View(data1)

data1$adher<- NULL
data1[data1$date_diff>data1$For_How_Many_Days,"adher"]<-"no"
data1[data1$date_diff<(data1$For_How_Many_Days+2),"adher"]<-"yes"
data1[data1$date_diff==0,"adher"]<-"1st"
head(data1)
data1$adher
table(data1$adher)
data1<- data1[ ! data1$adher %in% "1st", ]
data1$adher<-as.factor(data1$adher)
data1$Medication=as.numeric(data1$Medication)

str(data1)
set.seed(1234)
View(data1)
library(Hmisc)
data1$PatientID
table(data1$PatientID)

set.seed(1234)
ind <- sample(2, nrow(data1), replace = TRUE, prob = c(0.7, 0.3))
train <- data1[ind==1,]
test <- data1[ind==2,]
head(train)

train<-subset(data1, PatientID  <= 1010)
table(train$PatientID)
test<-subset(data1, PatientID  > 1010)
table(test$PatientID)
str(as.data.frame(data1))

library(randomForest)
rf<-randomForest(adher~.,data =train)
rf
varImpPlot(rf) 
library(caret)
p1<-predict(rf,train)
confusionMatrix(p1,train$adher)
p2<-predict(rf,test)
confusionMatrix(p2,test$adher)


# Cross validation

control <- trainControl(method = 'repeatedcv', number = 5, repeats = 3)

# Random forest
set.seed(1234)
mtry <- sqrt(ncol(train))
rf_random <- train(adher~., data = train, method = 'rf', metric = 'Accuracy', tuneLength = 7, trControl = control)

print(rf_random)
plot(rf_random)

predictions<- predict(rf_random,test)

# append predictions
pred<- cbind(test,predictions)

confusionMatrix<- confusionMatrix(pred$predictions,pred$adher)
confusionMatrix
varImp(rf_random)
train$adher
data1<- data1[ ! data1$adher %in% "1st", ]

#Bayesglm
library(caret) 
set.seed(1234)
lg_reg <- train(adher~., data = train, method = 'bayesglm', metric = 'Accuracy',  trControl = control,  maxit=100)
print(lg_reg)

predictions<- predict(lg_reg,test)

pred<- cbind(test,predictions)
confusionMatrix<- confusionMatrix(pred$predictions,pred$adher)


# Model output results

# Logistic Regression model predicts 92.14% accuracy for the "Test" data
# Data	Accuracy	Precision	Recall
# Train	99.78955	21.51766	4.638713
# Test	92.14	    34.87873	5.905822


# Random forest Regression model predicts 98.18 % accuracy for the "Test" data

# Data	Accuracy	Precision	Recall
# Train	99.88522	96.97802	97.8517
# Test	98.18 	  96.05978	96.5847
