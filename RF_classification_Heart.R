library(Amelia)
library(tidyverse)
library(dplyr)
library(stats)
library(lubridate)
library(corrplot)
library(caTools)
install.packages('adabag')
library("adabag")
library(caret)
library(randomForest)

library(e1071)
library(gains)
library(pROC)


#reading the csv data file
mydata = read.csv("C:/Users/rathu/Desktop/DMML Dataset/heart.csv", header = TRUE)

#since our data is too huge , so we are taking only 10000 rows into consideration
mydata <- mydata[1:10000,]


#checking the number of rows of data
nrow(mydata)

#checking the null values in the data
anyNA(mydata)
summary(mydata)
str(mydata)

#summary of data
summary(mydata)
head(mydata)


#as the missing values are zero
missmap(mydata)

#indexing our data to further divide it into training and testing data 
parts = createDataPartition(mydata$HeartDisease, p = 0.8, list = F)


#creating predictive variable to factor
mydata$HeartDisease <- as.factor(mydata$HeartDisease)



#splitting the data into training and test data
id <- sample(2,nrow(mydata), prob = c(0.7,0.3), replace = T)
mydata_ab_train <- mydata[id==1,]
mydata_ab_test <- mydata[id==2,]



train = mydata[parts, ]
test = mydata[-parts, ]



nrow(train)
nrow(test)



str(mydata_ab_train)
#applying the random forest model
RFM <- randomForest(HeartDisease~. ,data = mydata_ab_train )
RFM


#testing the data on model build up
pred_test = predict(RFM, mydata_ab_test)
pred_test



#building the confusion matrix
cm <- table(mydata_ab_test$HeartDisease,pred_test)
cm



#taking accuracy
accuracy_model <- (2671+18)/(2671+18+26+283)
accuracy_model



#taking precision
taking_precision <- (22)/(22+24)
taking_precision



#taking recall
taking_recall <- (22)/(22+262)
taking_recall



#taking F1 score
F1_SCORE <- (2*taking_precision*taking_recall)/(taking_precision+taking_recall)
F1_SCORE



str(mydata)
#feature selection 
RFM <- randomForest(HeartDisease~ BMI+PhysicalHealth+MentalHealth+SleepTime,
                    data= mydata_ab_train)
RFM



#testing with test data
pred_test = predict(RFM, mydata_ab_test)
pred_test



#building the confusion matrix
cm <- table(mydata_ab_test$HeartDisease,pred_test)
cm



#taking accuracy
accuracy_model <- (2673+5)/(2673+5+24+296)
accuracy_model



#taking precision
taking_precision <- (5)/(5+24)
taking_precision



#taking recall
taking_recall <- (5)/(5+296)
taking_recall



#taking F1 score
F1_SCORE <- (2*taking_precision*taking_recall)/(taking_precision+taking_recall)
F1_SCORE


ggplot(mydata, aes(x=HeartDisease)) + geom_bar()

