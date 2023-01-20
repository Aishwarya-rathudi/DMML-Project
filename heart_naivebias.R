#loading the required library
library(e1071)
library(caret)
library(klar)
library(gains)
library(pROC)
library(Amelia)
library(caTools)


#Loading the data
heart <- read.csv("C:/Users/rathu/Desktop/DMML Dataset/heart.csv", header = TRUE)


#since our data is too huge , so we are taking only 10000 rows into consideration
heart <- heart[1:10000,]


#checking the number of rows of data
nrow(heart)


#checking the null values in the data
data.frame(colSums(is.na(heart)))
anyNA(heart)


str(heart)
#summary of data
summary(heart)


head(heart)


#as the missing values are zero
missmap(heart)

#Visualization
histogram(heart$HeartDisease)


#Data Encoding
heart$HeartDisease<- as.factor(heart$HeartDisease)
str(heart)

#indexing our data to further divide it into training and testing data 
parts = createDataPartition(heart$HeartDisease, p = 0.8, list = F)



#splitting the data into training and test data
id <- sample(2,nrow(heart), prob = c(0.7,0.3), replace = T)
heart_ab_train <- heart[id==1,]
heart_ab_test <- heart[id==2,]


train = heart[parts, ]
test = heart[-parts, ]


nrow(train)
nrow(test)


#aApplying Naive Bayes 
length(heart$HeartDisease)
nrow(heart_ab_test)
model_nB <- naiveBayes(heart$HeartDisease~.,heart)


summary(model_nB)


#using model to make predictions on test data
pred_test = predict(model_nB, heart_ab_test)
pred_test


#confusion matrix
cm <- table(heart_ab_test$HeartDisease, pred_test)
cm
#missclassification
1 - sum(diag(cm)) / sum(cm)

#Checking the accuracy
accuracy_model <- (2432+122)/(2432+122+251+172)
accuracy_model


#Taking the precision
taking_precision <- (123)/(123+279)
taking_precision


#taking recall
taking_recall <- (123)/(123+167)
taking_recall


#taking F1 score
F1_SCORE <- (2*taking_precision*taking_recall)/(taking_precision+taking_recall)
F1_SCORE



#Featureselection
model_nB <- naiveBayes(HeartDisease~BMI+PhysicalHealth+AgeCategory+SleepTime+GenHealth, data=heart_ab_train)
summary(model_nB)


#using model to make predictions on test data
pred_test = predict(model_nB, heart_ab_test)
pred_test
plot(pred_test,)

#confusion matrix
cm <- table(heart_ab_test$HeartDisease, pred_test)
cm
1 - sum(diag(cm)) / sum(cm)


#Checking the accuracy
accuracy_model <- (2497+68)/(2497+68+186+226)
accuracy_model


#Taking the precision
taking_precision <- (78)/(78+216)
taking_precision


#taking recall
taking_recall <- (78)/(78+212)
taking_recall



#taking F1 score
F1_SCORE <- (2*taking_precision*taking_recall)/(taking_precision+taking_recall)
F1_SCORE

