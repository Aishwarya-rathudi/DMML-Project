airbnb <- read.csv("C:/Users/rathu/Desktop/DMML Dataset/AB_NYC_2019.csv")
str(airbnb)
summary(airbnb)
#no of rows
nrow(airbnb)
#since the data is too large,we are taking only 10000 rows
airbnb<-airbnb[1:10000,]

#check null values
anyNA(airbnb)
sum(is.null(airbnb))
data.frame(colSums(is.na(airbnb)))
#Drop null values
airbnb<-na.omit(airbnb)

#Correlation
library(corrplot)

corContVar <-cor(airbnb[sapply(airbnb,is.numeric)],use = "complete.obs")
col <- colorRampPalette(c("darkgoldenrod4", "burlywood1",
                          "darkkhaki", "darkgreen"))
corrplot(corContVar,method = "number",col=col(200),
         order="hclust",
         type = "full",
         #addCoef.col = "black",
         tl.col="black", tl.srt=45, tl.cex=0.7, tl.offset = 0.5,
         number.cex=0.5,
         #diag = FALSE,
         number.digits = 2)
corrplot(corContVar, method="color", col=col(200),
         order="hclust",
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, tl.offset = 0.5, #Text label color and rotation
)
mtext("Correlation Plot", family = "serif",
      col = "#0B3948", side = 2, at= 5, line= 2.50, cex=2)


#define response variable
y<-airbnb$price
#define matrix of predictor variables
x <- data.matrix(airbnb[, c('host_id', 'latitude','longitude','minimum_nights','number_of_reviews','last_review','reviews_per_month','calculated_host_listings_count','availability_365')])
#install package
install.packages('glmnet')
library(glmnet)

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

#produce plot of test MSE by lambda value
plot(cv_model) 

#find coefficients of best model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)

#use fitted best model to make predictions
y_predicted <- predict(best_model, s = best_lambda, newx = x)
y_predicted[1,]
#find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

#find R-Squared
rsq <- 1 - sse/sst
rsq
