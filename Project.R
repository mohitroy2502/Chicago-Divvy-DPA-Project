library(stringr)
library(chron)
library(plyr)
library(lubridate)
library(corrplot)
library(fastDummies)
library(ridge)
library(glmnet)
library(caret)
library(dplyr)
divvy<-read.csv("DivvyAvlBikesID35.csv")
weather<-read.csv("weather.csv")
divvydata<-read.csv("DivvyTripDataforID35Train.csv")
divvydatatest<-read.csv("DivvyTripDataforID35Test.csv")

divvyTrain<-divvy[year(divvy$Date) %in% 2017:2018,]
colnames(divvyTrain) <- c('startDate','hour','avail_bikes')
divvyTest<-divvy[!(year(divvy$Date) %in% 2017:2018),]
colnames(divvyTest) <- c('startDate','hour','avail_bikes')

weatherTrain <- weather[year(weather$startDate) %in% 2017:2018,c('startDate','hour','icon')]
weatherTest <- weather[!(year(weather$startDate) %in% 2017:2018),c('startDate','hour','icon')]

Train<-merge(weatherTrain,divvydata,by=c("startDate","hour"),all.y = TRUE)
Train<-merge(Train,divvyTrain,by=c("startDate","hour"),all.x = TRUE)
Train <- na.omit(Train)
Train<-dummy_cols(Train)

Test<-merge(weatherTest,divvydatatest,by=c("startDate","hour"),all.y = TRUE)
Test<-merge(Test,divvyTest,by=c("startDate","hour"),all.x = TRUE)
Test <- na.omit(Test)
Test<-dummy_cols(Test)
# Train$Date<-as.Date(Train$Date)
# 
# Train<-Train[,!(colnames(Train) %in% "Weather_Icon")]
# weatherTest<-weatherTest[weatherTest$Date %in% divvyTest$Date,]
# Test<-merge(weatherTest,divvydatatest,by.x = "Date")
# Test<-merge(Test,divvyTest,by.x = "Date")
# Test$Date<-as.Date(Test$Date)
# 
# Test<-Test[,!(colnames(Test) %in% "Weather_Icon")]

cols <- c('Hourly_Average_Trips', 'Hourly_Avg_avl_bikes')
pre_proc_val <- preProcess(Train[,cols], method = c("center", "scale"))
Train[,cols] <- predict(pre_proc_val, Train[,cols])
Test[,cols] <- predict(pre_proc_val, Test[,cols])

eval_metrics = function(model, df, predictions, target){
  resids = df[,target] - predictions
  resids2 = resids**2
  N = length(predictions)
  r2 = as.character(round(summary(model)$r.squared, 2))
  adj_r2 = as.character(round(summary(model)$adj.r.squared, 2))
  print(adj_r2) #Adjusted R-squared
  print(as.character(round(sqrt(sum(resids2)/N), 2))) #RMSE
}

#Linear Regression
lr<-lm(avail_bikes~.,data = Train)
#+poly(Hourly_Average_Trips,18)
summary(lr)
predictions = predict(lr, newdata = Train)
eval_metrics(lr, Train, predictions, target = 'Hourly_Avg_avl_bikes')
predictions = predict(lr, newdata = Test)
eval_metrics(lr, Test, predictions, target = 'Hourly_Avg_avl_bikes')
predict(lr,Test)

#Ridge Regression
dummies <- dummyVars(Hourly_Avg_avl_bikes ~ ., data = Train)
train_dummies = predict(dummies, newdata = Train)
test_dummies = predict(dummies, newdata = Test)


lambdas <- 10^seq(2, -3, by = -.1)
ridge_reg = glmnet(as.matrix(train_dummies), Train$Hourly_Avg_avl_bikes, nlambda = 25, alpha = 0, family = 'gaussian', lambda = lambdas)
summary(ridge_reg)
cv_ridge <- cv.glmnet(as.matrix(train_dummies), Train$Hourly_Avg_avl_bikes, alpha = 0, lambda = lambdas)
optimal_lambda <- cv_ridge$lambda.min
optimal_lambda

eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  # Model performance metrics
  data.frame(RMSE = RMSE, Rsquare = R_square)
  
}
# Prediction and evaluation on train data
predictions_train <- predict(ridge_reg, s = optimal_lambda, newx = as.matrix(train_dummies))
eval_results(Train$Hourly_Avg_avl_bikes, predictions_train, Train)

# Prediction and evaluation on test data
predictions_test <- predict(ridge_reg, s = optimal_lambda, newx = as.matrix(test_dummies))
eval_results(Test$Hourly_Avg_avl_bikes, predictions_test, Test)

mridge<-linearRidge(Hourly_Avg_avl_bikes~.,data = Train)
predict(mridge,Test)
eval_results(Test$Hourly_Avg_avl_bikes, predict(mridge,Test), Test)

lasso_reg <- cv.glmnet(as.matrix(train_dummies), Train$Hourly_Avg_avl_bikes, alpha = 1, lambda = lambdas, standardize = TRUE, nfolds = 5)
lambda_best <- lasso_reg$lambda.min 
lambda_best

lasso_model <- glmnet(as.matrix(train_dummies), Train$Hourly_Avg_avl_bikes, alpha = 1, lambda = lambda_best, standardize = TRUE)

predictions_train <- predict(lasso_model, s = lambda_best, newx = Train)
eval_results(Train$Hourly_Avg_avl_bikes, predictions_train, Train)

predictions_test <- predict(lasso_model, s = lambda_best, newx = Test)
eval_results(Test$Hourly_Avg_avl_bikes, predictions_test, Test)
