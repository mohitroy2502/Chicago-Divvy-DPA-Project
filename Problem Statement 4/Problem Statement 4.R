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
library(RSocrata)
library(stats)
library(modelr)
library(broom)
library(tidyselect)


divvyAvl<-read.csv("DivvyAvlBikesID35.csv")
weatherTest<-read.csv("weatherTest.csv")
weatherTrain<-read.csv("weatherTrain.csv")
divvytripTrain<-read.csv("DivvyTripDataforID35Train.csv")
divvytripTest<-read.csv("DivvyTripDataforID35Test.csv")

divvyAvlTrain<-divvyAvl[year(divvyAvl$Date) %in% 2017:2018,]
colnames(divvyAvlTrain) <- c('date','hour','avail_bikes')
divvyAvlTest<-divvyAvl[!(year(divvyAvl$Date) %in% 2017:2018),]
colnames(divvyAvlTest) <- c('date','hour','avail_bikes')

colnames(divvytripTrain) <- c('date','hour','trips')
colnames(divvytripTest) <- c('date','hour','trips')

Train<-merge(divvyAvlTrain,divvytripTrain,by=c("date","hour"),all.x = TRUE)
Train<-merge(Train,weatherTrain,by=c("date","hour"),all.x = TRUE)
Train <- na.omit(Train)
Train$date <- as.Date(Train$date)
Train<-dummy_cols(Train)
Train <- Train[,!(names(Train) %in% "icon")]
Train<-Train[,c(1:12)]
Train$date <- as.numeric(Train$date)
names(Train) <- c('date','hour','avail_bikes','trips','icon_clear_day','icon_clear_night',
                    'icon_cloudy','icon_fog','icon_partly_cloudy_day'
                  ,'icon_partly_cloudy_night','icon_rain','icon_snow')

Test<-merge(divvyAvlTest,divvytripTest,by=c("date","hour"),all.x = TRUE)
Test<-merge(Test,weatherTest,by=c("date","hour"),all.x = TRUE)
Test <- na.omit(Test)
Test$date <- as.Date(Test$date)
Test<-dummy_cols(Test)
Test <- Test[,!(names(Test) %in% "icon")]
Test<-Test[,c(1:12)]
Test$date <- as.numeric(Test$date)
names(Test) <- c('date','hour','avail_bikes','trips','icon_clear_day','icon_clear_night',
                  'icon_cloudy','icon_fog','icon_partly_cloudy_day'
                  ,'icon_partly_cloudy_night','icon_rain','icon_snow')

eval_metrics = function(model, df, predictions, target){
  resids = df[,target] - predictions
  resids2 = resids**2
  N = length(predictions)
  r2 = round(summary(model)$r.squared, 2)
  adj_r2 = summary(model)$adj.r.squared
  print(adj_r2) #Adjusted R-squared
  print(sqrt(sum(resids2)/N)) #RMSE
}

cols <- c('date','hour','avail_bikes','trips')
pre_proc_val <- preProcess(Train[,cols], method = c("center", "scale"))
Train[,cols] <- predict(pre_proc_val, Train[,cols])
Test[,cols] <- predict(pre_proc_val, Test[,cols])

#Linear Regression
lr<-lm(trips~.,data = Train)
summary(lr)
predictions = predict(lr, newdata = Train)
eval_metrics(lr, Train, predictions, target = 'avail_bikes')
predictions = predict(lr, newdata = Test)
eval_metrics(lr, Test, predictions, target = 'avail_bikes')


#Ridge Regression
dummies <- dummyVars(trips ~ ., data = Train)
train_dummies = predict(dummies, newdata = Train)
test_dummies = predict(dummies, newdata = Test)


lambdas <- 10^seq(2, -3, by = -.1)
ridge_reg = glmnet(as.matrix(train_dummies), Train$trips, nlambda = 25, alpha = 0, family = 'gaussian', lambda = lambdas)
summary(ridge_reg)
cv_ridge <- cv.glmnet(as.matrix(train_dummies), Train$trips, alpha = 0, lambda = lambdas)
optimal_lambda <- cv_ridge$lambda.min
optimal_lambda

eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  print(RMSE)
  # Model performance metrics
  data.frame(RMSE = RMSE, Rsquare = R_square)
}
# Prediction and evaluation on train data
predictions_train <- predict(ridge_reg, s = optimal_lambda, newx = as.matrix(train_dummies))
eval_results(Train$trips, predictions_train, Train)

# Prediction and evaluation on test data
a <- predict(ridge_reg, s = optimal_lambda, newx = as.matrix(test_dummies))
eval_results(Test$trips, a, Test)
save(ridge_reg,file = "ridge_reg.rda")

# mridge<-linearRidge(trips~.,data = Train)
# a <- predict(mridge,Test)
# predictions_test<-ifelse(a<0,0,a)
# eval_results(Test$trips, a, Test)

lasso_reg <- cv.glmnet(as.matrix(train_dummies), Train$trips, alpha = 1, lambda = lambdas, standardize = TRUE, nfolds = 5)
lambda_best <- lasso_reg$lambda.min 
lambda_best

lasso_model <- glmnet(as.matrix(train_dummies), Train$trips, alpha = 1, lambda = lambda_best, standardize = TRUE)

predictions_train <- predict(lasso_model, s = lambda_best, newx = as.matrix(train_dummies))
eval_results(Train$trips, predictions_train, Train)

a <- predict(lasso_model, s = lambda_best, newx = as.matrix(test_dummies))

eval_results(Test$trips, a, Test) 
save(lasso_model,file = "lasso_reg.rda")

#ElasticNet Regression
train_cont <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 5,
                           search = "random",
                           verboseIter = TRUE)

elastic_reg <- train(trips ~ .,
                     data = Train,
                     method = "glmnet",
                     preProcess = c("center", "scale"),
                     tuneLength = 10,
                     trControl = train_cont)
elastic_reg$bestTune
predictions_train <- predict(elastic_reg, Train)
eval_results(Train$trips, predictions_train, Train) 
predictions_test <- predict(elastic_reg, Test[4,])
eval_results(Test$trips, predictions_test, Test)

save(elastic_reg,file = "elastic_reg.rda")
