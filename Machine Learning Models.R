## Implementing ML models on the final prepared and cleaned data set to find out the number of new dock stations needed

# Increasing Memory limit
memory.limit(size=56000)

# Importing all separate quarter data 
q1_2017 = as.data.frame(read.csv("D:\\Divvy Datasets\\Divvy_Trips_2017_Q1.csv"))
q2_2017 = as.data.frame(read.csv("D:\\Divvy Datasets\\Divvy_Trips_2017_Q2.csv"))
q3_2017 = as.data.frame(read.csv("D:\\Divvy Datasets\\Divvy_Trips_2017_Q3.csv"))
q4_2017 = as.data.frame(read.csv("D:\\Divvy Datasets\\Divvy_Trips_2017_Q4.csv"))

q1_2018 = as.data.frame(read.csv("D:\\Divvy Datasets\\Divvy_Trips_2018_Q1.csv"))
q2_2018 = as.data.frame(read.csv("D:\\Divvy Datasets\\Divvy_Trips_2018_Q2.csv"))
q3_2018 = as.data.frame(read.csv("D:\\Divvy Datasets\\Divvy_Trips_2018_Q3.csv"))
q4_2018 = as.data.frame(read.csv("D:\\Divvy Datasets\\Divvy_Trips_2018_Q4.csv"))

q1_2019 = as.data.frame(read.csv("D:\\Divvy Datasets\\Divvy_Trips_2019_Q1.csv"))
q2_2019 = as.data.frame(read.csv("D:\\Divvy Datasets\\Divvy_Trips_2019_Q2.csv"))
q3_2019 = as.data.frame(read.csv("D:\\Divvy Datasets\\Divvy_Trips_2019_Q3.csv"))
q4_2019 = as.data.frame(read.csv("D:\\Divvy Datasets\\Divvy_Trips_2019_Q4.csv"))

# Importing the dock capacity data
dock_data = as.data.frame(read.csv("D:\\Divvy Datasets\\Divvy_Bicycle_Stations.csv"))

# Making similar column names in some individual quarter data
q1_2018 <- setNames(q1_2018, c("trip_id","start_time","end_time","bikeid","tripduration","from_station_id","from_station_name","to_station_id","to_station_name","usertype","gender","birthyear"))
q2_2019 <- setNames(q2_2019, c("trip_id","start_time","end_time","bikeid","tripduration","from_station_id","from_station_name","to_station_id","to_station_name","usertype","gender","birthyear"))

# Making separate data frames for test and training set
train_data<- rbind(q1_2017,q2_2017,q3_2017,q4_2017,q1_2018,q2_2018,q3_2018,q4_2018)
test_data<- rbind(q1_2019,q2_2019,q3_2019,q4_2019)

# Merging dock capacity data with training and testing data
train_data<- merge(train_data,dock_data,by.x="from_station_id",by.y = "ID",all.x = T,all.y = F)
test_data<- merge(test_data,dock_data,by.x="from_station_id",by.y = "ID",all.x = T,all.y = F)

## Model Implementations

# Estimating need of dock station by calculating difference of departure and arrival
# If arrival-departure is negative we need to add dock station in between

checkout_data$Dock_diff <-(checkout_data$arrivals-checkout_data$departs)

negative_station<-checkout_data[which(checkout_data$Dock_diff<0),]
nrow(negative_station)
summary(negative_station$Dock_diff)
boxplot(negative_station$Dock_diff,plot = FALSE)$out
outliers2 <- boxplot(negative_station$Dock_diff, plot = FALSE)$out
negative_station<- negative_station[which(negative_station$Dock_diff %in% outliers2),]

# These are the 24 station names where we are going to add more docking station in between
# to help Divvy not lose it's customers.

negative_station$Station.Name

#Adding a new column of giving yes if we need a new dock station and no if we don't 
train_data_new<- merge(train_data,negative_station,by.x="from_station_name",by.y = "Station.Name",all.x = T,all.y = F)
train_data_new$New_dock<- ifelse(negative_station$Station.Name==train_data_new$from_station_name,1,0)


test_data_new<- merge(test_data,negative_station,by.x="from_station_name",by.y = "Station.Name",all.x = T,all.y = F)
test_data_new$New_dock<- ifelse(negative_station$Station.Name==test_data_new$from_station_name,1,0)

# Making correlation plot to find hyperparameters

library(corrplot)
train_data_new<-as.numeric(train_data_new)
corrplot_main<-cor(train_data_new)
corrplot(corrplot_main,method = "number")

# Multiple Linear Regression model to find relationship among predictors and target

fit <- lm(New_dock ~ to_station_id + duration.mins + Latitude + Longitude + Total.Docks + Dock_diff, data=train_data_new)
summary(fit)

coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table
vcov(fit) # covariance matrix for model parameters
influence(fit) # regression diagnostics

# KNN

library(class)

## The column of training dataset because that is what we need to predict about testing dataset

x_train<-train_data_new[c('to_station_id','duration.mins','Latitude','Longitude','Total.Docks','Dock_diff')]
y_train <- train_data_new[c('New_dock')]
x_test<-test_data_new[c('to_station_id','duration.mins','Latitude','Longitude','Total.Docks','Dock_diff')]
y_test<-test_data_new[c('New_dock')]
cl<-train_data_new[c('New_dock')]

## Run knn function

knn_acc_list = list()

for(i in 1:20){
  model_KNN <-knn(x_train, y_train, cl, k=i, scale = TRUE)
  pred <- round(fitted(model_KNN)) == y_test  #predictions from the fitted function
  x = sum(pred) / nrow(y_test) #accuracy measurement -- average number of predictions returned TRUE
  
  knn_acc_list[[i]] = x
}

kknn_acc_list

preds <- knn.predict(x_train, y_train, y ,cl, k=4, agg.meth="majority")
preds

# Logistic Regression

library(stats)

model1= glm(formula = New_dock ~ duration.mins + to_station_id + Longitude + Latitude + Total.Docks + Dock_diff, family=binomial(link = 'logit'), data = train_data_new)

predictedval <- predict(model1,newdata=x_test,type='response')

fitted.results.cat <- ifelse(predictedval > 0.8,"Yes","No")

fitted.results.cat<-as.factor(fitted.results.cat)

require(caret)    

cm<-confusionMatrix(data=fitted.results.cat, 
                    reference=y_test)

Accuracy<-round(cm$overall[1],2)

# Final ML model results represented in the powerpoint presentation with accuracy and revenue calculated with assumptions