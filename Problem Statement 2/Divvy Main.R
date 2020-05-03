# Identifying the need and feasibility (revenue) for new stations 
# between 2 stations for providing parking spaces at heavy traffic stations. 
# This is to ensure that divvy does not lose customers, based on the parking docks unavailability.

# Heavy stations are identified as stations from where most trips start and most trips end and not
# whose dock capacity is initially larger because we need to identify stations which are the busiest.

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

# First converting trip duration to numeric and than to mins

train_data$tripduration<-as.numeric(train_data$tripduration)

# Cleaning Data

colSums(is.na(train_data))
train_data_remove<-train_data[,colSums(is.na(train_data))<2000000]
train_data<-na.omit(train_data_remove)


train_data$duration.mins <- train_data$tripduration/60
summary(train_data$duration.mins)
boxplot(train_data$duration.mins, plot = FALSE)$out
outliers <- boxplot(train_data$duration.mins, plot = FALSE)$out
train_data<- train_data[-which(train_data$duration.mins %in% outliers),]

library(lubridate)
library(stringr)

# Splitting Date and Time in two different Columns.
n<-str_split_fixed(train_data$start_time, " ", 2)   
train_data$startDate <-n[,1]
train_data$startTime <-n[,2]

n<-str_split_fixed(train_data$end_time, " ", 2)   
train_data$endDate <-n[,1]
train_data$endTime <-n[,2]

# Data Cleaning: StartDate present in Varied formats.
# Hence, made in a single format. (%Y-%m-%d)
train_data$startDate <- parse_date_time(train_data$startDate, orders = c("%m/%d/%Y", "%Y-%m-%d"))
train_data$endDate <- parse_date_time(train_data$endDate, orders = c("%m/%d/%Y", "%Y-%m-%d"))

## For test data

# First converting trip duration to numeric and than to mins

test_data$tripduration<-as.numeric(test_data$tripduration)

# Cleaning Data

colSums(is.na(test_data))
test_data_remove<-test_data[,colSums(is.na(test_data))<2000000]
test_data<-na.omit(test_data_remove)


test_data$duration.mins <- test_data$tripduration/60
summary(test_data$duration.mins)
boxplot(test_data$duration.mins, plot = FALSE)$out
outliers1 <- boxplot(test_data$duration.mins, plot = FALSE)$out
test_data<- test_data[-which(test_data$duration.mins %in% outliers1),]

library(lubridate)
library(stringr)

# Splitting Date and Time in two different Columns.
m<-str_split_fixed(test_data$start_time, " ", 2)   
test_data$startDate <-m[,1]
test_data$startTime <-m[,2]

m<-str_split_fixed(test_data$end_time, " ", 2)   
test_data$endDate <-m[,1]
test_data$endTime <-m[,2]

# Data Cleaning: StartDate present in Varied formats.
# Hence, made in a single format. (%Y-%m-%d)
test_data$startDate <- parse_date_time(test_data$startDate, orders = c("%m/%d/%Y", "%Y-%m-%d"))
test_data$endDate <- parse_date_time(test_data$endDate, orders = c("%m/%d/%Y", "%Y-%m-%d"))

## Data Analysis

# Looking at busiest from and to stations
departures <- as.data.frame(table(train_data$from_station_name))
departures<-setNames(departures, c("Station.Name","departs"))
head(departures)

arrivals <- as.data.frame(table(train_data$to_station_name))
arrivals<-setNames(arrivals, c("Station.Name","arrivals"))
head(arrivals)

# Duration of Rentals in Seconds
str(train_data$tripduration)
summary(train_data$tripduration)

# Looking for duration of trips
sum(train_data$duration.mins > 9) / nrow(train_data)

sum(train_data$duration.mins <= 8) / nrow(train_data)
# We find that around 45.6% of the rentals are more than 9 minutes
# Around 46.7% of the rentals are less than or equal to 8 minutes

summary(train_data$Total.Docks)

# Finding number of unique stations
length(unique(unlist(train_data[c("Station.Name")])))
# unique(unlist(train_data[c("Station.Name")]))


checkout_data<- merge(departures,arrivals,by.x="Station.Name",by.y = "Station.Name",all.x = T,all.y = T)
checkout_data<- merge(checkout_data,dock_data,by.x="Station.Name",by.y = "Station.Name",all.x = T,all.y = T)
checkout_data<-checkout_data[-c(6:10)]
colSums(is.na(checkout_data))
checkout_data_remove<-checkout_data[,colSums(is.na(checkout_data))<75]
checkout_data<-na.omit(checkout_data_remove)
checkout_data <- checkout_data[order(-checkout_data$departs, -checkout_data$arrivals),]
top10<-head(checkout_data,10)

summary(checkout_data$departs)
boxplot(checkout_data$departs, main = "Boxplot of departures", ylab = "Total Departures")
boxplot(checkout_data$arrivals, main = "Boxplot of arrivals", ylab = "Total Arrivals")
boxplot(checkout_data$Dock_diff, main = "Boxplot of Arrivals-Departures", ylab = "Arrivals- Departures")
boxplot(checkout_data$Total.Docks, main = "Boxplot of Dock Capacity", ylab = "Dock Capacity")


options(scipen=5)
boxplot(departs~arrivals, data=checkout_data, notch=FALSE,
        col=(c("gold")),
        main="Departure vs Arrivals", xlab="Arrivals")

ggplot(top10, aes(x=departs, y=Station.Name), ) +
  geom_bar(stat = 'identity')+
  coord_flip()

library(ggplot2)
par(mai=c(1,2,1,1))
barplot(height=top10$departs, names=top10$Station.Name, 
        col="#69b3a2",
        horiz=T, las=1
)

# Top 10 stations with most Departures

ggplot(top10, aes(x = Station.Name,y = departs, fill = "departs")) +
  geom_bar(stat='identity',color = "black") +
  ggtitle("Top 10 stations with most Departures") +
  scale_fill_brewer(palette="Dark2")+
  xlab("Station Name") +
  ylab("Number of Departures")

# Top 10 stations with most Arrivals

ggplot(top10, aes(x = Station.Name,y = arrivals, fill = "arrivals")) +
  geom_bar(stat='identity',color = "black", fill="blue1") +
  ggtitle("Top 10 stations with most Arrivals") +
  xlab("Station Name") +
  ylab("Number of Arrivals")

# Density plot of Trip duartion
ggplot(train_data, aes(x = train_data$duration.mins)) +
  geom_density() +
  ggtitle("Density plot of Trip duartion") +
  xlab("Duration in Mins") +
  ylab("Number of Trips")

# Scatterplot of Longitude and Latitude

ggplot(train_data,aes(x=Longitude,y=Latitude)) + 
  geom_point()

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