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
boxplot(checkout_data$departs)

library(ggplot2)


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

# These are the 22 station names where we are going to add more docking station in between
# to help Divvy not lose it's customers.

negative_station$Station.Name

#Adding a new column of giving yes if we need a new dock station and no if we don't 


# KNN

##the column of training dataset because that is what we need to predict about testing dataset

x_train<-train_data[c('to_station_id','duration.mins','Latitude','Longitude')]
y_train <- train_data[c('Total.Docks')]
x_test<-test_data[c('to_station_id','duration.mins','Latitude','Longitude')]
y_test<-test_data[c('Total.Docks')]
cl<-as.factor(train_data[c('from_station_id')])

##run knn function
library(class)
nrow(train_data[c('from_station_id')])
length(x_train)
pr <- knn(x_train,y_train,cl,k=1)

##create the confusion matrix
tb <- table(pr,)

##check the accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tb)

# Making correlation plot to find hyperparameters

library(corrplot)
train_data_new<-as.numeric(train_data)
corrplot_main<-cor(train_data_new)
corrplot(corrplot_main,method = "number")

# Logistic Regression
library(stats)
model1= glm(formula = Total.Docks ~ duration.mins + to_station_id + Longitude + Latitude, family=binomial, data = train_data)
 
target1 = predict(model1, newdata = test_data)

# Decision Tree Classification to the Training set

library(rpart)

dtree = rpart(formula = Total.Docks ~  duration.mins + to_station_id + Longitude + Latitude ,
                    data = train_data)
Test1<-test_data
target2 = predict(dtree, newdata = Test1, type = 'class')

# Making the Confusion Matrix
library(caret)

confusion = confusionMatrix(reference = test_data$Total.Docks, data = target2)
confusion
