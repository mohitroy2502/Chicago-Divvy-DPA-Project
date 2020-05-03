## Making use of Loaded and Merged data in order to prepare data for cleaning.

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

