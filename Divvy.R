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

# Cleaning Data

colSums(is.na(train_data))
train_data_remove<-train_data[,colSums(is.na(train_data))<75000]
train_data<-na.omit(train_data_remove)

# Looking at busiest from and to stations
departures <- as.data.frame(table(train_data$from_station_name))
departures<-setNames(departures, c("Station.Name","departs"))
head(departures)

arrivals <- as.data.frame(table(train_data$to_station_name))
arrivals<-setNames(arrivals, c("Station.Name","arrivals"))
head(arrivals)

# Duration of Rentals in Seconds
str(train_data$tripduration)

# First converting trip duration to factor then to integer and than to mins
train_data$tripduration<-as.factor(train_data$tripduration)
train_data$tripduration<-as.integer(train_data$tripduration)
train_data$duration.mins <- train_data$tripduration/60
summary(train_data$duration.mins)

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

# Looking for duration of trips
sum(train_data$duration.mins > 90) / nrow(train_data)

sum(train_data$duration.mins <= 30) / nrow(train_data)
# We find that around 85% of the rentals are more than 90 minutes

## Data Analysis
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

# Density plot of Trip duartion.
ggplot(train_data, aes(x = log(train_data$tripduration))) +
  geom_density() +
  # geom_text(aes(label="ylab")) +
  ggtitle("Usertype vs Number of trips") +
  xlab("Usertype") +
  ylab("Number of Trips")

