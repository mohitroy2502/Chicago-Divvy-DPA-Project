# Problem Statement 3 
# Predicting whether or not to put a bike on maintenance on a given day by keeping 
# various factors in mind like weather, maintenance due date etc.

# Loading and preparing data. Then use the data in python code and get the required datasets.

# Importing Primary dataset from the Chicago Divvy portal.
# Quarterly datasets of the year 2017, 2018 and 2019 are used.

Q1_2017 <- read.csv('Divvy_Trips_2017_Q1.csv', header = TRUE)
Q2_2017 <- read.csv('Divvy_Trips_2017_Q2.csv', header = TRUE)
Q3_2017 <- read.csv('Divvy_Trips_2017_Q3.csv', header = TRUE)
Q4_2017 <- read.csv('Divvy_Trips_2017_Q4.csv', header = TRUE)

Q1_2018 <- read.csv('Divvy_Trips_2018_Q1.csv', header = TRUE)
Q2_2018 <- read.csv('Divvy_Trips_2018_Q2.csv', header = TRUE)
Q3_2018 <- read.csv('Divvy_Trips_2018_Q3.csv', header = TRUE)
Q4_2018 <- read.csv('Divvy_Trips_2018_Q4.csv', header = TRUE)

Q1_2019 <- read.csv('Divvy_Trips_2019_Q1.csv', header = TRUE)
Q2_2019 <- read.csv('Divvy_Trips_2019_Q2.csv', header = TRUE)
Q3_2019 <- read.csv('Divvy_Trips_2019_Q3.csv', header = TRUE)
Q4_2019 <- read.csv('Divvy_Trips_2019_Q4.csv', header = TRUE)


# Alligning the names of the two datasets whose columns were not matching the rest of the datasets.
# Dataset Q1_2018 & Q2_2019 column names doesn't match.
Q1_2018 <- setNames(Q1_2018, c("trip_id", "start_time", "end_time", "bikeid", "tripduration", "from_station_id", "from_station_name", "to_station_id", "to_station_name", "usertype", "gender", "birthyear"))
Q2_2019 <- setNames(Q2_2019, c("trip_id", "start_time", "end_time", "bikeid", "tripduration", "from_station_id", "from_station_name", "to_station_id", "to_station_name", "usertype", "gender", "birthyear"))


# Calculating the Time duartion of every trip based on the start and end time.
# Data Cleaning. (Time available in various formats to be alligned in a single format.)

# Importing required libraries.
library(lubridate)

TimeCleanFunc <- function(x){
  
  x$start_time <- parse_date_time(x$start_time, orders = c("%m/%d/%Y %H:%M:%S","%m/%d/%Y %H:%M", "%Y-%m-%d %H:%M", "%Y-%m-%d %H:%M:%S"))
  x$end_time <- parse_date_time(x$end_time, orders = c("%m/%d/%Y %H:%M:%S","%m/%d/%Y %H:%M", "%Y-%m-%d %H:%M", "%Y-%m-%d %H:%M:%S"))
  
  # Calculating the time difference.
  x$tripduration = difftime(xt$end_time, x$start_time,units = "secs")
  
  # Converting tripduration attribute to numeric datatype.
  x$tripduration = as.numeric(x$tripduration)
  
}

TimeCleanFunc(Q1_2017)
TimeCleanFunc(Q2_2017)
TimeCleanFunc(Q3_2017)
TimeCleanFunc(Q4_2017)

TimeCleanFunc(Q1_2017)
TimeCleanFunc(Q2_2017)
TimeCleanFunc(Q3_2017)
TimeCleanFunc(Q4_2017)

TimeCleanFunc(Q1_2017)
TimeCleanFunc(Q2_2017)
TimeCleanFunc(Q3_2017)
TimeCleanFunc(Q4_2017)


# Importing required libraries.
library(stats)

# Calcualting the total duration bike has travelled and number of trips completed in every quarter.

# Tripduartion of 40 hours or No of trips 100 whichever is achieved first 
# is set the threshold for Bikes to be put for primary(regular) maintenance.

DataPrepFunc <- function(x){
  
  DurationSum <- aggregate(x$tripduration, by=list(bikeid = x$bikeid), FUN=sum)
  DurationSum <- as.data.frame(DurationSum)
  colnames(DurationSum) <- c('bikeid','TotalDuration')
  
  NoOfTrips <- aggregate(x$tripduration, by=list(bikeid = x$bikeid), FUN=length)
  NoOfTrips <- as.data.frame(NoOfTrips) 
  colnames(NoOfTrips) <- c('bikeid','No.Of.Trips')
  
  Dst <- merge(DurationSum, NoOfTrips, by.x = "bikeid", by.y = "bikeid", all.x = T, all.y = F)
  
  Dst$Maintenance <- 0
  Dst$Maintenance[Dst$TotalDuration>144000 | Dst$No.Of.Trips>100] <- 1
  x<-deparse(substitute(x))
  path<-paste("C:/Users/manis/Desktop/SEM2/Data Preparation And Analysis/Datasets/",x,".csv",sep = "")
  write.csv(Dst ,path, row.names = TRUE)
}

DataPrepFunc(Q1_2017)
DataPrepFunc(Q2_2017)
DataPrepFunc(Q3_2017)
DataPrepFunc(Q4_2017)

DataPrepFunc(Q1_2018)
DataPrepFunc(Q2_2018)
DataPrepFunc(Q3_2018)
DataPrepFunc(Q4_2018)

DataPrepFunc(Q1_2019)
DataPrepFunc(Q2_2019)
DataPrepFunc(Q3_2019)
DataPrepFunc(Q4_2019)
