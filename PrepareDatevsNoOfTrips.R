# Importing required libraries.
library(lubridate)
library(stringr)
# install.packages("dplyr")
library(dplyr)
library(data.table)
library(ggplot2)


# Import datasets. (Quarterly)

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

# Dataset Q1_2018 & Q2_2019 column names doesn't match.
Q1_2018 <- setNames(Q1_2018, c("trip_id", "start_time", "end_time", "bikeid", "tripduration", "from_station_id", "from_station_name", "to_station_id", "to_station_name", "usertype", "gender", "birthyear"))
Q2_2019 <- setNames(Q2_2019, c("trip_id", "start_time", "end_time", "bikeid", "tripduration", "from_station_id", "from_station_name", "to_station_id", "to_station_name", "usertype", "gender", "birthyear"))

# Getting the required Dataset.
Dst <- rbind(Q1_2017, Q2_2017, Q3_2017, Q4_2017, Q1_2018, Q2_2018, Q3_2018, Q4_2018, Q1_2019, Q2_2019, Q3_2019, Q4_2019)

# Data Cleaning and Transformation.

# Splitting Date and Time in two different Columns.
m<-str_split_fixed(Dst$start_time, " ", 2)   
Dst$startDate <-m[,1]
Dst$startTime <-m[,2]

# Data Cleaning: StartDate present in Varied formats.
# Hence, made in a single format. (%Y-%m-%d)
Dst$startDate <- parse_date_time(Dst$startDate, orders = c("%m/%d/%Y", "%Y-%m-%d"))

NoOfTrips_Date <- aggregate(Dst$trip_id, by=list(startDate = Dst$startDate), FUN=length)
NoOfTrips_Date <- as.data.frame(NoOfTrips_Date) 
colnames(NoOfTrips_Date) <- c('Date','No.Of.Trips')

write.csv(NoOfTrips_Date ,"C:/Users/manis/Desktop/SEM2/Data Preparation And Analysis/Project/chicago-divvy-bicycle-sharing-data/PreparedDatasets/DateTrips.csv", row.names = TRUE)


