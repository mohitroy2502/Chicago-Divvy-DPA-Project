# Loading and preparing data. Then use the data in python code and get the required datasets.

# Importing datasets.

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


# Converting tripduration attribute to numeric datatype.

Q1_2017$tripduration = as.numeric(Q1_2017$tripduration)
Q2_2017$tripduration = as.numeric(Q2_2017$tripduration)
Q3_2017$tripduration = as.numeric(Q3_2017$tripduration)
Q4_2017$tripduration = as.numeric(Q4_2017$tripduration)

Q1_2018$tripduration = as.numeric(Q1_2018$tripduration)
Q2_2018$tripduration = as.numeric(Q2_2018$tripduration)
Q3_2018$tripduration = as.numeric(Q3_2018$tripduration)
Q4_2018$tripduration = as.numeric(Q4_2018$tripduration)

Q1_2019$tripduration = as.numeric(Q1_2019$tripduration)
Q2_2019$tripduration = as.numeric(Q2_2019$tripduration)
Q3_2019$tripduration = as.numeric(Q3_2019$tripduration)
Q4_2019$tripduration = as.numeric(Q4_2019$tripduration)


library(stats)

DataPrepFunc <- function(x){
  # x$tripduration = as.numeric(x$tripduration)
  
  DurationSum <- aggregate(x$tripduration, by=list(bikeid = x$bikeid), FUN=sum)
  DurationSum <- as.data.frame(DurationSum)
  colnames(DurationSum) <- c('bikeid','TotalDuration')
  
  NoOfTrips <- aggregate(x$tripduration, by=list(bikeid = x$bikeid), FUN=length)
  NoOfTrips <- as.data.frame(NoOfTrips) 
  colnames(NoOfTrips) <- c('bikeid','No.Of.Trips')
  
  Dst <- merge(DurationSum, NoOfTrips, by.x = "bikeid", by.y = "bikeid", all.x = T, all.y = F)
  
  Dst$Maintenance <- 0
  Dst$Maintenance[Dst$TotalDuration>110000 | Dst$No.Of.Trips>80] <- 1
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