# Importing Dataset
dataset =read.csv('data_raw.csv', header = TRUE, nrows = 1000)
# cleaned dataset
cleaned =read.csv('data.csv', header = TRUE, nrows = 1000)

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Differentiating date and time in two different columns.
# START TIME
STime <- format(as.POSIXct(strptime(dataset$starttime,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H:%M:%S")

SDate <- format(as.POSIXct(strptime(dataset$starttime,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%Y-%m-%d")


dataset$StartTime <- STime
dataset$StartDate <- SDate


#STOP TIME
StopTime <- format(as.POSIXct(strptime(dataset$stoptime,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H:%M:%S")

StopDate <- format(as.POSIXct(strptime(dataset$stoptime,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%Y-%m-%d")

dataset$StopTime <- StopTime
dataset$StopDate <- StopDate

# --------------------------------------------
drops <- c("starttime","stoptime", "events", "windchill", "precipitation")
dataset <- dataset[ , !(names(dataset) %in% drops)]


# --------------------------------------------
# count of rides vs month.

SMonth <- format(as.POSIXct(strptime(dataset$StartDate,"%Y-%m-%d",tz="")) ,format = "%m")
dataset$StartMonth <- SMonth
library(ggplot2)

ggplot(dataset, aes(StartMonth)) +
  geom_bar(color = "black", fill = "red") +
  ggtitle("Count of trips vs Month") +
  xlab("Month") +
  ylab("Count of Trips")


# Trying to implement Decision Tree.

library(rpart)
regressor = rpart(formula = to_station_id ~ wind_speed,
                  data = dataset,
                  control = rpart.control(minsplit = 1))


y_pred = predict(regressor, data.frame(wind_speed = 12.1))


dataset$gender = ifelse(is.na(dataset$gender),
                     mode(dataset$gender, FUN = function(x) mode(x, na.rm = TRUE)),
                     dataset$gender)


# Deleting rows with missing data.

dat <-dataset[!(dataset$gender==""),]

# **********************************************************************************

# Mohit Datasets
# Q1_2017 <- read.csv('Divvy_Trips_2017_Q1.csv', header = TRUE)
# Q2_2017 <- read.csv('Divvy_Trips_2017_Q2.csv', header = TRUE)
# Q3_2017 <- read.csv('Divvy_Trips_2017_Q3.csv', header = TRUE)
# Q4_2017 <- read.csv('Divvy_Trips_2017_Q4.csv', header = TRUE)
# 
# Q1_2018 <- read.csv('Divvy_Trips_2018_Q1.csv', header = TRUE)
# Q2_2018 <- read.csv('Divvy_Trips_2018_Q2.csv', header = TRUE)
# Q3_2018 <- read.csv('Divvy_Trips_2018_Q3.csv', header = TRUE)
# Q4_2018 <- read.csv('Divvy_Trips_2018_Q4.csv', header = TRUE)
# 
# Q1_2019 <- read.csv('Divvy_Trips_2019_Q1.csv', header = TRUE)
# Q2_2019 <- read.csv('Divvy_Trips_2019_Q2.csv', header = TRUE)
# Q3_2019 <- read.csv('Divvy_Trips_2019_Q3.csv', header = TRUE)
# Q4_2019 <- read.csv('Divvy_Trips_2019_Q4.csv', header = TRUE)

Q1_2017 <- read.csv('Divvy_Trips_2017_Q1.csv', header = TRUE, nrows = 1000)
Q2_2017 <- read.csv('Divvy_Trips_2017_Q2.csv', header = TRUE, nrows = 1000)
Q3_2017 <- read.csv('Divvy_Trips_2017_Q3.csv', header = TRUE, nrows = 1000)
Q4_2017 <- read.csv('Divvy_Trips_2017_Q4.csv', header = TRUE, nrows = 1000)

Q1_2018 <- read.csv('Divvy_Trips_2018_Q1.csv', header = TRUE, nrows = 1000)
Q2_2018 <- read.csv('Divvy_Trips_2018_Q2.csv', header = TRUE, nrows = 1000)
Q3_2018 <- read.csv('Divvy_Trips_2018_Q3.csv', header = TRUE, nrows = 1000)
Q4_2018 <- read.csv('Divvy_Trips_2018_Q4.csv', header = TRUE, nrows = 1000)

Q1_2019 <- read.csv('Divvy_Trips_2019_Q1.csv', header = TRUE, nrows = 1000)
Q2_2019 <- read.csv('Divvy_Trips_2019_Q2.csv', header = TRUE, nrows = 1000)
Q3_2019 <- read.csv('Divvy_Trips_2019_Q3.csv', header = TRUE, nrows = 1000)
Q4_2019 <- read.csv('Divvy_Trips_2019_Q4.csv', header = TRUE, nrows = 1000)


# Dataset Q1_2018 column names doesn't match.
Q1_2018 <- setNames(Q1_2018, c("trip_id", "start_time", "end_time", "bikeid", "tripduration", "from_station_id", "from_station_name", "to_station_id", "to_station_name", "usertype", "gender", "birthyear"))
Q2_2019 <- setNames(Q2_2019, c("trip_id", "start_time", "end_time", "bikeid", "tripduration", "from_station_id", "from_station_name", "to_station_id", "to_station_name", "usertype", "gender", "birthyear"))


# Train (2017-2018) and Test (2019)
TrainDst <- rbind(Q1_2017, Q2_2017, Q3_2017, Q4_2017, Q1_2018, Q2_2018, Q3_2018, Q4_2018, Q1_2019, Q2_2019, Q3_2019, Q4_2019)
Dst <- rbind(Q1_2017, Q2_2017, Q3_2017, Q4_2017, Q1_2018, Q2_2018, Q3_2018, Q4_2018)
TestDst <- rbind(Q1_2019, Q2_2019, Q3_2019, Q4_2019)

# Dock Dataset
DockDst <- read.csv('Divvy_Bicycle_Stations.csv', header = TRUE)

# Merging Dock dataset
TrainDst <- merge(TrainDst, DockDst, by.x = "from_station_id", by.y = "ID", all.x = T, all.y = F)
TestDst <- merge(TestDst, DockDst, by.x = "from_station_id", by.y = "ID", all.x = T, all.y = F)

# Differentiating date and time in two different columns for Training Dataset.
# START TIME
# 
# STime <- format(as.POSIXct(strptime(TrainDst$start_time,"%m/%d/%Y %H:%M:%S",tz="")) ,format = "%H:%M:%S")
# STime <- format(as.POSIXct(strptime(TrainDst$start_time,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H:%M:%S")
# 
# SDate <- format(as.POSIXct(strptime(TrainDst$start_time,"%m/%d/%Y %H:%M:%S",tz="")) ,format = "%m/%d/%Y")
# SDate <- format(as.POSIXct(strptime(TrainDst$start_time,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%m/%d/%Y")
# 
# 
# TrainDst$StartTime <- STime
# TrainDst$StartDate <- SDate
# 
# # STOP TIME
# 
# StopTime <- format(as.POSIXct(strptime(TrainDst$end_time,"%m/%d/%Y %H:%M:%S",tz="")) ,format = "%H:%M:%S")
# StopTime <- format(as.POSIXct(strptime(TrainDst$end_time,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H:%M:%S")
# 
# StopDate <- format(as.POSIXct(strptime(TrainDst$end_time,"%m/%d/%Y %H:%M:%S",tz="")) ,format = "%m/%d/%Y")
# StopDate <- format(as.POSIXct(strptime(TrainDst$end_time,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%m/%d/%Y")
# 
# 
# TrainDst$StopTime <- StopTime
# TrainDst$StopDate <- StopDate



# TrainDst$start_time <- as.Date(TrainDst$start_time, format = "%Y-%m-%d %H:%M:%S") || as.Date(TrainDst$start_time, format = "%m/%d/%Y %H:%M:%S")

# install.packages("lubridate")

library(lubridate)
library(stringr)

m<-str_split_fixed(TrainDst$start_time, " ", 2)   
TrainDst$startDate <-m[,1]
TrainDst$startTime <-m[,2]

n<-str_split_fixed(TestDst$start_time, " ", 2)
TestDst$startDate <- n[,1]
TestDst$startTime <-n[,2]



# Data Cleaning: StartDate present in Varied formats.
# Hence, made in a single format. (%Y-%m-%d)

# TrainDst$startDate <- as.Date(TrainDst$startDate)
TrainDst$startDate <- parse_date_time(TrainDst$startDate, orders = c("%m/%d/%Y", "%Y-%m-%d"))
TestDst$startDate <- parse_date_time(TestDst$startDate, orders = c("%m/%d/%Y", "%Y-%m-%d"))





if (TrainDst$startDate == (as.Date(TrainDst$startDate, format = "%m/%d/%Y"))) {
   TrainDst$startDate <- as.Date(TrainDst$startDate, format = "%Y-%m-%d")
} 

ggplot(TrainDst, aes(startTime)) + geom_density()
ggplot(TrainDst, aes(startTime, y = TrainDst$trip_id)) + geom_boxplot()

# if( TrainDst$start_time == format(as.POSIXct(TrainDst$start_time, "%m/%d/%Y %H:%M:%S",tz="")))
count = 0
for(i in TrainDst$from_station_id)
{
  if (i == 2){
    count <- count + 1
    }
}
count


TrainDst = as.data.frame(TrainDst)
install.packages("dplyr")
library(dplyr)

# library(tidyverse)


tally(group_by(TrainDst, TrainDst$from_station_id))
TrainDst %>% count(TrainDst$from_station_id)

# Check uniqueness of trip_ids.
nrow(TrainDst) == length(unique(TrainDst$trip_id))


library(data.table)
TrainDst <- data.table(TrainDst)
Trips <- TrainDst[, .(rowCount = .N), by =from_station_id]
Trips
max(Trips)

# Plot of Station Id vs Count of Trips on that particular Station Id. 

ggplot(TrainDst, aes(x = from_station_id)) +
  geom_bar(color = "black", fill = "red") +
    ggtitle("Station Id vs Number of trips") +
    xlab("StationID") +
    ylab("Count of Trips")

# Finding number of trips on a particular day.

TrainDst <- data.table(TrainDst)
counts <- TrainDst[, .(rowCount = .N), by =startDate]
counts

ggplot(TrainDst, aes(x = startDate)) +
  geom_bar(color = "black", fill = "red") +
  ggtitle("Start Date vs Number of trips") +
  xlab("Start Date") +
  ylab("Count of Trips")

# Numeric variables in the Dataset.

Cols <- unlist(lapply(TrainDst, is.numeric))
NumericCols <- TrainDst[,Cols]
for(col in names(NumericCols))
{
  png(paste(col,'_box.png'))
  boxplot(NumericCols[col],
          main = paste("Boxplot of",col),
          xlab = col)
}

pairs(NumericCols)

cor(NumericCols)
library(corrplot)
corrplot::corrplot(cor(NumericCols),method = "circle")


# library(ggplot2)
# ggplot(TrainDst, aes(x = from_station_id, y = tripduration)) +
#   geom_point(aes(color = factor(gender)))


# Hour vs number of trips.

sum(is.na(TrainDst$startTime)) # to check whether there are missing values.

ggplot(TrainDst, aes(x = format(strptime(TrainDst$startTime, "%H:%M"), "%H"))) +
  geom_bar(color = "black", fill = "red") +
 # geom_text(aes(label="ylab")) +
  ggtitle("Hour vs Number of trips") +
  xlab("Hour") +
  ylab("Number of Trips")



# rightNow <- as.time
# myDate <- as.Date("2019-01-01")
# format(myDate, "%y")
# 
# timeList <- unclass(as.POSIXlt(rightNow))
# 
# openingTime <- as.POSIXct('08:00:00 AM', format='%H:%M:S %p', tzone = "EET")

# Year vs number of trips.
ggplot(TrainDst, aes(x = format(strptime(TrainDst$startDate, "%Y-%m-%d"), "%Y"))) +
  geom_bar(color = "black", fill = "red") +
  # geom_text(aes(label="ylab")) +
  ggtitle("Year vs Number of trips") +
  xlab("Year") +
  ylab("Number of Trips")

# Month vs number of trips.
ggplot(TrainDst, aes(x = format(strptime(TrainDst$startDate, "%Y-%m-%d"), "%m"))) +
  geom_bar(color = "black", fill = "red") +
  # geom_text(aes(label="ylab")) +
  ggtitle("Month vs Number of trips") +
  xlab("Month") +
  ylab("Number of Trips")

# Day vs number of trips.
ggplot(TrainDst, aes(x = weekdays(as.POSIXct(TrainDst$startDate), abbreviate = F))) +
  geom_bar(color = "black", fill = "red") +
  # geom_text(aes(label="ylab")) +
  ggtitle("Month vs Number of trips") +
  xlab("Month") +
  ylab("Number of Trips")


# Gender vs number of trips.
ggplot(TrainDst, aes(x = TrainDst$gender)) +
  geom_bar(color = "black", fill = "red") +
  # geom_text(aes(label="ylab")) +
  ggtitle("Gender vs Number of trips") +
  xlab("Gender") +
  ylab("Number of Trips")

# usertype vs number of trips.
ggplot(TrainDst, aes(x = TrainDst$usertype)) +
  geom_bar(color = "black", fill = "red") +
  # geom_text(aes(label="ylab")) +
  ggtitle("Usertype vs Number of trips") +
  xlab("Usertype") +
  ylab("Number of Trips")

TrainDst$tripduration = as.numeric(TrainDst$tripduration)
# Density plot of Trip duartion.
ggplot(TrainDst, aes(x = log(TrainDst$tripduration))) +
  geom_density() +
  # geom_text(aes(label="ylab")) +
  ggtitle("Usertype vs Number of trips") +
  xlab("Usertype") +
  ylab("Number of Trips")
  
  
ggplot(TrainDst, aes(x=usertype, y=tripduration)) + 
  geom_boxplot(notch=FALSE)

#replcing null values in tripduartion by mean.

TrainDst$tripduration = ifelse(is.na(TrainDst$tripduration),
                        ave(TrainDst$tripduration, FUN = function(x) mean(x, na.rm = TRUE)),
                        TrainDst$tripduration)

# For a particular station Id, Finding number of trips in a day.
# ggplot(TrainDst, aes(x = TrainDst$))

# Multiple Linear Regression. To find number of docks needed on a particular station.

TrainDst$tripduration = as.numeric(TrainDst$tripduration)
TestDst$tripduration = as.numeric(TestDst$tripduration)

regressor = lm(formula = Total.Docks ~ from_station_id + tripduration + to_station_id + Latitude + Longitude,
               data = TrainDst)
summary(regressor)

y_pred = predict(regressor, newdata = TestDst)



backwardElimination <- function(x, sl) {
  numVars = length(x)
  for (i in c(1:numVars)){
    regressor = lm(formula = Total.Docks ~ from_station_id + tripduration + to_station_id + startDate + Latitude + Longitude, data = x)
    maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
    if (maxVar > sl){
      j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar)
      x = x[, -j]
    }
    numVars = numVars - 1
  }
  return(summary(regressor))
}

SL = 0.05
backwardElimination(TrainDst, SL)

# Number of bikes owned by Divvy. (Chicago)   4681
length(unique(TrainDst$trip_id))

# Number of station owned by Divvy. 519
length(unique(TrainDst$from_station_id)) == length(unique(TrainDst$to_station_id))   #FALSE 519 526

nrow(DockDst) == length(unique(DockDst$ID))

DockDst$Total.Docks = as.numeric(DockDst$Total.Docks)
sum(DockDst$Total.Docks)


