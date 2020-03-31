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

Q1_2017 <- read.csv('Divvy_Trips_2017_Q1.csv', header = TRUE, nrows = 100)
Q2_2017 <- read.csv('Divvy_Trips_2017_Q2.csv', header = TRUE, nrows = 100)
Q3_2017 <- read.csv('Divvy_Trips_2017_Q3.csv', header = TRUE, nrows = 100)
Q4_2017 <- read.csv('Divvy_Trips_2017_Q4.csv', header = TRUE, nrows = 100)

Q1_2018 <- read.csv('Divvy_Trips_2018_Q1.csv', header = TRUE, nrows = 100)
Q2_2018 <- read.csv('Divvy_Trips_2018_Q2.csv', header = TRUE, nrows = 100)
Q3_2018 <- read.csv('Divvy_Trips_2018_Q3.csv', header = TRUE, nrows = 100)
Q4_2018 <- read.csv('Divvy_Trips_2018_Q4.csv', header = TRUE, nrows = 100)

Q1_2019 <- read.csv('Divvy_Trips_2019_Q1.csv', header = TRUE, nrows = 100)
Q2_2019 <- read.csv('Divvy_Trips_2019_Q2.csv', header = TRUE, nrows = 100)
Q3_2019 <- read.csv('Divvy_Trips_2019_Q3.csv', header = TRUE, nrows = 100)
Q4_2019 <- read.csv('Divvy_Trips_2019_Q4.csv', header = TRUE, nrows = 100)


# Dataset Q1_2018 column names doesn't match.
Q1_2018 <- setNames(Q1_2018, c("trip_id", "start_time", "end_time", "bikeid", "tripduration", "from_station_id", "from_station_name", "to_station_id", "to_station_name", "usertype", "gender", "birthyear"))

# Train (2017-2018) and Test (2019)
TrainDst <- rbind(Q1_2017, Q2_2017, Q3_2017, Q4_2017, Q1_2018, Q2_2018, Q3_2018, Q4_2018)
TestDst <- rbind(Q1_2019, Q2_2019, Q3_2019, Q4_2019)

# Dock Dataset
DockDst <- read.csv('Divvy_Bicycle_Stations.csv', header = TRUE)

# Merging Dock dataset
TrainDst <- merge(TrainDst, DockDst, by.x = "from_station_id", by.y = "ID", all.x = T, all.y = F)

# Differentiating date and time in two different columns for Training Dataset.
# START TIME

STime <- format(as.POSIXct(strptime(TrainDst$start_time,"%m/%d/%Y %H:%M:%S",tz="")) ,format = "%H:%M:%S")
STime <- format(as.POSIXct(strptime(TrainDst$start_time,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H:%M:%S")

SDate <- format(as.POSIXct(strptime(TrainDst$start_time,"%m/%d/%Y %H:%M:%S",tz="")) ,format = "%m/%d/%Y")
SDate <- format(as.POSIXct(strptime(TrainDst$start_time,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%m/%d/%Y")


TrainDst$StartTime <- STime
TrainDst$StartDate <- SDate
