# Importing Dataset
dataset =read.csv('data_raw.csv', header = TRUE, nrows = 10000)
# cleaned dataset
cleaned =read.csv('data.csv', header = TRUE, nrows = 1000)

# Mohit Dataset
Q1 <- read.csv('Divvy_Trips_2017_Q1.csv', header = TRUE, nrows = 100)
Q2 <- read.csv('Divvy_Trips_2017_Q2.csv', header = TRUE, nrows = 100)
total <- rbind(Q1, Q2)

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
SMonth <- format(as.POSIXct(strptime(dataset$StartDate,"%Y-%m-%d",tz="")) ,format = "%m")
dataset$StartMonth <- SMonth
library(ggplot2)

ggplot(dataset, aes(StartMonth)) + geom_bar()







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


