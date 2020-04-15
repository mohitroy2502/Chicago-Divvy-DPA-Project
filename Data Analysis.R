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

# Importing Dock Dataset.
DockDst <- read.csv('Divvy_Bicycle_Stations.csv', header = TRUE)

# Merging Dock Dataset with main Dataset.
Dst <- merge(Dst, DockDst, by.x = "from_station_id", by.y = "ID", all.x = T, all.y = F)


# ************************************************************************************ #
# Data Cleaning and Transformation.

# Splitting Date and Time in two different Columns.
m<-str_split_fixed(Dst$start_time, " ", 2)   
Dst$startDate <-m[,1]
Dst$startTime <-m[,2]

m<-str_split_fixed(Dst$end_time, " ", 2)   
Dst$endDate <-m[,1]
Dst$endTime <-m[,2]

# Data Cleaning: StartDate present in Varied formats.
# Hence, made in a single format. (%Y-%m-%d)
Dst$startDate <- parse_date_time(Dst$startDate, orders = c("%m/%d/%Y", "%Y-%m-%d"))
Dst$endDate <- parse_date_time(Dst$endDate, orders = c("%m/%d/%Y", "%Y-%m-%d"))

# Check Uniqueness of trip_ids.
nrow(Dst) == length(unique(Dst$trip_id))

# To check whether there are missing values.
sum(is.na(Dst$startTime))

# Calculating trip duration based on start and end time
# 
# Dst$start_time<- as.numeric(as.POSIXct(Dst$startTime,"UTC",origin = "1970-01-01"))
# 
# Dst$tripduration = difftime(end_time,start_time,units = "secs")



# Dealing with Categorial Variables.
Dst$from_station_name <- as.factor(Dst$from_station_name)
Dst$to_station_name <- as.factor(Dst$to_station_name)
Dst$usertype <- as.factor(Dst$usertype)
Dst$bikeid <- as.factor(Dst$bikeid)
Dst$usertype <- as.factor(Dst$usertype)
Dst$gender <- as.factor(Dst$gender)
Dst$from_station_id <- as.factor(Dst$from_station_id)

catVars <- c("from_station_name", "to_station_name", "usertype", "bikeid","usertype", "gender")

# Dealing with Numeric variables.
Dst$tripduration <- as.numeric(Dst$tripduration)
Dst$Total.Docks <- as.numeric(Dst$Total.Docks)
Dst$Docks.in.Service <- as.numeric(Dst$Docks.in.Service)
Dst$Latitude <- as.numeric(Dst$Latitude)
Dst$Longitude <- as.numeric(Dst$Longitude)

numVars <- c("tripduration", "Total.Docks", "Docks.in.Service", "Latitude", "Longitude")



# Data Analysis.

# For Numeric Variables -
# Histograms to check the normality, Boxplots to present the mean and the quartiles,
# scatter plots of mpg vs other variables,
# scatter plot matrix to check correlation between the variables.


for(ColName in numVars)
{
  title <- paste("Histogram for", ColName, sep = " ")
  hist(Dst[[ColName]], main = title, xlab = ColName)
  
  title <- paste("Boxplot for", ColName, sep = " ")
  boxplot(Dst[[ColName]], main = title, xlab = ColName)
}

plot(Dst[which(colnames(Dst) %in% numVars)])


for(columnName in catVars){
  title <- paste("barplot for", columnName, sep = " ")
  barplot(table(Dst[[columnName]]), xlab=columnName, main = title, horiz = FALSE)
}

# Correlation Analysis between Numeric Vraiables.

library('corrplot')
cor(na.omit(Dst[numVars]))
corMatrix <- cor(Dst[numVars])
corrplot(cor(na.omit(Dst[numVars])), method = "circle", diag = TRUE)

# ******* DATA ANALYSIS ********

# Plot of Station Id vs Count of Trips on that particular Station Id. 
ggplot(Dst, aes(x = from_station_id)) +
  geom_bar(color = "black", fill = "red") +
  ggtitle("Station Id vs Number of trips") +
  xlab("StationID") +
  ylab("Count of Trips")

# Hour vs number of trips.
ggplot(Dst, aes(x = format(strptime(Dst$startTime, "%H:%M"), "%H"))) +
  geom_bar(color = "black", fill = "red") +
  # geom_text(aes(label="ylab")) +
  ggtitle("Hour vs Number of trips") +
  xlab("Hour") +
  ylab("Number of Trips")

# Year vs number of trips.
ggplot(Dst, aes(x = format(strptime(Dst$startDate, "%Y-%m-%d"), "%Y"))) +
  geom_bar(color = "black", fill = "red") +
  # geom_text(aes(label="ylab")) +
  ggtitle("Year vs Number of trips") +
  xlab("Year") +
  ylab("Number of Trips")

# Month vs number of trips.
ggplot(Dst, aes(x = format(strptime(Dst$startDate, "%Y-%m-%d"), "%m"))) +
  geom_bar(color = "black", fill = "red") +
  # geom_text(aes(label="ylab")) +
  ggtitle("Month vs Number of trips") +
  xlab("Month") +
  ylab("Number of Trips")

# Day vs number of trips.
ggplot(Dst, aes(x = weekdays(as.POSIXct(Dst$startDate), abbreviate = F))) +
  geom_bar(color = "black", fill = "red") +
  # geom_line(weekdays(as.POSIXct(Dst$startDate))) +
  # geom_text(aes(label="ylab")) +
  ggtitle("Month vs Number of trips") +
  xlab("Month") +
  ylab("Number of Trips")


# Gender vs number of trips.
ggplot(Dst, aes(x = Dst$gender)) +
  geom_bar(color = "black", fill = "red") +
  # geom_text(aes(label="ylab")) +
  ggtitle("Gender vs Number of trips") +
  xlab("Gender") +
  ylab("Number of Trips")

# usertype vs number of trips.
ggplot(Dst, aes(x = Dst$usertype)) +
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


