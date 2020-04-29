# Importing required libraries.
library(lubridate)
library(stringr)
# install.packages("dplyr")
library(dplyr)

# Importing Transformed Datasets.

Q1_2017 <- read.csv('Q1_2017_.csv', header = TRUE)
Q2_2017 <- read.csv('Q2_2017_.csv', header = TRUE)
Q3_2017 <- read.csv('Q3_2017_.csv', header = TRUE)
Q4_2017 <- read.csv('Q4_2017_.csv', header = TRUE)

Q1_2018 <- read.csv('Q1_2018_.csv', header = TRUE)
Q2_2018 <- read.csv('Q2_2018_.csv', header = TRUE)
Q3_2018 <- read.csv('Q3_2018_.csv', header = TRUE)
Q4_2018 <- read.csv('Q4_2018_.csv', header = TRUE)

Q1_2019 <- read.csv('Q1_2019_.csv', header = TRUE)
Q2_2019 <- read.csv('Q2_2019_.csv', header = TRUE)
Q3_2019 <- read.csv('Q3_2019_.csv', header = TRUE)
Q4_2019 <- read.csv('Q4_2019_.csv', header = TRUE)

# Combing Datasets.
Dst <- rbind(Q1_2017, Q2_2017, Q3_2017, Q4_2017, Q1_2018, Q2_2018, Q3_2018, Q4_2018, Q1_2019, Q2_2019, Q3_2019, Q4_2019)

# Splitting Date and Time in two different Columns.
m<-str_split_fixed(Dst$MainDate, " ", 2)   
Dst$MainDate <-m[,1]
Dst$MainTime <-m[,2]

# Data Cleaning: MainDate present in Varied formats.
# Hence, made in a single format. (%Y-%m-%d)
Dst$MainDate <- parse_date_time(Dst$MainDate, orders = c("%m/%d/%Y", "%Y-%m-%d"))

# Droping extra column.
drops <- c("X")
Dst <- Dst[ , !(names(Dst) %in% drops)]

# Importing Weather Data which was obtained from Dark Sky API.

WeatherTest <- read.csv('weatherTrain.csv', header = TRUE)
WeatherTrain <- read.csv('weathertest.csv', header = TRUE)

Weather <- rbind(WeatherTest, WeatherTrain)

Weather$Date <- as.Date(Weather$Date)
Dst$MainDate <- as.Date(Dst$MainDate)

# Merging weather datasets with Original Dataset.
Dst <- merge(Dst, Weather, by.x = "MainDate", by.y = "Date", all.x = T, all.y = F)


# Dealing with Categorial Variables.
Dst$bikeid <- as.factor(Dst$bikeid)
Dst$Weather_Icon <- as.factor(Dst$Weather_Icon)

# Importing Dates vs No of Trips Dataset.
NoOfTripsDate <- read.csv('DateTrips.csv', header = TRUE)

# Data Cleaning: MainDate present in Varied formats.
# Hence, made in a single format. (%Y-%m-%d)
NoOfTripsDate$Date <- parse_date_time(NoOfTripsDate$Date, orders = c("%m/%d/%Y", "%Y-%m-%d"))

NoOfTripsDate$Date <- as.Date(NoOfTripsDate$Date)

# Merging all Datasets.
Dst <- merge(Dst, NoOfTripsDate, by.x = "MainDate", by.y = "Date", all.x = T, all.y = F)

Dst$TotalTrips <- as.numeric(Dst$TotalTrips)

# Condition when bikes needs Maintenance.
# Maintenance = 0 (Dosen't need Maintenance)
# Maintenance = 1 (Needs Maintenance)
Dst$Maintenance <- 0
Dst$Maintenance[(Dst$TotalDuration>144000 | Dst$No.Of.Trips>100) & (Dst$Weather_Icon == "clear-day" | Dst$Weather_Icon == "partly-cloudy-day" | Dst$Weather_Icon == "cloudy") & (Dst$TotalTrips < 15000)] <- 1

# Encoding the target feature as factor.
Dst$Maintenance = factor(Dst$Maintenance, levels = c(0, 1))

# Dividing the main DataSet into Training and Testing Dataset.
# 2017-18 as Training Dataset.
# 2019 as Testing Dataset.
Dst <- as.data.frame(Dst)

Train <- subset(Dst, (Dst$MainDate > "2017-01-01" & Dst$MainDate < "2018-12-31"))
Test <- subset(Dst, (Dst$MainDate > "2019-01-01" & Dst$MainDate < "2019-12-31"))


# Implementing Support Vector Machine.

# install.packages('e1071')
library(e1071)

classifier3 = svm(formula = Maintenance ~ bikeid + TotalDuration + Weather_Icon + No.Of.Trips + MainDate + TotalTrips, data= Train, type= 'C-classification', kernal= 'linear')

# Save the model
save(classifier3, file = 'C:/Users/manis/Desktop/SVM.rda')

y_pred3 = predict(classifier3, newdata = Test)

library(caret)
cm3 = confusionMatrix(reference = Test$Maintenance, data = y_pred3)
cm3

# Visualize the output.

Test$Maintenance = y_pred3

Visualize <- function(x){
  
  xx=unique(Test[Test$MainDate==x & Test$Maintenance==1,c("bikeid", 'Maintenance', "MainDate", "Weather_Icon")])
  xx
}

Visualize1 <- function(x){
  
  xx=unique(Test[Test$MainDate==x & Test$Maintenance==0,c("MainDate", "Weather_Icon")])
  xx
}

Visualize("2019-01-17")
Visualize("2019-02-08")
Visualize("2019-02-18")
