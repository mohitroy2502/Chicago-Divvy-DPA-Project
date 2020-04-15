# Importing required libraries.
library(lubridate)
library(stringr)
# install.packages("dplyr")
library(dplyr)
library(data.table)
library(ggplot2)

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

# Importing Weather Data.
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


# Condition when bikes needs Maintenance.
# Maintenance = 0 (Dosen't need Maintenance)
# Maintenance = 1 (Needs Maintenance)
Dst$Maintenance <- 0
Dst$Maintenance[(Dst$TotalDuration>110000 | Dst$No.Of.Trips>80) & (Dst$Weather_Icon == "clear-day" | Dst$Weather_Icon == "partly-cloudy-day" | Dst$Weather_Icon == "cloudy")] <- 1

# Dividing Training and Testing Dataset.
# library(caTools)
# set.seed(1234)
# split = sample.split(Dst$from_station_name, SplitRatio = 0.8)
# TrainDst = subset(Dst, split == TRUE)
# TestDst = subset(Dst, split == FALSE)
# library('caret')
# set.seed(1234)
# partition <- createDataPartition(y = Dst$from_station_name, p = 0.8, list = FALSE)
# TrainDst <- Dst[partition,]
# TestDst <- Dst[-partition,]
# stopifnot(nrow(TrainDst) + nrow(TestDst) == nrow(Dst))

# Dividing the main DataSet into Training and Testing Dataset.
# 2017-18 as Training Dataset.
# 2019 as Testing Dataset.

Dst <- as.data.frame(Dst)

Train <- subset(Dst, (Dst$MainDate > "2017-01-01" & Dst$MainDate < "2018-12-31"))
Test <- subset(Dst, (Dst$MainDate > "2019-01-01" & Dst$MainDate < "2019-12-31"))

# Implementing Decision Tree Classification.

# Encoding the target feature as factor.
Dst$Maintenance = factor(Dst$Maintenance, levels = c(0, 1))

# Fitting Decision Tree Classification to the Training set
# install.packages('rpart')
library(rpart)
classifier1 = rpart(formula = Maintenance ~ bikeid + TotalDuration + No.Of.Trips + Weather_Icon + MainDate,
                    data = Train)

y_pred = predict(classifier, newdata = Test, type = 'class')

# Making the Confusion Matrix
# cm = table(Test, y_pred)

# Adam
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(classifier1)
