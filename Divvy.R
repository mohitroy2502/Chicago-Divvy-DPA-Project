# Identifying the need and feasibility (revenue) for new stations 
# between 2 stations for providing parking spaces at heavy traffic stations. 
# This is to ensure that divvy does not lose customers, based on the parking docks unavailability.

# Heavy stations are identified as stations from where most trips start and most trips end and not
# whose dock capacity is initially larger because we need to identify stations which are the busiest.

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

# Looking at busiest from and to stations
departures <- table(train_data$from_station_name)
as.matrix(head(sort(departures,decreasing = TRUE)))

arrivals <- table(train_data$to_station_name)
as.matrix(head(sort(arrivals,decreasing = TRUE)))


train_data
#str(divvy_df)                     

## 75% of the sample size
#smp_size <- floor(0.75 * nrow(divvy_df))

## set the seed to make your partition reproducible
#set.seed(123)
#train_ind <- sample(seq_len(nrow(divvy_df)), size = smp_size)

#train <- divvy_df[train_ind, ]
#test <- divvy_df[-train_ind, ]

# Duration of Rentals

train_data$duration.mins <- train_data$tripduration/60
summary(train_data$duration.mins)

sum(train_data$duration.mins > 90) / nrow(train_data)

sum(train_data$duration.mins <= 30) / nrow(train_data)

# Rental Time of Day

train_data$time.hour <- as.numeric(strftime(train_data$starttime, format = "%H"))

# Are Divvy rentals occurring at the same time during the weekends as they are during the weekdays

train_data$day <- weekdays(as.Date(train_data$starttime))
train_data$day <- as.factor(train_data$day)
train_data$day <- factor(train_data$day, levels = c("Sunday", "Monday", "Tuesday", 
                                          "Wednesday","Thursday", "Friday", 
                                          "Saturday"))
train_dataTime <- separate(subset(train_data, !is.na(starttime)), starttime, 
                      c("start.date", "start.time"), sep = " ")
train_dataTime <- subset(train_dataTime, select = c(start.date, time.hour, day))
train_dataTime$time.hour <- as.factor(train_dataTime$time.hour)
train_dataTime.count <- train_dataTime %>%
      group_by(start.date, time.hour) %>%
      summarize(count = n())

# variance of time of rental between the weekday and the weekend.

train_dataTime$day <- as.factor(train_dataTime$day)

train_dataTime.count.weekday <- subset(train_dataTime, day != 'Saturday' & 
                                        day != 'Sunday') %>%
      group_by(start.date, time.hour) %>%
      summarize(count = n())

train_dataTime.count.weekend <- subset(train_dataTime, day == 'Saturday' 
                                  | day == 'Sunday') %>%
      group_by(start.date, time.hour) %>%
      summarize(count = n())


# I am spliting the data by day, station, and user type. This will allow me to 
# count how many renters there are per day per station. I can then find the 
#median number of renters per station per customer type.

train_data.date.split <- separate(subset(train_data, !is.na(starttime)), 
                             starttime, c("start.date", "start.time"), 
                             sep = " ")

CusSub <- train_data.date.split %>%
      group_by(usertype, start.date, from_station_id) %>%
      summarize(count = n()) %>%
      group_by(usertype, from_station_id) %>%
      summarize(median = median(count))


CusSubDiff <- spread(CusSub, usertype, median)
CusSubDiff$difference <- CusSubDiff$Customer - CusSubDiff$Subscriber

CusSubDiff.greatest <- subset(CusSubDiff, difference >= 15 
                              | difference <= -15)
print(CusSubDiff.greatest, max = 250)

DiffStation <- train_data.date.split %>%
      group_by(start.date, from_station_id) %>%
      summarize(median.duration = median(duration.mins)) %>%
      group_by(from_station_id) %>%
      summarize(median.duration = median(median.duration))

CusSub.DiffStation <- inner_join(CusSubDiff.greatest, 
                                 DiffStation, by = "from_station_id")

# Percentage of Renters by Sex and Age

train_data$age.bucket <- cut(train_data$age, breaks = c(16, 24, 34, 44, 54, 64, 74))
ggplot(aes(x = age.bucket, y = ..count../sum(..count..), 
           color = gender, fill = gender), 
       data = subset(train_data, (gender == 'Male' | gender == 'Female') 
                     & !is.na(age.bucket))) +
      geom_bar(position = 'dodge') + 
      scale_y_continuous(labels = percent) +
      ylab("Percentage of Riders")

# Variance in daily ridership

train_dataTime.by.day <- train_dataTime %>%
      group_by(start.date, day) %>%
      summarize(count = n())

aggregate(count~day,train_dataTime.by.day,mean)

# Model Implementations





