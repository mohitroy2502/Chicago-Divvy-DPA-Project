## Using cleaned data for analysis and exploring all the variables and their relationship.

# Increasing Memory limit
memory.limit(size=56000)

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

## Data Analysis

# Looking at busiest from and to stations
departures <- as.data.frame(table(train_data$from_station_name))
departures<-setNames(departures, c("Station.Name","departs"))
head(departures)

arrivals <- as.data.frame(table(train_data$to_station_name))
arrivals<-setNames(arrivals, c("Station.Name","arrivals"))
head(arrivals)

# Duration of Rentals in Seconds
str(train_data$tripduration)
summary(train_data$tripduration)

# Looking for duration of trips
sum(train_data$duration.mins > 9) / nrow(train_data)

sum(train_data$duration.mins <= 8) / nrow(train_data)
# We find that around 45.6% of the rentals are more than 9 minutes
# Around 46.7% of the rentals are less than or equal to 8 minutes

summary(train_data$Total.Docks)

# Finding number of unique stations
length(unique(unlist(train_data[c("Station.Name")])))
# unique(unlist(train_data[c("Station.Name")]))


checkout_data<- merge(departures,arrivals,by.x="Station.Name",by.y = "Station.Name",all.x = T,all.y = T)
checkout_data<- merge(checkout_data,dock_data,by.x="Station.Name",by.y = "Station.Name",all.x = T,all.y = T)
checkout_data<-checkout_data[-c(6:10)]
colSums(is.na(checkout_data))
checkout_data_remove<-checkout_data[,colSums(is.na(checkout_data))<75]
checkout_data<-na.omit(checkout_data_remove)
checkout_data <- checkout_data[order(-checkout_data$departs, -checkout_data$arrivals),]
top10<-head(checkout_data,10)

summary(checkout_data$departs)
boxplot(checkout_data$departs, main = "Boxplot of departures", ylab = "Total Departures")
boxplot(checkout_data$arrivals, main = "Boxplot of arrivals", ylab = "Total Arrivals")
boxplot(checkout_data$Dock_diff, main = "Boxplot of Arrivals-Departures", ylab = "Arrivals- Departures")
boxplot(checkout_data$Total.Docks, main = "Boxplot of Dock Capacity", ylab = "Dock Capacity")


options(scipen=5)
boxplot(departs~arrivals, data=checkout_data, notch=FALSE,
        col=(c("gold")),
        main="Departure vs Arrivals", xlab="Arrivals")

ggplot(top10, aes(x=departs, y=Station.Name), ) +
  geom_bar(stat = 'identity')+
  coord_flip()

library(ggplot2)
par(mai=c(1,2,1,1))
barplot(height=top10$departs, names=top10$Station.Name, 
        col="#69b3a2",
        horiz=T, las=1
)

# Top 10 stations with most Departures

ggplot(top10, aes(x = Station.Name,y = departs, fill = "departs")) +
  geom_bar(stat='identity',color = "black") +
  ggtitle("Top 10 stations with most Departures") +
  scale_fill_brewer(palette="Dark2")+
  xlab("Station Name") +
  ylab("Number of Departures")

# Top 10 stations with most Arrivals

ggplot(top10, aes(x = Station.Name,y = arrivals, fill = "arrivals")) +
  geom_bar(stat='identity',color = "black", fill="blue1") +
  ggtitle("Top 10 stations with most Arrivals") +
  xlab("Station Name") +
  ylab("Number of Arrivals")

# Density plot of Trip duartion
ggplot(train_data, aes(x = train_data$duration.mins)) +
  geom_density() +
  ggtitle("Density plot of Trip duartion") +
  xlab("Duration in Mins") +
  ylab("Number of Trips")

# Scatterplot of Longitude and Latitude

ggplot(train_data,aes(x=Longitude,y=Latitude)) + 
  geom_point()
