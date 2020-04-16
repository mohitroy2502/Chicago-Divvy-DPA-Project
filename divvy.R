library(stringr)
library(chron)

Divvy2018=read.csv("divvy2018.csv")
names(Divvy2018)<-c('trip_id','start_time','end_time','bikeid','tripduration','from_station_id','from_station_name','to_station_id','to_station_name','usertype','gender','birthyear')
start=str_split_fixed(Divvy2018$start_time,' ',2)
Divvy2018$`Start Date`=as.Date(start[,1])
Divvy2018$`Start Time`=chron(times=start[,2])
end=str_split_fixed(Divvy2018$end_time,' ',2)
Divvy2018$`End Date`=as.Date(end[,1])
Divvy2018$`End Time`=chron(times=end[,2])

Divvy=read.csv("divvy2019.csv")
names(Divvy)<-c('trip_id','start_time','end_time','bikeid','tripduration','from_station_id','from_station_name','to_station_id','to_station_name','usertype','gender','birthyear')
start=str_split_fixed(Divvy$start_time,' ',2)
Divvy$`Start Date`=as.Date(start[,1])
Divvy$`Start Time`=chron(times=start[,2])
end=str_split_fixed(Divvy$end_time,' ',2)
Divvy$`End Date`=as.Date(end[,1])
Divvy$`End Time`=chron(times=end[,2])

Divvy2017=read.csv("divvy2017.csv")
names(Divvy2017)<-c('trip_id','start_time','end_time','bikeid','tripduration','from_station_id','from_station_name','to_station_id','to_station_name','usertype','gender','birthyear')
start=str_split_fixed(Divvy2017$start_time,' ',2)
Divvy2017$`Start Date`=as.Date(start[,1],format="%m/%d/%Y")
Divvy2017$`Start Time`=chron(times=start[,2])
end=str_split_fixed(Divvy2017$end_time,' ',2)
Divvy2017$`End Date`=as.Date(end[,1],format="%m/%d/%Y")
Divvy2017$`End Time`=chron(times=end[,2])

library(plyr)
library(lubridate)
library(dplyr)
div=Divvy
div$StartMonth=month(div$`Start Date`)
tab=table(div$StartMonth)/365
format(tab / 1e5, trim = TRUE)
options(scipen = 5)
barplot(tab,names.arg=names(tab),xlab="Year 2019",col="blue",
        main="Daily Avg Trips in 2019 Month wise",border="red",las=1)

div1=Divvy2018
div1$StartMonth=month(div1$`Start Date`)
tab1=table(div1$StartMonth)/365
format(tab / 1e5, trim = TRUE)
options(scipen = 5)
barplot(tab1,names.arg=names(tab1),xlab="Year 2018",col="blue",
        main="Daily Avg Trips in 2018 Month wise",border="red",las=1)

DivvyCopy<-Divvy
toDrop <- c("start_time","end_time")
DivvyCopy<-DivvyCopy[ , !(names(DivvyCopy) %in% toDrop)]

library(ggmap)
library(ggplot2)
library(maps)
as.data.frame(table(DivvyCopy$from_station_name))
chicago <- get_map(location = 'chicago', zoom = 13)
Stations<-read.csv("Divvy_Bicycle_Stations.csv")

st<-as.data.frame(table(DivvyCopy$from_station_id))
colnames(st)<-c('ID','Frequency')
st$DailyAvg<-trunc(st$Frequency/365)
d<-merge(Stations,st,by.x = 'ID')
Divvy2017<-Divvy2017[order(Divvy2017$`Start Date`),]
divvyTrain<-rbind(Divvy2017,Divvy2018)

#divvyTrain_bydate<-as.data.frame(table(divvyTrain$`Start Date`))
divvyTrain$startTimeHour <- as.numeric(str_split_fixed(divvyTrain$`Start Time`,":",3)[,1])
divvyTrain[divvyTrain$`Start Date` %in% dates,]$startTimeHour <- 
  as.numeric(str_split_fixed(str_split_fixed(divvyTrain[divvyTrain$`Start Date` %in% dates,]$start_time," ",2)[,2],":",2)[,1])
divvyTrain<-divvyTrain[divvyTrain$from_station_id==35,]


divvyTrain_bydatehour <- divvyTrain %>% group_by(`Start Date`,startTimeHour) %>% summarize(trips=n())
colnames(divvyTrain_bydatehour)<-c('startDate','hour','trips')
divvyTrain_bydatehour$startDate<-as.Date(divvyTrain_bydatehour$startDate)
divvyTrain_bydatehour$trips<-as.numeric(divvyTrain_bydatehour$trips)

#rem_data<-data.frame(Date=as.Date(c("2018-02-04","2018-02-09","2018-02-10","2018-02-11")),Hourly_Average_Trips=c(0,0,0,0))
#rem_data$Date<-as.Date(rem_data$Date)
#divvyTrain_bydate<-rbind(divvyTrain_bydate,rem_data)
miss_dates<-as.Date(c("2018-02-04","2018-02-09","2018-02-10","2018-02-11"))
hours<-c(0:23)
rem_data<-data.frame()
for(date in miss_dates)
{
  for(hour in hours)
  {
    rem_data <- rbind(rem_data,c(date,hour,0))
  }
}
colnames(rem_data) <- c('startDate','hour','trips')
for(date in unique(divvyTrain_bydatehour$startDate))
{
  for(hour in hours)
  {
    if(!(hour %in% divvyTrain_bydatehour[divvyTrain_bydatehour$startDate==date,]$hour))
    {
      rem_data <- rbind(rem_data,c(date,hour,0))
    }
  }
}
rem_data$startDate <- as.Date(rem_data$startDate,origin="1970-01-01")
divvyTrain_bydatehour$trips <- as.numeric(divvyTrain_bydatehour$trips)
divvyTrain_bydatehour<-rbind(as.data.frame(divvyTrain_bydatehour),rem_data)
divvyTrain_bydatehour<-divvyTrain_bydatehour[order(divvyTrain_bydatehour$startDate,divvyTrain_bydatehour$hour),]
rownames(divvyTrain_bydatehour) <- NULL

write.table(divvyTrain_bydatehour,"DivvyTripDataforID35Train.csv",col.names = TRUE,row.names=FALSE,sep=',')

divvytest<-Divvy[Divvy$from_station_id==35,]
divvytest$startTimeHour <- as.numeric(str_split_fixed(divvytest$`Start Time`,':',3)[,1])

divvytest_bydatehour <- as.data.frame(divvytest %>% group_by(`Start Date`,startTimeHour) %>% summarize(trips=n()))
#divvytest_bydate<-as.data.frame(table(divvytest$`Start Date`))

colnames(divvytest_bydatehour)<-c('startDate','hour','trips')
#divvytest_bydate$Hourly_Average_Trips<-ceiling(divvytest_bydate$Hourly_Average_Trips/24)
rem_datatest<-data.frame()
for(date in unique(divvytest_bydatehour$startDate))
{
  for(hour in hours)
  {
    if(!(hour %in% divvytest_bydatehour[divvytest_bydatehour$startDate==date,]$hour))
    {
      rem_datatest <- rbind(rem_datatest,c(date,hour,0))
    }
  }
}
colnames(rem_datatest)<-c('startDate','hour','trips')
rem_datatest$startDate<-as.Date(rem_datatest$startDate,origin="1970-01-01")
divvytest_bydatehour<-rbind(as.data.frame(divvytest_bydatehour),rem_datatest)
divvytest_bydatehour<-divvytest_bydatehour[order(divvytest_bydatehour$startDate,divvytest_bydatehour$hour),]
row.names(divvytest_bydatehour) <- NULL

write.table(divvytest_bydatehour,"DivvyTripDataforID35Test.csv",col.names = TRUE,row.names=FALSE,sep=',')

#All divvy stations
ggmap(chicago) +
  geom_point(data = Stations,
             mapping = aes(x = Longitude,
                           y = Latitude),
             colour="dodgerblue")+
  ggtitle("Stations in 2019")
#All divvy stations bubble map based on total sum of trips
ggmap(chicago) +
geom_point(data = d, aes(x = Longitude, y = Latitude, size = Frequency),
               shape = 21, colour = "dodgerblue4", fill = "dodgerblue", 
               alpha = .5) +
  ggtitle("Total number of trips from stations in 2019")

#All divvy stations bubble map based on daily avg of trips
ggmap(chicago) +
  geom_point(data = d, aes(x = Longitude, y = Latitude, size = DailyAvg),
             shape = 21, colour = "dodgerblue4", fill = "dodgerblue", 
             alpha = .5) +
  scale_size_area(max_size = 5) +
  ggtitle("Daily Avg number of trips from stations in 2019")
#for ID=35
library(RSocrata)
df<-read.socrata(
  "https://data.cityofchicago.org/resource/eq45-8inv.csv?id=35",
  app_token = "MS1kqh9EMeAImOlbSZ71bfFJv",
  email     = "kkini1@hawk.iit.edu",
  password  = "Kashika3#"
)

library(dplyr)
df1<-df
df1$timestamp<-as.POSIXct(df1$timestamp)
df1<-df1[year(df1$timestamp) %in% 2017:2019,c("timestamp","available_bikes")]
df1$date<-date(df1$timestamp)
df1$hour<-hour(df1$timestamp)
df1_date<-df1 %>% group_by(date,hour) %>% summarise(sum_bikes=ceiling(mean(available_bikes)))
#df1_bydate<-as.data.frame(aggregate(df1_date$sum_bikes, by=list(Date=df1_date$date), FUN=sum))
df1_date<-df1_date[,c("date","hour","sum_bikes")]
colnames(df1_date)<-c("Date","Hour","Available_bikes")
#df1_bydate$Hourly_Avg_avl_bikes<-ceiling(df1_bydate$Hourly_Avg_avl_bikes/24)
write.table(df1_date,"DivvyAvlBikesID35.csv",col.names = TRUE,row.names=FALSE,sep=',')
