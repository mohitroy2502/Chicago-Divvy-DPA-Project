library(httr)
library(jsonlite)
library(lubridate)
library(rlist)
library(tidyverse)
library(dplyr)
library(stringr)
 
start_date<-as.Date("2017-01-01 00:00:00")
end_date<-as.Date("2017-12-31 00:00:00")
dates<-seq(start_date,end_date,by="days")
dates<-as.numeric(as.POSIXct(dates,"UTC",origin = "1970-01-01 00:00:00"))
username<-""
password<-""
jsons<-list()
i<-1
for (dt in dates) 
{
  url<-paste("https://api.darksky.net/forecast/<<API_KEY>>/41.8781, -87.6298,",dt)
  get_data <- GET(url, authenticate(username,password, type = "basic"))
  get_data_text<-content(get_data,"text")
  get_data_json <- fromJSON(get_data_text, flatten = TRUE)
  jsons[[i]]<-get_data_json
  i<-i+1
}
weather_2017<-data.frame()
for(j in jsons)
{
  weather_2017<-rbind(weather_2017,j[5]$hourly$data[,c('time','icon')])
}
weather_2017$date <- as.POSIXct(weather_2017$time,origin="1970-01-01 00:00:00")
weather_2017$date<-str_split_fixed(weather_2017$date," ",2)[,1]

start_date<-as.Date("2018-01-01 00:00:00")
end_date<-as.Date("2019-12-31 00:00:00")
dates<-seq(start_date,end_date,by="days")
dates<-as.numeric(as.POSIXct(dates,"UTC",origin = "1970-01-01"))
username<-""
password<-""
jsons<-list()
i<-1
for (dt in dates) 
{
  url<-paste("https://api.darksky.net/forecast/<<API_KEY>>/41.8781, -87.6298,",dt)
  get_data <- GET(url, authenticate(username,password, type = "basic"))
  get_data_text<-content(get_data,"text")
  get_data_json <- fromJSON(get_data_text, flatten = TRUE)
  jsons[[i]]<-get_data_json
  i<-i+1
}
weather_2018 <- data.frame()
for(j in jsons)
{
  weather_2018<-rbind(weather_2018,j[5]$hourly$data[,c('time','icon')])
}
weather_2018$date <- as.POSIXct(weather_2018$time,origin="1970-01-01 00:00:00")
weather_2018$date<-str_split_fixed(weather_2018$date," ",2)[,1]

start_date<-as.Date("2019-01-01 00:00:00")
end_date<-as.Date("2019-12-31 00:00:00")
dates<-seq(start_date,end_date,by="days")
dates<-as.numeric(as.POSIXct(dates,"UTC",origin = "1970-01-01"))
username<-""
password<-""
jsons<-list()
i<-1
for (dt in dates) 
{
  url<-paste("https://api.darksky.net/forecast/<<API_KEY>>/41.8781, -87.6298,",dt)
  get_data <- GET(url, authenticate(username,password, type = "basic"))
  get_data_text<-content(get_data,"text")
  get_data_json <- fromJSON(get_data_text, flatten = TRUE)
  jsons[[i]]<-get_data_json
  i<-i+1
}
weather_2019 <- data.frame()
for(j in jsons)
{
  weather_2019<-rbind(weather_2019,j[5]$hourly$data[,c('time','icon')])
}
weather_2019$date <- as.POSIXct(weather_2019$time,origin="1970-01-01 00:00:00")
weather_2019$date<-str_split_fixed(weather_2019$date," ",2)[,1]

#Creating Train and Test weather data
weatherTrain<-rbind(weather_2017,weather_2018)
weatherTest<-weather_2019
date_split <- str_split_fixed(weatherTrain$date," ",2)
weatherTrain$date<-as.Date(date_split[,1])
weatherTrain$hour <- as.numeric(str_split_fixed(date_split[,2],":",3)[,1])
date_split_test <- str_split_fixed(weatherTest$date," ",2)
weatherTest$date<-as.Date(date_split_test[,1])
weatherTest$hour <- as.numeric(str_split_fixed(date_split_test[,2],":",3)[,1])

write.csv(weatherTrain[,c('date','hour','icon')],"weatherTrain.csv",row.names = FALSE,col.names = TRUE)
write.csv(weatherTest[,c('date','hour','icon')],"weatherTest.csv",row.names = FALSE,col.names = TRUE)
