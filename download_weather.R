library(httr)
library(jsonlite)
library(lubridate)
library(rlist)
library(tidyverse)
library(dplyr)
library(stringr)

start_date<-as.Date("2019-01-01 00:00:00")
end_date<-as.Date("2019-12-31 00:00:00")
dates<-seq(start_date,end_date,by="days")
dates<-as.numeric(as.POSIXct(dates,"UTC",origin = "1970-01-01"))
username<-"karthikkini1234@gmail.com"
password<-"Kashika3#"
jsons<-list()
i<-1
for (dt in dates) 
{
  url<-paste("https://api.darksky.net/forecast/61339836293844f7e646a5e47ed33c55/41.8781, -87.6298,",dt)
  get_data <- GET(url, authenticate(username,password, type = "basic"))
  get_data_text<-content(get_data,"text")
  get_data_json <- fromJSON(get_data_text, flatten = TRUE)
  jsons[[i]]<-get_data_json
  i<-i+1
  #weather_icon<-c(weather_icon,get_data_json[6]$daily$data$icon)
}
i<-1
weather<-data.frame()
for(j in jsons)
{
  weather<-rbind(weather,j[5]$hourly$data[,c('time','icon')])
  i<-i+1
}
weather$time <- as.POSIXct(weather$time,origin="1970-01-01 00:00:00")
weather$time <- as.numeric(weather$time)


start_date<-as.Date("2018-01-01 00:00:00")
end_date<-as.Date("2018-12-30 00:00:00")
dates<-seq(start_date,end_date,by="days")
dates<-as.numeric(as.POSIXct(dates,"UTC",origin = "1970-01-01"))
username<-"karthikkini1234@gmail.com"
password<-"Kashika3#"
jsons<-list()
i<-1
for (dt in dates) 
{
  url<-paste("https://api.darksky.net/forecast/61339836293844f7e646a5e47ed33c55/41.8781, -87.6298,",dt)
  get_data <- GET(url, authenticate(username,password, type = "basic"))
  get_data_text<-content(get_data,"text")
  get_data_json <- fromJSON(get_data_text, flatten = TRUE)
  jsons[[i]]<-get_data_json
  i<-i+1
}
for(j in jsons)
{
  weather<-rbind(weather,j[5]$hourly$data[,c('time','icon')])
}
# weather2018<-as.data.frame(seq(start_date,end_date,by="days"))
# names(weather2018)<-"Date"
# weather_icon<-append(weather_icon,"partly-cloudy-day",168)
# weather2018$Weather_Icon<-weather_icon

start_date<-as.Date("2017-01-02 00:00:00")
end_date<-as.Date("2017-12-30 00:00:00")
dates<-seq(start_date,end_date,by="days")
dates<-as.numeric(as.POSIXct(dates,"UTC",origin = "1970-01-01"))
username<-"kkini1@hawk.iit.edu"
password<-"Kashika3#"
weather_icon<-list()
jsons<-list()
i<-1
for (dt in dates) 
{
  url<-paste("https://api.darksky.net/forecast/9d855b766c1cbca72e1461a3e7e476e6/41.8781, -87.6298,",dt)
  get_data <- GET(url, authenticate(username,password, type = "basic"))
  get_data_text<-content(get_data,"text")
  get_data_json <- fromJSON(get_data_text, flatten = TRUE)
  jsons[[i]]<-get_data_json
  i<-i+1
  #weather_icon<-c(weather_icon,get_data_json[6]$daily$data$icon)
}
for(j in jsons)
{
  weather<-rbind(weather,j[5]$hourly$data[,c('time','icon')])
}
# weather2017<-as.data.frame(dates)
# names(weather2017)<-"Date"
# weather_icon<-append(weather_icon,"partly-cloudy-day",333)
# weather2017$Weather_Icon<-weather_icon
# weather$Weather_Icon[229]<-'cloudy'
# weatherTrain<-rbind(weather2017,weather2018)
# weatherTest<-weather
# 
# 
# weatherTrain$Weather_Icon<-unlist(weatherTrain$Weather_Icon)
# write.table(weatherTrain,"weatherTrain.csv",col.names = TRUE,row.names=FALSE,sep = ',')
# weatherTest$Weather_Icon<-unlist(weatherTest$Weather_Icon)
colnames(weather) <- c('time','icon','startDate','hour')
write.table(weather,"weather.csv",col.names = TRUE,row.names=FALSE,sep = ',')
