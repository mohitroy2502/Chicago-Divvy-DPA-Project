library(httr)
library(jsonlite)
library(lubridate)
url<-"https://api.darksky.net/forecast/9d855b766c1cbca72e1461a3e7e476e6/41.8781, -87.6298,1514764800"
username<-"kkini1@hawk.iit.edu"
password<-"Kashika3#"
get_data <- GET(url, authenticate(username,password, type = "basic"))
get_data_text<-content(get_data,"text")
get_data_json <- fromJSON(get_data_text, flatten = TRUE)
get_data_df <- as.data.frame(get_data_json)
url<-"https://api.darksky.net/forecast/9d855b766c1cbca72e1461a3e7e476e6/41.8781, -87.6298,1546304400"
get_data1 <- GET(url, authenticate(username,password, type = "basic"))
get_data_text1<-content(get_data1,"text")
get_data_json1 <- fromJSON(get_data_text1, flatten = TRUE)
get_data_df1 <- as.data.frame(get_data_json1)

date<-vector()
weather_summary<-vector()
jsons<-list()
start_date<-as.Date("2018-01-01 12:00:00")
end_date<-as.Date("2019-12-31 12:00:00")
dates<-seq(start_date,end_date,by="days")
username<-"kkini1@hawk.iit.edu"
password<-"Kashika3#"
for (dt in dates) {
  dtPost<-as.numeric(as.POSIXct(dt,"UTC",origin = "1970-01-01"))
  url<-paste("https://api.darksky.net/forecast/9d855b766c1cbca72e1461a3e7e476e6/41.8781, -87.6298,",dtPost)
  get_data <- GET(url, authenticate(username,password, type = "basic"))
  get_data_text<-content(get_data,"text")
  get_data_json <- fromJSON(get_data_text, flatten = TRUE)
  list.append(jsons,get_data_json)
  date<-c(date,dt)
  weather_summary<-c(weather_summary,get_data_json[4]$currently$summary)
}
weather<-as.data.frame(dates)
weather$Weather_Summary<-weather_summary
