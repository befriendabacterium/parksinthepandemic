# DESCRIPTION ----------------------------------------------------------------

#Pulls historical and forecast weather data from OpenWeather using own API key.
#Heavily adapated from: http://datacornering.com/how-to-use-openweather-api-in-r/

# START -------------------------------------------------------------------
rm(list=ls())
set.seed(1234)
#set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#move up a directory
setwd("..")
getwd()


# LOAD PACKAGES -----------------------------------------------------------

library(jsonlite)
library(dplyr)
library(tidyr)
library(anytime)

# CODE --------------------------------------------------------------------

#London City ID
city_id = 2643743
#london lat long
lat = 51.509865
lon = -0.118092

#Setting the date
st_dt = as.numeric(as.POSIXct(strftime(Sys.Date()-364)))
end_dt = st_dt + 86400*7

#read API-key from a file in the home/parksinthepandemic directory
API_key = read.table('APIkey_OW.txt')[1,]

# EXAMPLE WITH A SINGLE CALL -----------------------------------------------------

hist_all = c()

for (i in 1:52){
  
  #print date for clarity in loop
  print(anytime(st_dt))
  
  # Rewrite out the url for the later date
  url <- paste0("http://history.openweathermap.org/data/2.5/history/city?id=",city_id
                ,"&type=hour&start=",st_dt
                ,"&end=",end_dt
                ,"&appid=",API_key)
  
  #read in the JSON/make API call
  hist <- fromJSON(url)
  #read list of weather data from it
  hist_df<-hist$list
  #convert date into human-readable format
  hist_df$dt<-anytime::anytime(hist_df$dt)
  #### REFORMING THE DATASET ####
  hist_df$date = as.Date(format(as.POSIXct(hist_df$dt, "%Y-%m-%d %H:%M:%S"),format = "%Y-%m-%d"))
  hist_df$hour = as.numeric(format(as.POSIXct(hist_df$dt, "%Y-%m-%d %H:%M:%S",tz = ""),format = "%H"))
  
  # Extract the variables
  hist_dt = unique(hist_df$date)
  
  for (j in 1:7){
    hist_date = hist_dt[j]
    hist_var = hist_df[hist_df$date == hist_date,]
    
    if (dim(hist_var)[1]==24){
    }else{
      hist_date = hist_date + 1
      hist_var = hist_df[hist_df$date == hist_date,]
      hist_dt = hist_dt[2:length(hist_dt)]
    }
    
    hist_main = hist_var$main 
    hist_wind = hist_var$wind 
    hist_clouds = hist_var$clouds
    hist_rain = hist_var$rain  
    
    hist_df_all = t(data.frame(c("date" = as.character(hist_date),"temp.day" = hist_main$temp[14],"temp.min" = min(hist_main$temp),
    "temp.max" = max(hist_main$temp), "temp.night" = hist_main$temp[24],
    "temp.eve" = hist_main$temp[20], "temp.morn" = hist_main$temp[8],
    "feels_like.day" = hist_main$feels_like[14], "feels_like.night" = hist_main$feels_like[24],
    "feels_like.eve" = hist_main$feels_like[20], "feels_like.morn" = hist_main$feels_like[8],
    "pressure" = hist_main$pressure[14],"humidity" = hist_main$humidity[14],
    "wind_speed" = hist_wind$speed[14], "wind_deg" = hist_wind$deg[14],
    "clouds" = as.numeric(hist_clouds$all[14]), "tot_rain" = sum(hist_rain$`1h`, na.rm = T))))
    
    rownames(hist_df_all)<-NULL
    
    hist_all = rbind.data.frame(hist_all,hist_df_all)
  }
  # Reset dates
  st_dt = st_dt + 86400*7
  end_dt = st_dt + 86400*7
}

# Clear NA created from our if loop, when the date is then repeated.
hist_all <- drop_na(hist_all,temp.day)

# Reset date
st_dt = as.numeric(as.POSIXct(strftime(Sys.Date())))

#write url based on parameters
url_2 <- paste0("http://api.openweathermap.org/data/2.5/onecall?lat=",lat
              ,"&lon=",lon
              ,"&exclue=hourly"
              ,"&appid=",API_key)
#read in the JSON/make API call
fore <- fromJSON(url_2)
#read list of weather data from it
fore_df<-fore$daily
#convert date into human-readable format
fore_df$dt<-anytime::anytime(fore_df$dt)

fore_df$date = as.Date(format(as.POSIXct(fore_df$dt, "%Y-%m-%d %H:%M:%S"),format = "%Y-%m-%d"))

fore_temp = fore_df$temp
fore_feels_like = fore_df$feels_like 

fore_df_all = cbind.data.frame("date" = as.character(fore_df$date),"temp.day" = fore_temp$day,"temp.min" = fore_temp$min,
                             "temp.max" = fore_temp$max , "temp.night" = fore_temp$night ,
                             "temp.eve" = fore_temp$eve , "temp.morn" = fore_temp$morn ,
                             "feels_like.day" = fore_feels_like$day , "feels_like.night" = fore_feels_like$night,
                             "feels_like.eve" = fore_feels_like$eve , "feels_like.morn" = fore_feels_like$morn ,
                             "pressure" = fore_df$pressure ,"humidity" = fore_df$humidity ,
                             "wind_speed" = fore_df$wind_speed , "wind_deg" = fore_df$wind_deg,
                             "clouds" = fore_df$clouds, "tot_rain" = fore_df$rain)

hist_all = rbind(hist_all,fore_df_all)
hist_all$date = as.Date(hist_all$date)

#Presume all NA in tot rain is = 0
hist_all$tot_rain[is.na(hist_all$tot_rain)]<-0

#save(hist_all,file = "hist_and_fore.csv")