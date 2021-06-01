# DESCRIPTION ----------------------------------------------------------------

#Pulls historical weather data from OpenWeather using own API key.
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

#london lat long
lat = 51.509865
lon = -0.118092

# start and end date for getting hist data - this must be as.numeric from datetime
st_dt<-as.numeric(as.POSIXct(as.Date("2020-06-02")))
end_dt<-as.numeric(as.POSIXct(as.Date("2021-06-01")))
#read API-key from a file in the home/parksinthepandemic directory
API_key = read.table('APIkey_OW.txt')[1,]

# EXAMPLE WITH A SINGLE CALL -----------------------------------------------------

#write url based on parameters
url <- paste0("http://history.openweathermap.org/data/2.5/history/city?",
              "lat=",lat,
              "&lon=", lon,
              "&type=hour",
              "&start=", st_dt,
              "&end=", end_dt,
              "&appid=", API_key)

#read in the JSON/make API call
hist <- fromJSON(url)
#read list of weather data from it
hist_df<-hist$list
#convert date into human-readable format
hist_df$dt<-anytime::anytime(hist_df$dt)

# EXAMPLE WITH A MULTIPLE CALLS TO GET LAST YEAR'S WORTH OF DATA -----------------------------------------------------

hist_all<-c()

# for loop from 0 (yesterday) to 4 (5 days ago)

#loop 0 to 71 because 71*5+10(either end) = 365
for (i in 0:71) {
  print(i)
  # changing datetime by adding 5 days (86400 seconds in a day)
  st_dt <- st_dt+5*86400
  #print date for clarity in loop
  print(anytime(st_dt))
  
  #create url for that date
  url <- paste0("http://history.openweathermap.org/data/2.5/history/city?",
                "lat=",lat,
                "&lon=", lon,
                "&type=hour",
                "&start=", st_dt,
                "&end=", end_dt,
                "&appid=", API_key)
  
   #read in the JSON/make API call
   hist <- fromJSON(url)
   #read list of weather data from it
   hist_df<-hist$list
   #bind rows to make mega dataframe
   hist_all<-dplyr::bind_rows(hist_df,hist_all)
}

#unnest the weather column
weather<-cbind(do.call(rbind,hist$list$weather)
