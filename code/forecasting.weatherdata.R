forecasting.weatherdata<-function(){
  
# Load packages ----------------------------------------------------------
##Calculate baseline

source('match.metoffice2google.R')

#install.packages('plotrix')
library(plotrix)
#install.packages('tibble')
library(tibble)
#install.packages('ggplot2')
library(ggplot2)
#install.packages('gridExtra')
library(gridExtra)
#install.packages('grid)
library(grid)
#install.packages('lattice')
library(lattice)
#install.packages("tidyr")
library(tidyr)
#install.packages('reshape2')
library(reshape2)
#install.packages('dplyr')
library(dplyr)
#install.packages('XML')
library(XML) # HTML processing
#install.packages('RCurl')
library(RCurl)
#install.packages('rvest')
library(rvest)
#install.packages('stringr')
library(stringr)
#install.packages("owmr")
library(owmr)

#Reads in the metoffice_England data
metoffice_england<-match.metoffice2google()
#Add a column for weekdays within the met_office data
metoffice_england<-metoffice_england %>% tibble::add_column(weekdays = weekdays(as.Date(metoffice_england$date)), 
                                                            .before = 1)
#Converts all the weather data to be compatible with the open wather data.
metoffice_england<-metoffice_england%>%
  mutate(
    precipitation_flux_max_mean..kg.m.2.s.1.= precipitation_flux_max_mean..kg.m.2.s.1.*86400,
    precipitation_flux_mean_mean..kg.m.2.s.1.= precipitation_flux_mean_mean..kg.m.2.s.1.*86400,
    precipitation_flux_max_variance..m.4.kg2.s.2. = precipitation_flux_max_variance..m.4.kg2.s.2.*86400,
    precipitation_flux_mean_variance..m.4.kg2.s.2. = precipitation_flux_mean_variance..m.4.kg2.s.2.*86400,
    air_temperature_max_mean..K. = air_temperature_max_mean..K. - 273.15,
    air_temperature_mean_mean..K. = air_temperature_mean_mean..K. - 273.15,
    air_temperature_min_mean..K. = air_temperature_min_mean..K. - 273.15,
    air_temperature_max_variance..K2. = (sqrt(air_temperature_max_variance..K2.) - 273)^2,
    air_temperature_mean_variance..K2. = (sqrt(air_temperature_mean_variance..K2.) - 273)^2,
    air_temperature_min_variance..K2. = (sqrt(air_temperature_min_variance..K2.)-273)^2)

#make a vector of dates within Google's non-baseline/time series period
nonbaselinerange<-seq(as.Date('2020-02-15'),as.Date('2020-06-27'),1)
#get met office england data for the nonbaseline period
metoffice_england_nonbaselineperiod<-metoffice_england[as.Date(metoffice_england$date)%in%nonbaselinerange,]

#make a vector of dates within Google's Baseline period
baselinerange<-seq(as.Date('2020-01-03'),as.Date('2020-02-06'),1)
#get met office england data for the baseline period
metoffice_england_baselineperiod<-metoffice_england[as.Date(metoffice_england$date)%in%baselinerange,]

unique(as.Date(metoffice_england$date))
unique(as.Date(baselinerange))

#create a dataframe of the median values of each meteorological measurement for each district, from the baseline period (as google did for mobility)
baselineweather<-aggregate(metoffice_england_baselineperiod,
                           list(metoffice_england_baselineperiod$weekdays,
                                metoffice_england_baselineperiod$sub_region_1),
                           median)[,-c(1:2)]

# Inserting in the open weather data --------------------------------------

#Set's the system to my own API key.
Sys.setenv(OWM_API_KEY = "31af4c38fca8bb4ec366db9695bebdc0")
#This gets forecasted weather for the specified "district"
  forecast <-get_forecast("Bedford", units = "metric")%>%
    owmr_as_tibble()

#This extracts the date and time column into two seperate strings.
date_time<-t(as.data.frame(strsplit(forecast$dt_txt," ")))
#Then combines them to the main data frame.
forecast<-as.data.frame(cbind("date" = (as.Date(date_time[,1])), "time" = date_time[,2],forecast))
#This works out the first two dates with the full sets of data for a whole day.
date_keep<-subset(count(forecast$date), as.numeric(freq) >= 8)[,1]

#This creates dataframe that contains the first two full days and their complimentary data values for mean aggregation.
forecast_final<-rbind(subset(forecast,date == date_keep[[1]],select = c("date","temp","temp_min","temp_max")),
                  subset(forecast,date == date_keep[[2]],select = c("date","temp","temp_min","temp_max")))
#This calculates the mean value for each value extracted from above.   
forecast_final<-aggregate(forecast_final, list(forecast_final$date), FUN = mean)

#This contains only data for precipitation which needs to be aggregated via a sum function.  
forecast_rainfall<-rbind(subset(forecast,date == date_keep[[1]],select = c("date","rain_3h")),
                           subset(forecast,date == date_keep[[2]],select = c("date","rain_3h")))
#This calculates the sum for each value extracted from above.  
forecast_rainfall<-aggregate(forecast_rainfall$rain_3h, list(forecast_rainfall$date), FUN = sum)

#This combines the two dataframes together and negates the second row.  
forecast_final<-cbind("weekdays" = weekdays(forecast_final$date),forecast_final, "rain_3h" = forecast_rainfall$x)[,-2]

#This subsets out the metoffice data frame to only contain values for Bedford and the required baseline values needed for calculation.
baselineweather_district<-subset(baselineweather, sub_region_1 == "Bedford",select = c("weekdays","date","air_temperature_mean_mean..K.", "air_temperature_min_mean..K.","air_temperature_max_mean..K.","precipitation_flux_mean_mean..kg.m.2.s.1."))
#This sets the column names to the same names as the open weather data.
colnames(baselineweather_district)<- c("weekdays","date","temp","temp_min","temp_max","rain_3h")

#This extracts the weekdays for the first two days.
weekday<-forecast_final$weekdays
#This extracts the weekday baselines that are required for calculating the baseline 
weekday_baseline<-rbind(subset(baselineweather_district, weekdays == weekday[[1]]),
                                subset(baselineweather_district, weekdays == weekday[[2]]))
#This manipulates the forecast_final dataframe to work out the open weather data with respect to the metoffice baseline.
forecast_final<-forecast_final%>%
    mutate(
      temp = temp - weekday_baseline$temp,
      temp_min = temp_min - weekday_baseline$temp_min,
      temp_max = temp_max - weekday_baseline$temp_max,
      rain_3h = rain_3h - weekday_baseline$rain_3h
    )
}
