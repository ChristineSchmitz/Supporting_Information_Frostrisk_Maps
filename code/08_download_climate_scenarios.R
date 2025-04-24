#load libraries####
library(chillR)
library(tidyverse)
library(beepr)
#source("username_pw_cs.R") #load my personal login data

#download future climate data####

#define area to download
area <- c(56, 5, 47, 15.5)

#download future climate from https://cds.climate.copernicus.eu/
download_cmip6_ecmwfr(
  #scenarios = 'ssp126', 
  scenarios = c('ssp126', "ssp245", "ssp370", "ssp585"), 
  area =  area,
  user = user_Christine,#add your personal login data here
  key = key_Christine,#add your personal login data here
  model = 'default',
  frequency = 'monthly',
  variable = c('Tmin', 'Tmax'),
  year_start = 2015,
  year_end = 2100)
beep(8)#gives a sound when the task is done

download_baseline_cmip6_ecmwfr(
  area = area,
  user = user_Christine,
  key = key_Christine,
  model = 'match_downloaded',
  frequency = 'monthly',
  variable = c('Tmin', 'Tmax'),
  year_start = 1986, 
  year_end = 2014, 
  month = 1:12)
beep(8)

#calculate the change between past and future climate####
#read in historical weather data
weather <- arrow::read_feather('data/weather_observed_ready.feather')

station_infos<-weather[c(1,8:11)]

pheno_stations<-station_infos%>%distinct()

#extract future Temps at the phenoe station locations
extracted <- extract_cmip6_data(stations = pheno_stations)

#calculate change scenarios
change_scenarios <- gen_rel_change_scenario(extracted)
write.csv(change_scenarios, "data/change_scenarios.csv", row.names = FALSE)
beep(8)

#read in saved change scenarios
change_scenarios<-read.csv("data/change_scenarios.csv")

