#Load libraries####
library(tidyverse)
library(chillR)

#Prepare weather data####

#Information about the stations
DWD_station_lexicon<-read.csv("data/DWD_weather_stations.csv", sep = ";", dec=".", fileEncoding = "latin1")
#Source: https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/historical/KL_Tageswerte_Beschreibung_Stationen.txt

DWD_station_lexicon<-DWD_station_lexicon%>%
  rename(Station_ID=Stations_ID)
DWD_station_lexicon<-DWD_station_lexicon%>%distinct(Station_ID, .keep_all = TRUE)

#automatic download of weather data
for(i in 1992:2022){
  time<-c(paste(i,0101), paste0(i,1231))
  Wetterstationen_t<-na.omit(handle_dwd(action = "list_stations", 
                                        location = c(latitude = 51, longitude = 10),
                                        time_interval = time,
                                        stations_to_choose_from = 10000))
  Wetter_t<-data.table::rbindlist(handle_dwd(action = "download_weather",
                                             location =  Wetterstationen_t$Station_ID,
                                             time_interval = time))
  Wetter_t<-make_JDay(Wetter_t)
  Wetter_t$Station_ID<-as.numeric(Wetter_t$Station_ID)
  assign(paste0("Wetter_",i),merge(Wetter_t, DWD_station_lexicon, by="Station_ID"))
}

#save the weather as csv
for(i in 1992:2022){
  weather<-get(paste0("Wetter_",i))
  write.csv(weather, paste0("data/Wetter_",i,".csv"))
}

# read weather data

for(i in 1992:2022){
  Wetter<-read.csv(paste0("data/Wetter_",i,".csv"), fileEncoding="latin1")
  assign(paste0("weather_",i), Wetter)
}

#combine all weather files
Wetter_all<-rbind(weather_1992,
                  weather_1993,
                  weather_1994,
                  weather_1995,
                  weather_1996,
                  weather_1997,
                  weather_1998,
                  weather_1999,
                  weather_2000,
                  weather_2001,
                  weather_2002,
                  weather_2003,
                  weather_2004,
                  weather_2005,
                  weather_2006,
                  weather_2007,
                  weather_2008,
                  weather_2009,
                  weather_2010,
                  weather_2011,
                  weather_2012,
                  weather_2013,
                  weather_2014,
                  weather_2015,
                  weather_2016,
                  weather_2017,
                  weather_2018,
                  weather_2019,
                  weather_2020,
                  weather_2021,
                  weather_2022)
#save combined weather file
write.csv(Wetter_all,"Wetter_1992_2022.csv")

#Bloom data preparation ####
#Read in the DWD phenology data (only early and late ripening)
#datasets downloaded manually from https://opendata.dwd.de/climate_environment/CDC/observations_germany/phenology/ on 17.08.2023
stations<-read.table("data/Stationen.txt", sep = ";", header = T)
Jahresmelder_1929_2021_fruehe_Reife<-read.table("data/opendata.dwd.de_climate_environment_CDC_observations_germany_phenology_annual_reporters_fruit_historical_PH_Jahresmelder_Obst_Apfel_fruehe_Reife_1929_2021_hist.txt", sep = ";", header = T)
Jahresmelder_1925_2021_spaete_Reife<-read.table("data/opendata.dwd.de_climate_environment_CDC_observations_germany_phenology_annual_reporters_fruit_historical_PH_Jahresmelder_Obst_Apfel_spaete_Reife_1925_2021_hist.txt", sep = ";", header = T)
Jahresmelder_aktuell_fruehe_Reife<-read.table("data/opendata.dwd.de_climate_environment_CDC_observations_germany_phenology_annual_reporters_fruit_recent_PH_Jahresmelder_Obst_Apfel_fruehe_Reife_akt.txt", sep = ";", header = T)
Jahresmelder_aktuell_spaete_Reife<-read.table("data/opendata.dwd.de_climate_environment_CDC_observations_germany_phenology_annual_reporters_fruit_recent_PH_Jahresmelder_Obst_Apfel_spaete_Reife_akt.txt", sep = ";", header = T)
Sofortmelder_2001_2021_spaete_Reife<-read.table("data/opendata.dwd.de_climate_environment_CDC_observations_germany_phenology_immediate_reporters_fruit_historical_PH_Sofortmelder_Obst_Apfel_spaete_Reife_2001_2021_hist.txt", sep = ";", header = T)
Sofortmelder_aktuell_spaete_Reife<-read.table("data/opendata.dwd.de_climate_environment_CDC_observations_germany_phenology_immediate_reporters_fruit_recent_PH_Sofortmelder_Obst_Apfel_spaete_Reife_akt.txt", sep = ";", header = T)


#combine data from different tables
Apfel_alle_fruehe_Reife<-rbind(Jahresmelder_1929_2021_fruehe_Reife, Jahresmelder_aktuell_fruehe_Reife)

Apfel_alle_spaete_Reife<-rbind(Jahresmelder_1925_2021_spaete_Reife, Jahresmelder_aktuell_spaete_Reife)
Apfel_alle_spaete_Reife<-rbind(Apfel_alle_spaete_Reife, Sofortmelder_2001_2021_spaete_Reife)
Apfel_alle_spaete_Reife<-rbind(Apfel_alle_spaete_Reife, Sofortmelder_aktuell_spaete_Reife)

#remove duplicates 
Apfel_alle_fruehe_Reifezeit_2<-Apfel_alle_fruehe_Reife%>%distinct()
Apfel_alle_spaete_Reifezeit_2<-Apfel_alle_spaete_Reife%>%distinct()

#reduce information to begin, full and end of flowering
Apfel_alle_fruehe_Reifezeit_3<-subset(Apfel_alle_fruehe_Reifezeit_2, Apfel_alle_fruehe_Reifezeit_2$Phase_id==5|Apfel_alle_fruehe_Reifezeit_2$Phase_id==6|Apfel_alle_fruehe_Reifezeit_2$Phase_id==7)
Apfel_alle_spaete_Reifezeit_3<-subset(Apfel_alle_spaete_Reifezeit_2, Apfel_alle_spaete_Reifezeit_2$Phase_id==5|Apfel_alle_spaete_Reifezeit_2$Phase_id==6|Apfel_alle_spaete_Reifezeit_2$Phase_id==7)

#add station information
Apfel_alle_fruehe_Reifezeit_4<-merge(Apfel_alle_fruehe_Reifezeit_3, stations, by="Stations_id")
Apfel_alle_spaete_Reifezeit_4<-merge(Apfel_alle_spaete_Reifezeit_3, stations, by="Stations_id")


#find stations where the information are missing in the stations dataset
missing_stations_fruehe <-setdiff(Apfel_alle_fruehe_Reifezeit_3[1:8], Apfel_alle_fruehe_Reifezeit_4[1:8])
id_missing_stations_fruehe<-unique(missing_stations_fruehe$Stations_id)
missing_stations_spaete <-setdiff(Apfel_alle_spaete_Reifezeit_3[1:8], Apfel_alle_spaete_Reifezeit_4[1:8])
id_missing_stations_spaete<-unique(missing_stations_spaete$Stations_id)

id_missing_stations_all<-unique(c(id_missing_stations_fruehe, id_missing_stations_spaete))
id_missing_stations_all

#check if information on those station is in the weather station list
Add_missing_stations<-DWD_station_lexicon[DWD_station_lexicon$Station_ID %in% id_missing_stations_all,]
write.csv(Add_missing_stations, "data/missing_stations_v2.csv")
Add_missing_stations_2<-read.csv("data/missing_stations_v2.csv", sep = ",", dec=".")
Add_missing_stations_2<-Add_missing_stations_2%>%rename(
  Stations_id = Station_ID,
  geograph.Breite = Breite,
  geograph.Laenge = Länge,
  Stationshoehe = Stationshöhe
)
#combine station information
stations_2<-rbind(stations[c(1,2,3,4,5,9)], Add_missing_stations_2[c(2,3,6,7,8,10)])

#remove duplicates
stations_2<-stations_2%>%distinct()

Apfel_alle_fruehe_Reifezeit_5<-merge(Apfel_alle_fruehe_Reifezeit_3, stations_2, by="Stations_id")
Apfel_alle_spaete_Reifezeit_5<-merge(Apfel_alle_spaete_Reifezeit_3, stations_2, by="Stations_id")

write.csv(Apfel_alle_spaete_Reifezeit_5,"data/Apfel_Bluehdaten_spaete_Reifezeit.csv")
write.csv(Apfel_alle_fruehe_Reifezeit_5,"data/Apfel_Bluehdaten_fruehe_Reifezeit.csv")





#blooming date####
###read in bloom data####
flower_dates_late_ripening<-read.csv("data/Apfel_Bluehdaten_spaete_Reifezeit.csv")
flower_dates_early_ripening<-read.csv("data/Apfel_Bluehdaten_fruehe_Reifezeit.csv")
