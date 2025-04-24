#load library####
library(tidyverse)

#read the phenology data
pheno_early <- read.csv('data/Apfel_Bluehdaten_fruehe_Reifezeit.csv')
pheno_late <- read.csv('data/Apfel_Bluehdaten_spaete_Reifezeit.csv')

#read the weather data interpolated in Python
weather <- arrow::read_feather('data/prediction_t_min_max_1992-2022.feather')

#make sure that each observation of phenology has a corresponding weather data
(unique(pheno_early$Stations_id) %in% unique(weather$Stations_id) == FALSE) %>% 
  sum()
#all ids in pheno early are present in weather
(unique(pheno_late$Stations_id) %in% unique(weather$Stations_id) == FALSE) %>% 
  sum()
#all ids in pheno late are present in weather

#make sure that the years are covered too
year_id <- paste(weather$Stations_id, weather$Referenzjahr, sep = '_')

(paste(pheno_early$Stations_id, pheno_early$Referenzjahr, sep = '_') %in% year_id == FALSE) %>%  
  sum()
#some years of the phenology are not covered by the weather file (also make sure that the year
#prior to the phenology event is also covered, because the season to accumulate chill
#starts in october of the year before)

(paste(pheno_late$Stations_id, pheno_late$Referenzjahr, sep = '_') %in% year_id == FALSE) %>%  
  sum()


#there is less weather than there are phenological records
#--> I need to subset phenology data based on available weather data


weather %>% 
  group_by(Stations_id) %>% 
  summarise(year_start = min(Referenzjahr),
            year_end = max(Referenzjahr))
#so for each station there is the coverage 1993 --> 2022
#filter all phenology observations prior to 1994


pheno_early_sub <- pheno_early %>%
  filter(Referenzjahr > 1993)
pheno_late_sub <- pheno_late %>%
  filter(Referenzjahr > 1993)


#make sure that the weather files are complete
#and bring in chillR format
weather_ready <- weather %>% 
  #filter(Stations_id %in% c(140, 183)) %>% 
  mutate(Tmin = t_min_mean,
         Tmax = t_max_mean,
         Date = lubridate::parse_date_time(date, orders = c('ymd HMS')),
         Year = lubridate::year(Date),
         Month = lubridate::month(Date),
         Day = lubridate::day(Date)) %>% 
  select(Year, Month, Day, Tmin, Tmax, Stations_id) %>% 
  split(f = ~ Stations_id) %>%
  purrr::map(chillR::make_all_day_table) %>% 
  do.call('rbind', .)


#add location of the stations
#needed to calculate the change scenarios
all_pheno_stations<-read.csv("data/all_pheno_stations.csv")
weather_ready<-merge(weather_ready, all_pheno_stations[2:6], by="Stations_id")
new_names<-c(station_name="Stationsname", longitude="geograph.Laenge", latitude="geograph.Breite")
weather_ready<-rename(weather_ready, all_of(new_names) )

weather_ready <- weather_ready %>% 
  select(Day, Month, Year, Tmin, Tmax, Stations_id) %>% 
  mutate(Tmin = round(Tmin, digits = 4),
         Tmax = round(Tmax, digits = 4))

arrow::write_feather(weather_ready, 'data/weather_observed_ready.feather')



weather <- arrow::read_feather('data/weather_observed_ready.feather')

#check out if there is unusual phenology data
pheno_early_sub %>% 
  ggplot(aes(x = Jultag)) +
  geom_histogram() +
  facet_wrap(~Phase_id)

pheno_late_sub %>% 
  ggplot(aes(x = Jultag)) +
  geom_histogram() +
  facet_wrap(~Phase_id)

# we work with phase 5 = start of bloom for further analysis

#look at phenology data
pheno_late_sub %>% 
  filter(Phase_id == 5) %>% 
  summary()

pheno_early_sub %>% 
  filter(Phase_id == 5) %>% 
  summary()


#look at temperature data
weather %>% 
  ggplot(aes(x = Tmin)) +
  geom_histogram() +
  facet_wrap(~Month)

weather %>% 
  ggplot(aes(x = Tmax)) +
  geom_histogram() +
  facet_wrap(~Month)

