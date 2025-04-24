#randomly split phenology data per location in calibration and validation dataset

#load library####
library(tidyverse)


#load phenology data####
pheno_early <- read.csv('data/Apfel_Bluehdaten_fruehe_Reifezeit.csv') %>% 
  filter(Referenzjahr >= 1993)
pheno_late <- read.csv('data/Apfel_Bluehdaten_spaete_Reifezeit.csv') %>% 
  filter(Referenzjahr >= 1993)

#Spilt dataset in calibration and validation####
#also choose some random locations that we do not consider in the calibration
#how many locations we completely set asid
n_loc_out <- 50
#share of the calibration data
share_cal <- 0.75
#which phase id to analyse
#5 = start of bloom
#6 = full bloom
phase <- 5


set.seed(1234567879)
loc_val <- pheno_early$Stations_id %>% 
  unique() %>% 
  sample(size = n_loc_out)

pheno_early_sub <- pheno_early %>% 
  filter(Stations_id %in% loc_val == FALSE)
pheno_late_sub <- pheno_late %>% 
  filter(Stations_id %in% loc_val == FALSE)


#ids of the locations for the calibration
ids <- unique(pheno_early_sub$Stations_id)

calibration_df <- data.frame()
validation_df <- data.frame()

#for each id, take 75% for calibration
set.seed(1234567879)
for(i in 1:length(ids)){
  #subset pheno data
  sub <- pheno_early_sub %>% 
    filter(Stations_id == ids[i],
           Phase_id == phase) %>% 
    mutate(id = Stations_id,
           latitude = geograph.Breite,
           longitude = geograph.Laenge,
           year = Referenzjahr,
           yday = Jultag, 
           name = Stationsname) %>% 
    select(id, name, latitude, longitude, year, yday)
  
  #calibration dataset size
  n_cal <- floor(share_cal * nrow(sub))
  
  row_cal <- sample(1:nrow(sub),size = n_cal) %>% 
    sort()
  
  calibration_df <- rbind(calibration_df, sub[row_cal,])
  validation_df <-  rbind(validation_df, sub[-row_cal,])
}

write.csv(calibration_df, file = 'data/calibration_early.csv', row.names = FALSE)
write.csv(validation_df, file = 'data/validation_early.csv', row.names = FALSE)

#ids of the locations for the calibration
ids <- unique(pheno_late_sub$Stations_id)

calibration_df <- data.frame()
validation_df <- data.frame()

#for each id, take 75% for calibration
set.seed(1234567879)
for(i in 1:length(ids)){
  #subset pheno data
  sub <- pheno_late_sub %>% 
    filter(Stations_id == ids[i],
           Phase_id == phase) %>% 
    mutate(id = Stations_id,
           latitude = geograph.Breite,
           longitude = geograph.Laenge,
           year = Referenzjahr,
           yday = Jultag, 
           name = Stationsname) %>% 
    select(id, name, latitude, longitude, year, yday)
  
  #calibration dataset size
  n_cal <- floor(share_cal * nrow(sub))
  
  row_cal <- sample(1:nrow(sub),size = n_cal) %>% 
    sort()
  
  calibration_df <- rbind(calibration_df, sub[row_cal,])
  validation_df <-  rbind(validation_df, sub[-row_cal,])
}

write.csv(calibration_df, file = 'data/calibration_late.csv', row.names = FALSE)
write.csv(validation_df, file = 'data/validation_late.csv', row.names = FALSE)


#--------------#
#subset the calibration data, as it is too large####
#--------------#

pheno_early_cal <- read.csv('data/calibration_early.csv') %>%
  mutate(id_year =  paste(id, year, sep = '_'))
weather <- arrow::read_feather('data/weather_observed_ready.feather') %>%
  mutate(season = ifelse(Month > 8, yes = Year + 1, no = Year),
         id_year = paste(Stations_id, season, sep = '_'))

#subset the calibration data (early ripening), to 1000 data points
nrep <- 10
df_row_calibration <- data.frame()
set.seed(123456789)
for(i in 1:nrep){
  i_sample <- sample(1:nrow(pheno_early_cal), size = 1000)
  df_row_calibration <- rbind(df_row_calibration,
                              data.frame(r = i,
                                         row_select = i_sample))
}
write.csv(df_row_calibration, file = 'data/row-calibration_early.csv', row.names = FALSE)


pheno_late_cal <- read.csv('data/calibration_late.csv') %>%
  mutate(id_year =  paste(id, year, sep = '_'))
weather <- arrow::read_feather('data/weather_observed_ready.feather') %>%
  mutate(season = ifelse(Month > 8, yes = Year + 1, no = Year),
         id_year = paste(Stations_id, season, sep = '_'))

#subset the calibration data (late ripening), maybe have to data points
nrep <- 10
df_row_calibration <- data.frame()
set.seed(123456789)
for(i in 1:nrep){
  i_sample <- sample(1:nrow(pheno_late_cal), size = 1000)
  df_row_calibration <- rbind(df_row_calibration,
                              data.frame(r = i,
                                         row_select = i_sample))
}
write.csv(df_row_calibration, file = 'data/row-calibration_late.csv', row.names = FALSE)



