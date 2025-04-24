#load libraries####
library(chillR)
library(tidyverse)

#read in relevant data files####
par_df <- read.csv('data/par_1993-2022-cluster.csv') %>% 
  select(-par.start, -par.upper, -par.lower) %>% 
  pivot_wider(names_from = par, values_from = value) %>% 
  mutate(pie_c = piec)


pheno_stations <- read.csv('data/all_pheno_stations.csv')

#read weather
weather <- arrow::read_feather('data/weather_observed_ready.feather') %>%
  mutate(season = ifelse(Month > 8, yes = Year + 1, no = Year),
         id_year = paste(Stations_id, season, sep = '_'))

#make model predictions for each station####
#subset phenodata
id_i <- unique(weather$Stations_id)[1]

pred_df <- purrr::map(unique(weather$Stations_id), function(id_i){
  
  lat <- pheno_stations$geograph.Breite[pheno_stations$Stations_id == id_i]
  
  #create seasonlist
  seasonlist <- weather[weather$Stations_id == id_i,] %>% 
    chillR::stack_hourly_temps(latitude = lat) %>% 
    purrr::pluck('hourtemps') %>% 
    chillR::genSeasonList(mrange = c(9, 5),years =  1993:2022)
  
  #predict bloom dates
  pheno_early <- purrr::map(1:10, function(i){
    #extract and convert parameters
    par <- par_df[par_df$repetition == i & par_df$cultivar == 'early', LarsChill::phenoflex_parnames_new] %>% 
      unlist() %>% 
      LarsChill::convert_parameters()
    
    pred <- LarsChill::return_predicted_days(par = par,
                                             evalpheno::custom_PhenoFlex_GDHwrapper, 
                                             SeasonList = seasonlist, 
                                             na_penalty = NA)
    
    data.frame(pred = round(pred, digits = 1),
             r = i,
             cultivar = 'early',
             stations_id = id_i,
             year = 1993:2022) %>% 
      return()
  }) %>% 
    bind_rows() 
  
  #predict bloom dates
  pheno_late <- purrr::map(1:10, function(i){
    #extract and convert parameters
    par <- par_df[par_df$repetition == i & par_df$cultivar == 'late', LarsChill::phenoflex_parnames_new] %>% 
      unlist() %>% 
      LarsChill::convert_parameters()
    
    pred <- LarsChill::return_predicted_days(par = par,
                                             evalpheno::custom_PhenoFlex_GDHwrapper, 
                                             SeasonList = seasonlist, 
                                             na_penalty = NA)
    
    data.frame(pred = round(pred, digits = 1),
               r = i,
               cultivar = 'late',
               stations_id = id_i,
               year = 1993:2022) %>% 
      return()
  }) %>% 
    bind_rows() 
  
  return(rbind(pheno_early, pheno_late))
  
}, .progress = TRUE )
#making predictions takes ~1h

pred <- do.call(what = 'rbind', pred_df)




#merge with predicted and observed phenology data####



#read which rows of pheno data were used for calibration
df_row_calibration_early <- read.csv('data/row-calibration_early.csv') 
df_row_calibration_late <- read.csv('data/row-calibration_late.csv') 


#read pheno data
pheno_early_cal <- read.csv('data/calibration_early.csv') 
pheno_late_cal <- read.csv('data/calibration_late.csv') 

calibration_master <- data.frame()
validation_sub <- data.frame()

for(i in 1:10){
  r_i <- df_row_calibration_early %>% 
    filter(r ==i) %>% 
    pull(row_select)
  early_sub <- pheno_early_cal %>% 
    slice(r_i) %>% 
    mutate(repetition = i,
           split = 'calibration',
           cultivar = 'early')
  early_val <- pheno_early_cal %>% 
    slice(-r_i) %>% 
    mutate(repetition = i,
           split = 'validation',
           cultivar = 'early')
  
  r_i <- df_row_calibration_late %>% 
    filter(r ==i) %>% 
    pull(row_select)
  late_sub <- pheno_late_cal %>% 
    slice(r_i) %>% 
    mutate(repetition = i,
           split = 'calibration',
           cultivar = 'late')
  late_val <- pheno_late_cal %>% 
    slice(-r_i) %>% 
    mutate(repetition = i,
           split = 'validation',
           cultivar = 'late')
  
  calibration_master <- calibration_master %>% 
    rbind(early_sub) %>% 
    rbind(late_sub)
  
  validation_sub <- validation_sub %>% 
    rbind(early_val) %>% 
    rbind(late_val)
}

validation_late <- read.csv('data/validation_late.csv') %>% 
  mutate(id_year = paste(id, year, sep = '_'),
         split = 'validation',
         cultivar = 'early')
validation_early <- read.csv('data/validation_early.csv') %>% 
  mutate(id_year = paste(id, year, sep = '_'),
         split = 'validation',
         cultivar = 'late')
validation_master <- validation_late %>% rbind(validation_early)
rm(validation_early, validation_late)



#merge pred with:
#1) true calibration data (calibration master)
#2) true validation data (validation master)
#3) calibration data sorted out by the sub-sampling (validation sub)

pred_obs_cal <- merge(pred, calibration_master, 
      by.x = c('stations_id', 'year', 'r', 'cultivar'),
      by.y = c('id', 'year', 'repetition', 'cultivar'),
      all.y = TRUE)

pred_obs_val <- merge(pred, validation_master, 
                      by.x = c('stations_id', 'year', 'cultivar'),
                      by.y = c('id', 'year', 'cultivar'),
                      all.y = TRUE)

pred_obs_val_sub <- merge(pred, validation_sub, 
                          by.x = c('stations_id', 'year', 'r', 'cultivar'),
                          by.y = c('id', 'year', 'repetition', 'cultivar'),
                          all.y = TRUE)

pred_obs <- pred_obs_val %>% 
  select(-id_year) %>% 
  rbind(pred_obs_cal) %>% 
  rbind(pred_obs_val_sub)

rm(pred_obs_cal, pred_obs_val, pred_obs_val_sub, calibration_master, validation_master, validation_sub,
   pheno_early, pheno_early_cal, pheno_late, pheno_late_cal, pheno_stations,
   weather, seasonlist, df_row_calibration_early, df_row_calibration_late, early_sub, early_val,
   late_sub, late_val)

#https://slowkow.com/notes/ggplot2-color-by-density/
get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

#calculate the performance####

performance <- pred_obs %>% 
  mutate(split_label = factor(split,
                              levels = c('calibration', 'validation'),
                              labels = c('Calibration', 'Validation')),
         cultivar_label = factor(cultivar, levels = c('early', 'late'),
                                 labels = c('Early Ripening', 'Late Ripening'))) %>% 
  group_by(cultivar_label, split_label, r) %>% 
  summarise(rmsep = round(RMSEP(pred, yday, na.rm = TRUE),digits = 1),
            rpiq = round(RPIQ(pred, yday, na.rm = TRUE),digits = 1),
            n = n()) %>% 
  na.omit()
write.csv(performance, 'data/performance_models.csv', row.names = FALSE)

pred_obs %>% 
  mutate(split_label = factor(split,
                              levels = c('calibration', 'validation'),
                              labels = c('Calibration', 'Validation')),
         cultivar_label = factor(cultivar, levels = c('early', 'late'),
                                 labels = c('Early Ripening', 'Late Ripening'))) %>% 
  filter(r == 1) %>% 
  na.omit() %>% 
  mutate(density = get_density(x = yday, y = pred, n = 100)) %>% 
  ggplot(aes(x = yday, y = pred)) +
  geom_point(aes(color = density)) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', col = 'red') +
  geom_text(data = performance[performance$r == 1, ], 
            aes(x = 65, y = 145, 
                label = paste0('RMSE: ', format(rmsep, digits = 2),
                               '\nRPIQ: ', format(rpiq, digits = 2),
                               '\nn Observations: ', n)), hjust = 0) +
  viridis::scale_color_viridis() +
  scale_x_continuous(breaks = c(1, 32, 60,91, 121, 152), 
                     labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun')) +
  scale_y_continuous(breaks = c(1, 32, 60,91, 121, 152), 
                     labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun')) +
  ylab('Predicted Bloom Date') +
  xlab('Observed Bloom Date') +
  theme_bw() +
  facet_grid(cultivar_label~split_label)
ggsave('figures/performance_m1.jpeg', height = 20, width = 25, units = 'cm', device = 'jpeg')





RMSEP(test$yday, test$pred, na.rm = TRUE)
RPIQ(test$yday, test$pred, na.rm = TRUE)
#not crazy good but kinda okay

#check how well it performs for the calibration data only
cal_df <- pheno_early_cal %>% 
  mutate(id_row = 1:nrow(pheno_early_cal)) %>% 
  merge(df_row_calibration, by.x = 'id_row', by.y = 'row_select') %>% 
  merge(test, by = c('id', 'name', 'latitude', 'longitude', 'year', 'yday', 'id_year', 'r'))

cal_df %>% 
  na.omit() %>% 
  mutate(density = get_density(x = yday, y = pred, n = 100)) %>% 
  ggplot(aes(x = yday, y = pred)) +
  geom_point(aes(color = density)) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', col = 'red') +
  viridis::scale_color_viridis() +
  scale_x_continuous(breaks = c(1, 32, 60,91, 121, 152), 
                     labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun')) +
  scale_y_continuous(breaks = c(1, 32, 60,91, 121, 152), 
                     labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun')) +
  ylab('Predicted Bloom Date') +
  xlab('Observed Bloom Date') +
  theme_bw()
#dir.create('figures')
ggsave('figures/pred_obs_calibration.jpeg',
       height = 15,
       width = 20,
       units = 'cm',
       device = 'jpeg')

RMSEP(cal_df$yday, cal_df$pred, na.rm = TRUE)
RPIQ(cal_df$yday, cal_df$pred, na.rm = TRUE)

cal_df %>% 
  group_by(r) %>% 
  summarise(rmse = RMSEP(yday, pred, na.rm = TRUE),
            rpiq = RPIQ(yday, pred, na.rm = TRUE))
#sind alle ungefähr gleich gut / schlecht


spatial_performance <- pred_obs %>% 
  mutate(split_label = factor(split,
                              levels = c('calibration', 'validation'),
                              labels = c('Calibration', 'Validation')),
         cultivar_label = factor(cultivar, levels = c('early', 'late'),
                                 labels = c('Early Ripening', 'Late Ripening'))) %>% 
  group_by(cultivar_label, split_label, r, latitude, longitude, stations_id) %>% 
  summarise(rmsep = round(RMSEP(pred, yday, na.rm = TRUE),digits = 1),
            rpiq = round(RPIQ(pred, yday, na.rm = TRUE),digits = 1),
            n = n()) %>% 
  na.omit()


#check out locations with worst performance####


library(maps)
world <- map_data("world") %>% 
  filter(region == 'Germany')
ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group)) + 
  geom_point(data = spatial_performance, aes(x = longitude, y = latitude, col = rmsep)) +
  scale_color_gradientn(colours = colorRamps::matlab.like(15)) +
  scale_size_manual(values = c(1,2)) +
  theme_bw() +
  facet_grid(split_label~cultivar_label)
ggsave('figures/performance_m1_spatial.jpeg',
       height = 20,
       width = 20,
       units = 'cm',
       device = 'jpeg')
