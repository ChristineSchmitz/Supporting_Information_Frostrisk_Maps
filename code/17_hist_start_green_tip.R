#adapted code version to calculate the start of green tip stage 
#Read libraries and data####
library(tidyverse)
library(chillR)
library(optparse)
library(LarsChill)
library(viridis)
library(beepr)

#read model parameters
par_df <- read.csv('par_1993-2022-cluster.csv') %>% 
  select(par, value, repetition, cultivar) %>% 
  pivot_wider(names_from = par, values_from = value) %>% 
  mutate(pie_c = piec)%>%
  arrange(repetition, cultivar)

#read in crit temps
crit_temp<-read.csv("data/crit_temp_and_curve_fit.csv")
#crit_temp<-read.csv("/home/jkopton/frost_maps_future/damage/scripts/crit_temp_and_curve_fit.csv")
#crit_temp_dt <- as.data.table(crit_temp)

#station infos
all_pheno_stations <- read.csv('data/all_pheno_stations.csv')
#all_pheno_stations<-read.csv("/home/jkopton/frost_maps_future/damage/scripts/all_pheno_stations.csv")
new_names<-c(location="Stationsname")
all_pheno_stations<-rename(all_pheno_stations, all_of(new_names) )

#function to get the stages
get_stages <- function(pheno_out, zc, frost_thresholds){
  #pheno_out <- out
  #zc <- par_df$zc[1]
  
  #translate relative requirement to absolute
  frost_thresholds$req <- frost_thresholds$share_zc * zc
  
  #make sure they are ordered correctly
  frost_thresholds <- frost_thresholds[order(frost_thresholds$req),]
  
  stage <- rep('dormant', length(pheno_out$z))
  for(i in 1:(nrow(frost_thresholds)-1)){
    stage <- ifelse(pheno_out$z >= frost_thresholds$req[i] & pheno_out$z < frost_thresholds$req[i+1], 
                    yes = frost_thresholds$stage[i], no = stage)
    
  }
  #last stage
  stage <- ifelse(pheno_out$z >= frost_thresholds$req[nrow(frost_thresholds)],
                  yes = frost_thresholds$stage[nrow(frost_thresholds)],
                  no = stage)
  #if there is no match --> no bloom
  if(is.na(which(stage=="bloom_to_fruit")[1])==TRUE){
    stage<-stage
  }else{
    stage[which(stage=="bloom_to_fruit"):length(stage)]<-"bloom_to_fruit"
  }
  
  pheno_out$stage<-stage
  return(pheno_out)
  
}

#share zc based on apple data
share_zc<-c(0.25,0.37,0.58,0.7,0.86,1)
stage<-c("green_tip","half_green","tight_cluster","first_pink","full_pink","bloom_to_fruit")
frost_thresholds_apple<-data.frame(share_zc,stage)


weather <- arrow::read_feather('data/weather_observed_ready.feather')
stations<-unique(weather$Stations_id)
#decide on the parameter to use



out_path <- 'C:/Users/schmitz_ch/Documents/frostmap_dwd/data/green_tip/'
#out_path <- 'data/output/frost-damage/'


# Create a mapping table for stages to their respective `k_fit` and `x0_fit`
stage_mapping <- crit_temp[, c('stage', 'k_fit', 'x0_fit')]


#early####

for(j in stations){
  #subset the task data.frame
  #row <- mytasks[j, ]
  
  #extract the fileinformation from the weather data
  #id <- str_split(string = row$weather_data, pattern = '_') %>% purrr::map_chr(4)
  #remove the file extension
  id <- j
  hist_weather<-subset(weather, weather$Stations_id== j)
  #
  fname <- paste0("green_tip_hist_early_", id, '.rds')
  
  cat(paste0("(", j, ") ", 'Calculating damage for: ', fname, '\n'))
  
  if(file.exists(paste0(out_path, fname))){
    cat("exists!\n")
    next()
  }
  
  #extract the model parameter
  par <- par_df %>% 
    filter(repetition == 1, cultivar == "early") %>% 
    dplyr::select(matches(LarsChill::phenoflex_parnames_new)) %>%
    mutate(across(everything(), as.numeric)) %>% 
    unlist() %>% 
    LarsChill::convert_parameters() %>% 
    set_names(LarsChill::phenoflex_parnames_old)
  
  #read future weather  
  
  future_weather<-make_JDay(hist_weather)
  
  #Prepare weather data##
  Current_Stations_id<-j
  Latitude_Station<-subset(all_pheno_stations, all_pheno_stations$Stations_id==Current_Stations_id)
  
  iSeason <- future_weather %>% 
    stack_hourly_temps(latitude = Latitude_Station$geograph.Breite) %>% 
    pluck('hourtemps') %>% 
    chillR::genSeasonList(mrange = c(8, 6),
                          years = c(1992:2022))
  
  frost_result <- purrr::map(2:length(iSeason), function(i){
    season_data <-  iSeason[[i]]
    
    res <- PhenoFlex(temp = season_data$Temp,
                     times = c(1: length(season_data$Temp)),
                     stopatzc = TRUE,
                     basic_output = FALSE,
                     yc = par['yc'], 
                     zc = par['zc'], 
                     s1 = par['s1'],
                     Tu = par['Tu'], 
                     Tf = par['Tf'],
                     Tc = par['Tc'], 
                     Tb = par['Tb'], 
                     E0 = par['E0'],
                     E1 = par['E1'], 
                     A0 = par['A0'], 
                     A1 = par['A1'], 
                     slope = par['slope'], 
                     deg_celsius = TRUE
                     #zc = zc,
                     #yc = yc
    )
    season_data[,"x"]<-res$x
    season_data[,"y"]<-res$y
    season_data[,"z"]<-res$z
    #season_data<-add_date(season_data)  
    #season_Jday <-  make_JDay(season_data)
    
    #add the phenological stage to the temperature data
    season_data_stage<-get_stages(pheno_out = season_data,
                                  zc= as.double(par['zc']),
                                  frost_thresholds = frost_thresholds_apple) %>% 
      merge(stage_mapping, by = "stage", all.x = TRUE)
    
    #bloom_to_fruit should have the same critical temperatures as the flowering
    season_data_stage$k_fit[season_data_stage$stage == "bloom_to_fruit"] <- crit_temp[crit_temp$stage == "first_bloom", 'k_fit']
    season_data_stage$x0_fit[season_data_stage$stage == "bloom_to_fruit"] <- crit_temp[crit_temp$stage == "first_bloom", 'x0_fit']
    
    #% damage and remaining flowers
    season_data_stage$frost_damage<-ifelse(season_data_stage$Temp < 0, 1 / (1 + exp(-season_data_stage$k_fit * (season_data_stage$Temp - season_data_stage$x0_fit))),0)
    season_data_stage$remaining<-round(1 - season_data_stage$frost_damage, digits = 3)
    
    #ad a date column
    season_data_stage$date <- as.Date(season_data_stage$JDay - 1, origin = paste0(season_data_stage$Year, "-01-01"))
    max_year <- max(season_data_stage$Year)
    
    #get number of frost nights
    frost_nights <- season_data_stage %>% 
      filter(stage %in% c('dormant') == FALSE) %>% 
      group_by(JDay) %>% 
      summarise(frost_night = any(Temp < 0)) %>% 
      ungroup() %>% 
      pull(frost_night) %>% 
      sum()
    
    bloomdate <- NA
    #calculate the bloom date, digits after comma indicate part of the day.
    #day is centered at 12:00 so JDay 110 = at day 110 at 12:00 110.1 shoudl be at day 110 at 13:00
    if(is.na(res$bloomindex) == FALSE){
      JDay <- season_data$JDay[res$bloomindex]
      JDaylist <- which(season_data$JDay == JDay)
      n <- length(JDaylist)
      if(n == 1)  bloomdate <- JDay
      #calculate the part after the digit
      #            julian day   part of day with 0 as midnight   shift zero to midday
      bloomdate <- round(JDay + which(JDaylist == res$bloomindex)/n - 1/(n/ceiling(n/2)), digits = 1)
    }
    
    green_tip_prep<-subset(season_data_stage, season_data_stage$stage=="green_tip")
  
    season_data_stage<-season_data_stage %>% 
      arrange(date) %>% 
      mutate(season = ifelse(JDay >= 200, yes = Year + 1, no = Year)) %>% 
      group_by() %>% 
      summarise(max_damage=max(frost_damage, na.rm = T),
                remaining_flowers=prod(remaining, na.rm = T),
                n_frost_hours=sum(Temp<0&stage!="dormant"&stage!="no_stage"),
                bloomdate = bloomdate) %>%
      mutate(season = max_year,
             n_frost_nights = frost_nights)
    
    season_data_stage$green_tip<-min(green_tip_prep$JDay)
    season_data_stage%>% 
      return()
  },.progress = FALSE) %>% 
    bind_rows()
  
  #summarise results and add to the result list
  #add the information of the climate scenario etc
  frost_result %>% 
    mutate(max_damage = round(max_damage, digits = 3),
           remaining_flowers = round(remaining_flowers, digits = 3)) %>% 
    saveRDS(file = paste0(out_path, fname))
  
}
beep(5)
print('calculations complete :D')

#late####
out_path <- 'C:/Users/schmitz_ch/Documents/frostmap_dwd/data/green_tip/late'


for(j in stations){
  #subset the task data.frame
  #row <- mytasks[j, ]
  
  #extract the fileinformation from the weather data
  #id <- str_split(string = row$weather_data, pattern = '_') %>% purrr::map_chr(4)
  #remove the file extension
  id <- j
  hist_weather<-subset(weather, weather$Stations_id== j)
  #
  fname <- paste0("green_tip_hist_late_", id, '.rds')
  
  cat(paste0("(", j, ") ", 'Calculating damage for: ', fname, '\n'))
  
  if(file.exists(paste0(out_path, fname))){
    cat("exists!\n")
    next()
  }
  
  #extract the model parameter
  par <- par_df %>% 
    filter(repetition == 1, cultivar == "late") %>% 
    dplyr::select(matches(LarsChill::phenoflex_parnames_new)) %>%
    mutate(across(everything(), as.numeric)) %>% 
    unlist() %>% 
    LarsChill::convert_parameters() %>% 
    set_names(LarsChill::phenoflex_parnames_old)
  
  #read future weather  
  #future_weather<-read.csv(paste0("code/bonna/",row$weather_data))
  #future_weather<-read.csv(paste0("/home/jkopton/frost_maps_future/damage/input/",row$weather_data))
  future_weather<-make_JDay(hist_weather)
  
  #Prepare weather data#
  Current_Stations_id<-j
  Latitude_Station<-subset(all_pheno_stations, all_pheno_stations$Stations_id==Current_Stations_id)
  
  iSeason <- future_weather %>% 
    stack_hourly_temps(latitude = Latitude_Station$geograph.Breite) %>% 
    pluck('hourtemps') %>% 
    chillR::genSeasonList(mrange = c(8, 6),
                          years = c(1992:2022))
  
  #hier zweiter loop über die 100 Jahre  in season 1 bis 100
  frost_result <- purrr::map(2:length(iSeason), function(i){
    season_data <-  iSeason[[i]]
    
    res <- PhenoFlex(temp = season_data$Temp,
                     times = c(1: length(season_data$Temp)),
                     stopatzc = TRUE,
                     basic_output = FALSE,
                     yc = par['yc'], 
                     zc = par['zc'], 
                     s1 = par['s1'],
                     Tu = par['Tu'], 
                     Tf = par['Tf'],
                     Tc = par['Tc'], 
                     Tb = par['Tb'], 
                     E0 = par['E0'],
                     E1 = par['E1'], 
                     A0 = par['A0'], 
                     A1 = par['A1'], 
                     slope = par['slope'], 
                     deg_celsius = TRUE
                     #zc = zc,
                     #yc = yc
    )
    season_data[,"x"]<-res$x
    season_data[,"y"]<-res$y
    season_data[,"z"]<-res$z
    #season_data<-add_date(season_data)  
    #season_Jday <-  make_JDay(season_data)
    
    #add the phenological stage to the temperature data
    season_data_stage<-get_stages(pheno_out = season_data,
                                  zc= as.double(par['zc']),
                                  frost_thresholds = frost_thresholds_apple) %>% 
      merge(stage_mapping, by = "stage", all.x = TRUE)
    
    #bloom_to_fruit should have the same critical temperatures as the flowering
    season_data_stage$k_fit[season_data_stage$stage == "bloom_to_fruit"] <- crit_temp[crit_temp$stage == "first_bloom", 'k_fit']
    season_data_stage$x0_fit[season_data_stage$stage == "bloom_to_fruit"] <- crit_temp[crit_temp$stage == "first_bloom", 'x0_fit']
    
    #% damage and remaining flowers
    season_data_stage$frost_damage<-ifelse(season_data_stage$Temp < 0, 1 / (1 + exp(-season_data_stage$k_fit * (season_data_stage$Temp - season_data_stage$x0_fit))),0)
    season_data_stage$remaining<-round(1 - season_data_stage$frost_damage, digits = 3)
    
    #ad a date column
    season_data_stage$date <- as.Date(season_data_stage$JDay - 1, origin = paste0(season_data_stage$Year, "-01-01"))
    max_year <- max(season_data_stage$Year)
    
    #get number of frost nights
    frost_nights <- season_data_stage %>% 
      filter(stage %in% c('dormant') == FALSE) %>% 
      group_by(JDay) %>% 
      summarise(frost_night = any(Temp < 0)) %>% 
      ungroup() %>% 
      pull(frost_night) %>% 
      sum()
    
    bloomdate <- NA
    #calculate the bloom date, digits after comma indicate part of the day.
    #day is centered at 12:00 so JDay 110 = at day 110 at 12:00 110.1 shoudl be at day 110 at 13:00
    if(is.na(res$bloomindex) == FALSE){
      JDay <- season_data$JDay[res$bloomindex]
      JDaylist <- which(season_data$JDay == JDay)
      n <- length(JDaylist)
      if(n == 1)  bloomdate <- JDay
      #calculate the part after the digit
      #            julian day   part of day with 0 as midnight   shift zero to midday
      bloomdate <- round(JDay + which(JDaylist == res$bloomindex)/n - 1/(n/ceiling(n/2)), digits = 1)
    }
    
    green_tip_prep<-subset(season_data_stage, season_data_stage$stage=="green_tip")
    
    season_data_stage<-season_data_stage %>% 
      arrange(date) %>% 
      mutate(season = ifelse(JDay >= 200, yes = Year + 1, no = Year)) %>% 
      group_by() %>% 
      summarise(max_damage=max(frost_damage, na.rm = T),
                remaining_flowers=prod(remaining, na.rm = T),
                n_frost_hours=sum(Temp<0&stage!="dormant"&stage!="no_stage"),
                bloomdate = bloomdate) %>%
      mutate(season = max_year,
             n_frost_nights = frost_nights)
    
    season_data_stage$green_tip<-min(green_tip_prep$JDay)
    season_data_stage%>% 
      return()
  },.progress = FALSE) %>% 
    bind_rows()
  
  #summarise results and add to the result list
  #add the information of the climate scenario etc
  frost_result %>% 
    mutate(max_damage = round(max_damage, digits = 3),
           remaining_flowers = round(remaining_flowers, digits = 3)) %>% 
    saveRDS(file = paste0(out_path, fname))
  
}


#load all the rds files and save to some combined files
flist <- list.files('data/green_tip/', pattern = '*.rds', full.names = TRUE)
flist <- list.files('data/green_tip/late/', pattern = '*.rds', full.names = TRUE)

#summarize results####
damage_summary_list <- list()
for (i in 1:length(flist)){
  frost_damage_file<-readRDS(flist[i])
  frost_damage_file$Station_id<-sub(".*_(\\d+)\\.rds$", "\\1", flist[i])
  
  
  frost_szenario_summary<-frost_damage_file%>%
    group_by(Station_id)%>%
    summarise(n_years_damage=sum(remaining_flowers!=1),
              n_years_damage_10=sum(remaining_flowers<0.9),
              n_years_damage_50=sum(remaining_flowers<0.5),
              earliest_bloom=min(bloomdate, na.rm = T),
              latest_bloom=max(bloomdate, na.rm = T),
              median_bloom=median(bloomdate, na.rm = T),
              n_no_bloom=sum(is.na(bloomdate)),
              earliest_green_tip=min(green_tip, na.rm = T),
              latest_green_tip=max(green_tip, na.rm = T),
              median_green_tip=median(green_tip, na.rm = T),
              n_no_green_tip=sum(is.na(green_tip)),
              max_n_frost_nights=max(n_frost_nights),
              min_n_frost_nights=min(n_frost_nights),
              median_n_frost_nights=median(n_frost_nights),
              perc_years_damage=(sum(remaining_flowers!=1)/30)*100,
              perc_years_damage_10=(sum(remaining_flowers<0.9)/30)*100,
              perc_years_damage_50=(sum(remaining_flowers<0.5)/30)*100)
  
  
  damage_summary_list[[i]] <- frost_szenario_summary
}

damage_summary_df <- bind_rows(damage_summary_list)

write.csv(damage_summary_df, "data/hist_damage_late_summary_green_tip.csv")

#compare early and late####
green_tip_hist_early<-read.csv("data/hist_damage_early_summary_green_tip.csv")
green_tip_hist_late<-read.csv("data/hist_damage_late_summary_green_tip.csv")

summary(green_tip_hist_early$median_bloom)
summary(green_tip_hist_late$median_bloom)

summary(green_tip_hist_early$median_green_tip)
summary(green_tip_hist_late$median_green_tip)


median(green_tip_hist_early$median_green_tip - green_tip_hist_late$median_green_tip)
mean(green_tip_hist_early$median_green_tip - green_tip_hist_late$median_green_tip)
summary(green_tip_hist_early$median_green_tip - green_tip_hist_late$median_green_tip)
