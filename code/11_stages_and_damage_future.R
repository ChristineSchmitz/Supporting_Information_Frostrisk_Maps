#Read libraries and data####
library(tidyverse)
library(chillR)
library(optparse)
library(LarsChill)
 

LarsChill::phenoflex_parnames_old

#read model parameters####
par_df <- read.csv('data/par_1993-2022-cluster.csv') %>% 
  select(par, value, repetition, cultivar) %>% 
  pivot_wider(names_from = par, values_from = value) %>% 
  mutate(pie_c = piec)%>%
  arrange(repetition, cultivar)

#read in crit temps####
crit_temp<-read.csv("data/crit_temp_and_curve_fit.csv")

#station infos####
all_pheno_stations <- read.csv('data/all_pheno_stations.csv')
new_names<-c(location="Stationsname")
all_pheno_stations<-rename(all_pheno_stations, all_of(new_names) )

#function to get the stages####
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

#share zc based on apple data####
share_zc<-c(0.25,0.37,0.58,0.7,0.86,1)
stage<-c("green_tip","half_green","tight_cluster","first_pink","full_pink","bloom_to_fruit")
frost_thresholds_apple<-data.frame(share_zc,stage)

#create a vector with all generated future weather files
future_weather_files<-c(list.files("data/future_weather"))


out_path <- 'data/output/frost_damage/'


# Create a mapping table for stages to their respective `k_fit` and `x0_fit`
stage_mapping <- crit_temp[, c('stage', 'k_fit', 'x0_fit')]


print('start calculation o_o')
print(paste0(rep('-', 10), collapse = ''))
reslist <- list()


for(j in future_weather_files){
  
  fname <- paste0("frost_damage_",sub("\\.csv$", "", j),".rds")
  
  cat(paste0("(", j, ") ", 'Calculating damage for: ', fname, '\n'))

  if(file.exists(paste0(out_path, fname))){
      cat("exists!\n")
      next()
  }
  
  #extract the model parameter
  par <- par_df %>% 
    filter(repetition == 1, cultivar == "early") %>% #replace by late to make calculations for late ripening varieties
    dplyr::select(matches(LarsChill::phenoflex_parnames_new)) %>%
    mutate(across(everything(), as.numeric)) %>% 
    unlist() %>% 
    LarsChill::convert_parameters() %>% 
    set_names(LarsChill::phenoflex_parnames_old)
  
  #read future weather  
  future_weather<-read.csv(paste0("data/future_weather/", j))
  future_weather<-make_JDay(future_weather)
  
  #Prepare weather data#
  Current_Stations_id<-as.numeric(sub(".*_(\\d+)\\..*", "\\1", j))
  Latitude_Station<-subset(all_pheno_stations, all_pheno_stations$Stations_id==Current_Stations_id)
  
  iSeason <- future_weather %>% 
    stack_hourly_temps(latitude = Latitude_Station$geograph.Breite) %>% 
    pluck('hourtemps') %>% 
    chillR::genSeasonList(mrange = c(8, 6),
                       years = c(2000:2100))
  
  #predict bloom date and estimate frost damage####
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

    season_data_stage %>% 
      arrange(date) %>% 
      mutate(season = ifelse(JDay >= 200, yes = Year + 1, no = Year)) %>% 
      group_by() %>% 
      summarise(max_damage=max(frost_damage, na.rm = T),
                remaining_flowers=prod(remaining, na.rm = T),
                n_frost_hours=sum(Temp<0&stage!="dormant"&stage!="no_stage"),
                bloomdate = bloomdate) %>%
      mutate(season = max_year,
             n_frost_nights = frost_nights) %>% 
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
print(paste0(rep('-', 10), collapse = ''))
print('calculations complete :D')
