#load libraries####
library(chillR)
library(tidyverse)

#summarize change scenarios####
#read an prepare location information of all stations
all_pheno_stations<-read.csv("data/all_pheno_stations.csv")
new_names<-c(location="Stationsname")
all_pheno_stations<-rename(all_pheno_stations, all_of(new_names) )

#read in change scenarios
change_scenarios<-read.csv("data/change_scenarios.csv")

#add location information
change_scenarios<-merge(change_scenarios, all_pheno_stations[2:3], by="location")

#summary change scenarios Tmin
summary_change_scenarios_Tmin<-change_scenarios%>%
  group_by(scenario, scenario_year, Stations_id, Month)%>%
  summarise(start_year=median(start_year),
            end_year=median(end_year),
            reference_year=median(reference_year),
            median_Tmin=median(Tmin),
            min_Tmin=min(Tmin),
            max_Tmin=max(Tmin),
            quantile10_Tmin=quantile(Tmin,0.1),
            quantile25_Tmin=quantile(Tmin,0.25),
            quantile75_Tmin=quantile(Tmin,0.75),
            quantile90_Tmin=quantile(Tmin,0.9),
            mean_Tmin=mean(Tmin))
write.csv(summary_change_scenarios_Tmin, "data/summary_change_scenarios_Tmin.csv")

#summary change scenarios Tmax
summary_change_scenarios_Tmax<-change_scenarios%>%
  group_by(scenario, scenario_year, Stations_id, Month)%>%
  summarise(median_Tmax=median(Tmax),
            min_Tmax=min(Tmax),
            max_Tmax=max(Tmax),
            quantile10_Tmax=quantile(Tmax,0.1),
            quantile25_Tmax=quantile(Tmax,0.25),
            quantile75_Tmax=quantile(Tmax,0.75),
            quantile90_Tmax=quantile(Tmax,0.9),
            mean_Tmax=mean(Tmax))
write.csv(summary_change_scenarios_Tmax, "data/summary_change_scenarios_Tmax.csv")


#read in previously saved summary files
summary_change_scenarios_Tmax<-read.csv("data/summary_change_scenarios_Tmax.csv")
summary_change_scenarios_Tmin<-read.csv("data/summary_change_scenarios_Tmin.csv")

#reformat change scenario summaries for weather generation####

#reformat "median" scenario to generate future weather
median_scenario<-summary_change_scenarios_Tmin[1:8]
median_scenario$Tmin<-summary_change_scenarios_Tmin$median_Tmin
median_scenario$Tmax<-summary_change_scenarios_Tmax$median_Tmax
median_scenario$labels<-rep("median",)
median_scenario$scenario_type<-rep("relative",)
median_scenario$location<-median_scenario$Stations_id
write.csv(median_scenario, "data/median_climate_model.csv")

#reformat "quantile10" scenario to generate future weather
quantile10_scenario<-summary_change_scenarios_Tmin[1:8]
quantile10_scenario$Tmin<-summary_change_scenarios_Tmin$quantile10_Tmin
quantile10_scenario$Tmax<-summary_change_scenarios_Tmax$quantile10_Tmax
quantile10_scenario$labels<-rep("quantile10",)
quantile10_scenario$scenario_type<-rep("relative",)
quantile10_scenario$location<-quantile10_scenario$Stations_id
write.csv(quantile10_scenario, "data/quantile10_climate_model.csv")

#reformat "quantile90" scenario to generate future weather
quantile90_scenario<-summary_change_scenarios_Tmin[1:8]
quantile90_scenario$Tmin<-summary_change_scenarios_Tmin$quantile90_Tmin
quantile90_scenario$Tmax<-summary_change_scenarios_Tmax$quantile90_Tmax
quantile90_scenario$labels<-rep("quantile90",)
quantile90_scenario$scenario_type<-rep("relative",)
quantile90_scenario$location<-quantile90_scenario$Stations_id
write.csv(quantile90_scenario, "data/quantile90_climate_model.csv")

#generate future weather ####
f_path <- 'data/future_weather/'
pheno_station_IDs_vector<-unique(weather$Stations_id)

for(id_i in unique(weather$Stations_id)){
  #subset weather file
  weather_sub <- weather[weather$Stations_id == id_i,]
  
  cat(paste0('Generating weather for: ', id_i, '\n'))
  
  #get temperature scenario for actual median of observed temperatures
  scenario_2007 <- chillR::temperature_scenario_from_records(subset(weather, weather$Stations_id==id_i),
                                                             2007)
  
  #get temperature scenario for needed time point
  scenario_2000 <- chillR::temperature_scenario_from_records(subset(weather, weather$Stations_id==id_i),
                                                             2000)
  
  #calculate the baseline adjustment needed for the observed weather 
  base <- chillR::temperature_scenario_baseline_adjustment(scenario_2007,
                                                           scenario_2000)
  
  #subset scenario list and reformat so that each scenario is a different entry in the list
  #assumed that the location column contains the station id. If it contains the name of the 
  #station, then we need to find out what id corresponds to what name
  scen_list_sub <- median_scenario %>% 
    filter(Stations_id == id_i) %>% 
    chillR::convert_scen_information(give_structure = FALSE)
  

  #adjust the calculated temperature scenarios for the future weather
  adjusted_list <- chillR::temperature_scenario_baseline_adjustment(base,
                                                                    scen_list_sub,
                                                                    temperature_check_args=
                                                                      list( scenario_check_thresholds = c(-5, 15)))
  
  
  
  #iterate over the adjusted list
  for(i in 1:length(adjusted_list)){
    
    cat(paste0('Scenario: ', i, 'out of ', length(adjusted_list), '\n'))
    
    #file name
    fname <- paste0(f_path,'future_weather', '_', i,'_', names(adjusted_list[i]), '.csv')
    
    if(file.exists(fname)) next()
    
    #start <- Sys.time()
    temps <- temperature_generation(weather_sub, 
                                    years = c(1992, 2022), 
                                    sim_years = c(2000, 2100), 
                                    adjusted_list[i],    
                                    temperature_check_args=
                                      list( scenario_check_thresholds = c(-5, 15)))
    
    write.csv(temps[[1]], file = fname, row.names = FALSE)
    
  }
  
}
