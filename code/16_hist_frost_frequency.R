#Read libraries and data####
library(tidyverse)
library(chillR)
library(optparse)
library(LarsChill)
library(viridis)
library(beepr)

#Load data####
#read model parameters

par_df <- read.csv('par_1993-2022-cluster.csv') %>% 
  select(par, value, repetition, cultivar) %>% 
  pivot_wider(names_from = par, values_from = value) %>% 
  mutate(pie_c = piec)%>%
  arrange(repetition, cultivar)

#read in crit temps
crit_temp<-read.csv("data/crit_temp_and_curve_fit.csv")


#station infos
all_pheno_stations <- read.csv('data/all_pheno_stations.csv')
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

#create a vector with all generated future weather files
#future_weather_files<-c(list.files("data/future_weather"))

weather <- arrow::read_feather('data/weather_observed_ready.feather')
stations<-unique(weather$Stations_id)

out_path <- 'data/hist_frost_frequency/'


# Create a mapping table for stages to their respective `k_fit` and `x0_fit`
stage_mapping <- crit_temp[, c('stage', 'k_fit', 'x0_fit')]


#Apply the fitted PhenoFlex Model to observed past weather####
###early####
for(j in stations){

  id <- j
  hist_weather<-subset(weather, weather$Stations_id== j)
  #
  fname <- paste0("frostdamage_hist_early_", id, '.rds')
  
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
  
  #read past weather  
  future_weather<-make_JDay(hist_weather)
  
  #Prepare weather data#
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
beep(5)
print('calculations complete :D')



#load all the rds files and save to some combined files
flist <- list.files('data/hist_frost_frequency/', pattern = '*.rds', full.names = TRUE)

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
              max_n_frost_nights=max(n_frost_nights),
              min_n_frost_nights=min(n_frost_nights),
              median_n_frost_nights=median(n_frost_nights),
              perc_years_damage=(sum(remaining_flowers!=1)/30)*100,
              perc_years_damage_10=(sum(remaining_flowers<0.9)/30)*100,
              perc_years_damage_50=(sum(remaining_flowers<0.5)/30)*100)
  
  

  damage_summary_list[[i]] <- frost_szenario_summary
}

damage_summary_df <- bind_rows(damage_summary_list)

write.csv(damage_summary_df, "data/hist_damage_early_summary.csv")
damage_summary_df_early<-read.csv("data/hist_damage_early_summary.csv")



###late####
out_path <- 'data/hist_frost_frequency_late/'

for(j in stations){
  
  id <- j
  hist_weather<-subset(weather, weather$Stations_id== j)
  #
  fname <- paste0("frostdamage_hist_late_", id, '.rds')
  
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
  future_weather<-make_JDay(hist_weather)
  
  #Prepare weather data#
  Current_Stations_id<-j
  Latitude_Station<-subset(all_pheno_stations, all_pheno_stations$Station_id==Current_Stations_id)
  
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
beep(5)
print('calculations complete :D')



#load all the rds files and save to some combined files
flist <- list.files('data/hist_frost_frequency_late/', pattern = '*.rds', full.names = TRUE)

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
              max_n_frost_nights=max(n_frost_nights),
              min_n_frost_nights=min(n_frost_nights),
              median_n_frost_nights=median(n_frost_nights),
              perc_years_damage=(sum(remaining_flowers!=1)/30)*100,
              perc_years_damage_10=(sum(remaining_flowers<0.9)/30)*100,
              perc_years_damage_50=(sum(remaining_flowers<0.5)/30)*100)
  
  

  damage_summary_list[[i]] <- frost_szenario_summary
}

damage_summary_df <- bind_rows(damage_summary_list)

write.csv(damage_summary_df, "data/hist_damage_late_summary.csv")

##Interpolation####
map_data_hist_early<-read.csv("data/hist_damage_early_summary.csv")
map_data_hist_late<-read.csv("data/hist_damage_late_summary.csv")

all_pheno_stations<-read.csv("data/all_pheno_stations.csv")
new_names<-c(location="Stationsname", Station_id="Stations_id")
all_pheno_stations<-rename(all_pheno_stations, all_of(new_names) )
map_data_hist_early<-merge(map_data_hist_early, all_pheno_stations[2:6], by="Station_id")
map_data_hist_late<-merge(map_data_hist_late, all_pheno_stations[2:6], by="Station_id")

gdf <- st_as_sf(map_data_hist_early, coords=c("geograph.Laenge","geograph.Breite"), crs=4326)

# Define a regular grid
xmin <- min(st_coordinates(gdf$geometry)[,1])-0.5
xmax <- max(st_coordinates(gdf$geometry)[,1])+0.5
ymin <- min(st_coordinates(gdf$geometry)[,2])-0.5
ymax <- max(st_coordinates(gdf$geometry)[,2])+0.5
cell_size <- 0.05

dem<-rast("data/srtm_germany_dsm.tif")  #Quelle: https://www.opendem.info/download_srtm.html
dem <- project(dem, "EPSG:4326")


#10 % damage####

      map_data_simyear_model<-map_data_hist_early #choose early or late here
      #map_data_simyear_model<-map_data_hist_late #choose early or late here
      gdf <- st_as_sf(map_data_simyear_model, coords=c("geograph.Laenge","geograph.Breite"), crs=4326)
      gdf$dem <- terra::extract(dem, vect(gdf$geometry))$srtm_germany_dsm
      
      
      model_interpolation <- 
        tryCatch({
          withTimeout({ model_interpolation <- scam(perc_years_damage_10~s(dem, k=4, bs = "mpi"), data=gdf)}, timeout=30 , cpu = Inf)
        }, error = function(e){
          if(grepl("reached elapsed time limit",e$message))
            model <- lm(perc_years_damage_10~dem, data = gdf) else
              paste(e$message,"EXTRACTERROR")
        })
      
      # predict expected temperature based on elevation
      gdf$damage_trend <- predict(model_interpolation,list(dem=gdf$dem))
      

      
      # calcuearly residuals
      gdf$residuals <- gdf$perc_years_damage_10 - gdf$damage_trend
      

      # drop NAs
      gdf <- gdf[!is.na(gdf$residuals),]
      # remove duplicates
      gdf<-gdf[!duplicated(gdf$geometry),]

      
      # calcuearly empirical variogram
      gpb <- gstat(formula=residuals~1, data=gdf)
      vgpb <- variogram(gpb, boundaries = c(0, 1:10*20, 300, 400, 500, 700))
      
      # fit variogram model
      vgmpb <- vgm(model = "Sph", nugget=T)
      
      vgmpb <- fit.variogram(vgpb, vgmpb, fit.method = 6)#methode 7 ging nicht daher auf 6 geändert
      
      # plot variogram
      plot(vgpb, vgmpb, lwd=2)
      
      # create grid for kriging
      grid <- expand.grid(x = seq(from = xmin, to = xmax, by = cell_size),
                          y = seq(from = ymin, to = ymax, by = cell_size))
      grid <- st_as_sf(grid, coords=c("x","y"), crs=4326)
      # local kriging with 15 neighboring points based on variogram model
      krig_res <- krige(residuals~1, gdf, grid, nmax=15, vgmpb)
      
      # calculate feature prediction (based on only elevation) for each grid cell
      krig_res$dem <- terra::extract(dem, krig_res)$srtm_germany_dsm
      krig_res$trend <- predict(model_interpolation,list(dem=krig_res$dem))
      
      # for total prediction add feature prediction and spatial prediction
      krig_res$var1.pred_total <- krig_res$var1.pred + krig_res$trend
      #replace values >100 and <0
      krig_res$var1.pred_total[krig_res$var1.pred_total>100]<-100
      krig_res$var1.pred_total[krig_res$var1.pred_total<0]<-0
      
      # drop NAs
      krig_res <- krig_res[!is.na(krig_res$var1.pred_total),]
      
      ras<-raster(xmn=xmin-0.5*cell_size, xmx=xmax, ymn=ymin-0.5*cell_size, ymx=ymax,
                  crs="EPSG:4326",
                  resolution=c(cell_size,cell_size), vals=NA)
      krig_res_prep<-krig_res[,c(3,6)]
      krig_res_raster <- terra::rasterize(krig_res_prep, ras)
      
      filename_save<-paste0("data/map_preparation/","map_hist_early_interpolated.rds") #choose early or late here
      #filename_save<-paste0("data/map_preparation/","map_hist_late_interpolated.rds") #choose early or late here
      
      saveRDS(krig_res_raster, filename_save)


crs <- st_crs("EPSG:4326")
read_sf("data/DE_VG5000.gpkg", layer="vg5000_lan")|> st_transform(crs) -> DE_Bundeslaender_EPSG_4326
DE_Bundeslaender_ohne_Meer_EPSG_4326<-subset(DE_Bundeslaender_EPSG_4326, DE_Bundeslaender_EPSG_4326$GF==9)

DE_Bundeslaender_ohne_Meer_EPSG_4326_poly<-as(DE_Bundeslaender_ohne_Meer_EPSG_4326,"Spatial")

#Plot maps >10% damage####
raster_hist_late<-readRDS("data/map_preparation/map_hist_late_interpolated.rds")
raster_hist_early<-readRDS("data/map_preparation/map_hist_early_interpolated.rds")


raster_hist_late<-mask(raster_hist_late, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)
raster_hist_early<-mask(raster_hist_early, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)


A1<-rasterVis::levelplot(raster_hist_late$var1.pred_total,
                         margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="quantile 10",
                         #colorkey=F,
                         #scales=list(x=list(draw=FALSE)),
                         xlab="Longitude",
                         ylab="Latitude",
                         colorkey=list(title=list("Percentage\nof years\nwith frost\ndamage\n>10 %\n[%]",fontsize=8))
                         ) +
  latticeExtra::layer({
    SpatialPolygonsRescale(layout.north.arrow(type = 1),
                           offset = c(14,48),
                           scale = 1.5)
  })+
  latticeExtra::layer({
    xs <- seq(12.5, 14.9, by=0.7)
    grid.rect(x=xs, y=47.35,
              width=0.7, height=0.1,
              gp=gpar(fill=rep(c('transparent', 'black'), 2)),
              default.units='native')
    grid.text(x= xs - 0.25, y=47.2, seq(0, 200, by=50),
              gp=gpar(cex=0.5), rot=0,
              default.units='native')
  })+
  latticeExtra::layer({
    grid.text(x= 12.5, y=47.05, "kilometers",
              gp=gpar(cex=0.5), rot=0,
              default.units='native',
              just="left")
  })+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "1993-2022\nlate",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
A1


B1<-rasterVis::levelplot(raster_hist_early$var1.pred_total,
                         margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="quantile 10",
                         colorkey=F,
                         #scales=list(x=list(draw=FALSE)),
                         xlab="Longitude",
                         ylab="Latitude"#,
                         #colorkey=list(title=list("Percentage\nof years\nwith frost\ndamage\n>10 %\n[%]",fontsize=8))
                         ) +
  # latticeExtra::layer({
  #   SpatialPolygonsRescale(layout.north.arrow(type = 1),
  #                          offset = c(14,48),
  #                          scale = 1.5)
  # })+
  # latticeExtra::layer({
  #   xs <- seq(12.5, 14.9, by=0.7)
  #   grid.rect(x=xs, y=47.35,
  #             width=0.7, height=0.1,
  #             gp=gpar(fill=rep(c('transparent', 'black'), 2)),
  #             default.units='native')
  #   grid.text(x= xs - 0.25, y=47.2, seq(0, 200, by=50),
  #             gp=gpar(cex=0.5), rot=0,
  #             default.units='native')
  # })+
  # latticeExtra::layer({
  #   grid.text(x= 12.5, y=47.05, "kilometers",
  #             gp=gpar(cex=0.5), rot=0,
  #             default.units='native',
  #             just="left")
  # })+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "1993-2022\nearly",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
B1


hist_all <- c(B1,A1, layout = c(2,1), merge.legends = T)
print(hist_all)
update(hist_all, scales = list(alternating = 1))

png("figures/hist_early_and_late.png", pointsize=8, width=4000, height=2850, res=600)
hist_all <- c(B1,A1, layout = c(2,1), merge.legends = T)
print(hist_all)
update(hist_all, scales = list(alternating = 1))
dev.off()

#50 % damage####

#map_data_simyear_model<-map_data_hist_early #choose early or late here
map_data_simyear_model<-map_data_hist_late #choose early or late here
gdf <- st_as_sf(map_data_simyear_model, coords=c("geograph.Laenge","geograph.Breite"), crs=4326)
gdf$dem <- terra::extract(dem, vect(gdf$geometry))$srtm_germany_dsm


model_interpolation <- 
  tryCatch({
    withTimeout({ model_interpolation <- scam(perc_years_damage_50~s(dem, k=4, bs = "mpi"), data=gdf)}, timeout=30 , cpu = Inf)
  }, error = function(e){
    if(grepl("reached elapsed time limit",e$message))
      model <- lm(perc_years_damage_50~dem, data = gdf) else
        paste(e$message,"EXTRACTERROR")
  })

# predict expected temperature based on elevation
gdf$damage_trend <- predict(model_interpolation,list(dem=gdf$dem))


# calcuearly residuals
gdf$residuals <- gdf$perc_years_damage_50 - gdf$damage_trend


# drop NAs
gdf <- gdf[!is.na(gdf$residuals),]
# remove duplicates
gdf<-gdf[!duplicated(gdf$geometry),]
# plot residuals
#plot(gdf_day["residuals"])

# calcuearly empirical variogram
gpb <- gstat(formula=residuals~1, data=gdf)
vgpb <- variogram(gpb, boundaries = c(0, 1:10*20, 300, 400, 500, 700))

# fit variogram model
vgmpb <- vgm(model = "Sph", nugget=T)

vgmpb <- fit.variogram(vgpb, vgmpb, fit.method = 6)

# plot variogram
plot(vgpb, vgmpb, lwd=2)

# create grid for kriging
grid <- expand.grid(x = seq(from = xmin, to = xmax, by = cell_size),
                    y = seq(from = ymin, to = ymax, by = cell_size))
grid <- st_as_sf(grid, coords=c("x","y"), crs=4326)
# local kriging with 15 neighboring points based on variogram model
krig_res <- krige(residuals~1, gdf, grid, nmax=15, vgmpb)

#assign(paste0("krig_res_2022_",i),krig_res)
# calculate feature prediction (based on only elevation) for each grid cell
krig_res$dem <- terra::extract(dem, krig_res)$srtm_germany_dsm
krig_res$trend <- predict(model_interpolation,list(dem=krig_res$dem))

# for total prediction add feature prediction and spatial prediction
krig_res$var1.pred_total <- krig_res$var1.pred + krig_res$trend
#replace values >100 and <0
krig_res$var1.pred_total[krig_res$var1.pred_total>100]<-100
krig_res$var1.pred_total[krig_res$var1.pred_total<0]<-0

# drop NAs
krig_res <- krig_res[!is.na(krig_res$var1.pred_total),]

ras<-raster(xmn=xmin-0.5*cell_size, xmx=xmax, ymn=ymin-0.5*cell_size, ymx=ymax,
            crs="EPSG:4326",
            resolution=c(cell_size,cell_size), vals=NA)
krig_res_prep<-krig_res[,c(3,6)]
krig_res_raster <- terra::rasterize(krig_res_prep, ras)

#filename_save<-paste0("data/map_preparation/","map_hist_early_interpolated_damage50.rds") #choose early or late here
filename_save<-paste0("data/map_preparation/","map_hist_late_interpolated_damage50.rds") #choose early or late here

saveRDS(krig_res_raster, filename_save)


crs <- st_crs("EPSG:4326")
read_sf("data/DE_VG5000.gpkg", layer="vg5000_lan")|> st_transform(crs) -> DE_Bundeslaender_EPSG_4326
DE_Bundeslaender_ohne_Meer_EPSG_4326<-subset(DE_Bundeslaender_EPSG_4326, DE_Bundeslaender_EPSG_4326$GF==9)

DE_Bundeslaender_ohne_Meer_EPSG_4326_poly<-as(DE_Bundeslaender_ohne_Meer_EPSG_4326,"Spatial")

#Plot maps >50% damage####
raster_hist_late<-readRDS("data/map_preparation/map_hist_late_interpolated_damage50.rds")
raster_hist_early<-readRDS("data/map_preparation/map_hist_early_interpolated_damage50.rds")


#r_masked <- mask(test_raster, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)
raster_hist_late<-mask(raster_hist_late, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)
raster_hist_early<-mask(raster_hist_early, DE_Bundeslaender_ohne_Meer_EPSG_4326_poly)


A1<-rasterVis::levelplot(raster_hist_late$var1.pred_total,
                         margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="quantile 10",
                         #colorkey=F,
                         #scales=list(x=list(draw=FALSE)),
                         xlab="Longitude",
                         ylab="Latitude",
                         colorkey=list(title=list("Percentage\nof years\nwith frost\ndamage\n>50 %\n[%]",fontsize=8))
) +
  latticeExtra::layer({
    SpatialPolygonsRescale(layout.north.arrow(type = 1),
                           offset = c(14,48),
                           scale = 1.5)
  })+
  latticeExtra::layer({
    xs <- seq(12.5, 14.9, by=0.7)
    grid.rect(x=xs, y=47.35,
              width=0.7, height=0.1,
              gp=gpar(fill=rep(c('transparent', 'black'), 2)),
              default.units='native')
    grid.text(x= xs - 0.25, y=47.2, seq(0, 200, by=50),
              gp=gpar(cex=0.5), rot=0,
              default.units='native')
  })+
  latticeExtra::layer({
    grid.text(x= 12.5, y=47.05, "kilometers",
              gp=gpar(cex=0.5), rot=0,
              default.units='native',
              just="left")
  })+
  latticeExtra::layer({
    grid.text(x= 5.9, y=54.8, "1993-2022\nlate",
              gp=gpar(fontsize=8), rot=0,
              default.units='native',
              just="left")
  })
A1


B1<-rasterVis::levelplot(raster_hist_early$var1.pred_total,
                         margin=F,
                         #col.regions=turbo(100),
                         col.regions=viridis(100, direction = -1),
                         at=seq(0, 100, length.out=100),
                         #main="quantile 10",
                         colorkey=F,
                         #scales=list(x=list(draw=FALSE)),
                         xlab="Longitude",
                         ylab="Latitude"#,
                         #colorkey=list(title=list("Percentage\nof years\nwith frost\ndamage\n>50 %\n[%]",fontsize=8))
) +
  # latticeExtra::layer({
  #   SpatialPolygonsRescale(layout.north.arrow(type = 1),
  #                          offset = c(14,48),
  #                          scale = 1.5)
  # })+
  # latticeExtra::layer({
  #   xs <- seq(12.5, 14.9, by=0.7)
  #   grid.rect(x=xs, y=47.35,
  #             width=0.7, height=0.1,
  #             gp=gpar(fill=rep(c('transparent', 'black'), 2)),
  #             default.units='native')
#   grid.text(x= xs - 0.25, y=47.2, seq(0, 200, by=50),
#             gp=gpar(cex=0.5), rot=0,
#             default.units='native')
# })+
# latticeExtra::layer({
#   grid.text(x= 12.5, y=47.05, "kilometers",
#             gp=gpar(cex=0.5), rot=0,
#             default.units='native',
#             just="left")
# })+
latticeExtra::layer({
  grid.text(x= 5.9, y=54.8, "1993-2022\nearly",
            gp=gpar(fontsize=8), rot=0,
            default.units='native',
            just="left")
})
B1


hist_all <- c(B1,A1, layout = c(2,1), merge.legends = T)
print(hist_all)
update(hist_all, scales = list(alternating = 1))

png("figures/hist_early_and_late_damage50.png", pointsize=8, width=4000, height=2850, res=600)
hist_all <- c(B1,A1, layout = c(2,1), merge.legends = T)
print(hist_all)
update(hist_all, scales = list(alternating = 1))
dev.off()

