#prepare the data for cluster for optimization####

#2 cultivars, 10 repetitions
#--> 20 different environments
setwd('../2024_frostmap_dwd/')
library(tidyverse)
library(chillR)

#read pehnology data
pheno_early_cal <- read.csv('data/calibration_early.csv') %>%
  mutate(id_year =  paste(id, year, sep = '_')) 
pheno_late_cal <- read.csv('data/calibration_late.csv') %>%
  mutate(id_year =  paste(id, year, sep = '_')) 

#read which rows to subset
df_row_calibration_early <- read.csv('data/row-calibration_early.csv')
df_row_calibration_late <- read.csv('data/row-calibration_late.csv')

#read weather and subset
weather <- arrow::read_feather('data/weather_observed_ready.feather') %>%
  mutate(season = ifelse(Month > 8, yes = Year + 1, no = Year),
         id_year = paste(Stations_id, season, sep = '_'))

#iterate over the repetitions, create seasonlist, save objects with phenology data
for(i in unique(df_row_calibration_early$r)){
  
  ###early####
  
  fname <- paste0('data/input/season-pheno-early-', i, '.RData')

  if(file.exists(fname) == FALSE){
    i_sample <- df_row_calibration_early %>%
      filter(r == i) %>%
      pull(row_select)

    #subset the phenology and remove the orgiginal data.frame to
    #free memory
    pheno_sub <- pheno_early_cal[i_sample,]

    #iterate other the ids and create seasonlist for the relevant years only
    seasonlist <- purrr::map(unique(pheno_sub$id), function(id_i){

      #id_i <- unique(pheno_sub$id)[1]
      lat <- pheno_sub$latitude[pheno_sub$id == id_i][1]

      year_id <- pheno_sub %>%
        filter(id == id_i) %>%
        pull(year)

      weather[weather$Stations_id == id_i,] %>%
        chillR::stack_hourly_temps(latitude = lat) %>%
        purrr::pluck('hourtemps') %>%
        chillR::genSeasonList(mrange = c(9, 5), years = year_id) %>%
        setNames(paste(id_i, year_id, sep = '_')) %>%
        return()

    }, .progress = TRUE)

    #have one giant list with all the seasons
    seasonlist <- unlist(seasonlist, recursive = FALSE)
    #make sure season list in the same order as the pheno data
    seasonlist <- seasonlist[pheno_sub$id_year]

    save(pheno_sub, seasonlist, file = fname)
  }

  ###late####
  
  fname <- paste0('data/input/season-pheno-late-', i, '.RData')
  
  if(file.exists(fname) == FALSE){
    i_sample <- df_row_calibration_late %>% 
      filter(r == i) %>% 
      pull(row_select)
    
    #subset the phenology and remove the orgiginal data.frame to
    #free memory
    pheno_sub <- pheno_late_cal[i_sample,]
    
    #iterate other the ids and create seasonlist for the relevant years only
    seasonlist <- purrr::map(unique(pheno_sub$id), function(id_i){
      
      #id_i <- unique(pheno_sub$id)[1]
      lat <- pheno_sub$latitude[pheno_sub$id == id_i][1]
      
      year_id <- pheno_sub %>% 
        filter(id == id_i) %>% 
        pull(year)
      
      weather[weather$Stations_id == id_i,] %>% 
        chillR::stack_hourly_temps(latitude = lat) %>% 
        purrr::pluck('hourtemps') %>% 
        chillR::genSeasonList(mrange = c(9, 5), years = year_id) %>% 
        setNames(paste(id_i, year_id, sep = '_')) %>% 
        return()
      
    }, .progress = TRUE)
    
    #have one giant list with all the seasons
    seasonlist <- unlist(seasonlist, recursive = FALSE)
    #make sure season list in the same order as the pheno data
    seasonlist <- seasonlist[pheno_sub$id_year]
    
    save(pheno_sub, seasonlist, file = fname)
  }

}

#write a csv file that coordinates which jobid belongs to which file####
fnames <- list.files('data/input/', pattern = '*.RData', full.names = TRUE)
cultivar <- strsplit(fnames, split = '-') %>% purrr::map_chr(3)
r <- strsplit(fnames, split = '-') %>% purrr::map_chr(4) %>% gsub(pattern = '.RData', replacement = '', x = .) %>% as.numeric()

data.frame(jobid = (1:20)-1, fname = fnames, cultivar = cultivar, r = r) %>% 
  write.csv('data/input/id-coordinator-fitting.csv', row.names = FALSE)
