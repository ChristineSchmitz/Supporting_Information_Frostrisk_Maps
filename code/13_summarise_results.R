#load libraries####
library(tidyverse)
library(chillR)
library(beepr)

#make vector of file names
future_damage_files<-c(list.files("data/output/frost_damage"))

#summarize results####
damage_summary_list <- list()
for (i in 1:length(future_damage_files)){
  frost_damage_file<-readRDS(paste0("data/output/frost_damage/",future_damage_files[i]))
  
  frost_damage_file$pathway<-sub(".*\\.(ssp\\d+).*", "\\1", frost_damage_file$id)
  frost_damage_file$model<-sub(".*\\.ssp\\d+\\.(\\w+)\\.\\d+$", "\\1", frost_damage_file$id)
  frost_damage_file$simyear<-sub(".*\\.(\\d+)$", "\\1", frost_damage_file$id)
  frost_damage_file$Station_id<-sub(".*\\.(\\d+)\\.ssp.*", "\\1", frost_damage_file$id)
  frost_damage_file$repetition<-sub("^[^.]+\\.(\\d+)\\..*", "\\1", frost_damage_file$id)
  frost_damage_file$ripening<-sub("^([^.]+)\\..*", "\\1", frost_damage_file$id)
  
  frost_szenario_summary<-frost_damage_file%>%
    group_by(pathway, model, simyear, Station_id, repetition, ripening)%>%
    summarise(n_years_damage=sum(remaining_flowers!=1),
              n_years_damage_10=sum(remaining_flowers<0.9),
              n_years_damage_50=sum(remaining_flowers<0.5),
              earliest_bloom=min(bloomdate, na.rm = T),
              latest_bloom=max(bloomdate, na.rm = T),
              median_bloom=median(bloomdate, na.rm = T),
              n_no_bloom=sum(is.na(bloomdate)),
              max_n_frost_nights=max(n_frost_nights),
              min_n_frost_nights=min(n_frost_nights),
              median_n_frost_nights=median(n_frost_nights))
  
  

  damage_summary_list[[i]] <- frost_szenario_summary
}

damage_summary_df <- bind_rows(damage_summary_list)

#save summary#####
write.csv(damage_summary_df, "data/frost_damage_summary_new.csv")
beep(8)
