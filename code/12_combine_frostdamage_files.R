#load all the rds files and save to some combined files####
library(tidyverse)
flist <- list.files('data/output/frost-damage/', pattern = '*.rds', full.names = TRUE)

#always take 3000 files and bind to one
path_out <- 'data/output/'



#group by ssp and cultivar and repetition####

for(r in 1:10){
  for(cult in c('early', 'late')){
    for(ssp in c('ssp126', 'ssp245', 'ssp370', 'ssp585')){
      
      print(paste(r, cult, ssp, sep = ' '))
      
      search_pat <- paste0('frostdamage\\.', cult, '\\.', r, '\\..*\\.', ssp, '.*\\.rds$')
      i_files <- grep(search_pat, x = flist)
      if(length(i_files)< 1) next()
      fname <- paste0('frostdamage.', cult, '.', r, '.', ssp, '.rds')
      if(file.exists(paste0(path_out, fname))) next()
      
      purrr::map(flist[i_files], function(f){
        #remove the path from file name
        add_info <- str_split(string = f, pattern = '/') %>% purrr::map_chr(function(x) return(x[length(x)]))
        #remove file extension and frostdamage from file name
        add_info <- gsub(pattern = 'frostdamage.', '', add_info)
        add_info <- gsub(pattern = '.rds', '', add_info)
        
        df <- readRDS(f) %>% 
          mutate(id = add_info) %>% 
          return()
      }) %>% 
        do.call('rbind',.) %>% 
        saveRDS(file = paste0(path_out, fname))
      
    }
  }
}





