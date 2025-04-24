#setwd('../2024_frostmap_dwd/')
library(tidyverse)
library(chillR)
library(optparse)
#devtools::install_github('https://github.com/larscaspersen/addition_chillR')
#library(LarsChill)
#devtools::install_github('https://github.com/larscaspersen/eval_phenoflex')
#library(evalpheno)



#some pre-set parameters for the calibration experiment

#name the parameters will be saved to
fname <- 'par_early-1993-2022.csv'
#mamximum number of evaluations (this should be the limiting criterion
#time is there to make sure it is not stuck forever
n_evaluations <- 10000
#maximum time an optimization run should take (in seconds)
max_time <- 60 * 60 * 24
#fixed values for theta_star, Tc
theta_star <- 279
Tc <- 36

#this will determine which files to load
i <- 1

#part to read the csv file deciding which job does which calibration task
load('data/input/season-pheno-early-1.RData')
#also read if it is late or early
cult <- 'early'
option_list <- list(make_option("--job-id", type="integer"))
opt <- parse_args(OptionParser(option_list=option_list))



#read evaluation functions
source('code/util/evaluation_function_gensa.R')

#----------#
#run calibration####
#----------#
print(i)


#parameters first guess and bounds
#        yc      zc     s1      Tu      theta_c   tau      piec    Tf     Tb     slope
x_0 <- c(24.79,	337.04,	0.2529,	17.72,	285.54,	   45.67,	  29.49,	2.97,	1.87,	2.69)
x_U <- c(80,    500,    1.0,    30,     287,       48,      50,    10,    10,     5.00)
x_L <- c(20,    100,    0.1,    15,     284,       16,      24,     2,     2,     1.2)


control <- list(smooth = FALSE, 
                verbose = TRUE, 
                max.time = 2,
                #maxit = 10,
                nb.stop.improvement = 250)

start <- Sys.time() %>% round(units = 'secs') %>% hms::as_hms()
set.seed(123456)
res <- GenSA::GenSA(par = x_0, 
                    fn = evalfun_SA_newpar,
                    bloomJDays = pheno_sub$yday,
                    SeasonList = seasonlist,
                    modelfn = custom_GDH_wrapper,
                    control = control, 
                    lower = x_L, 
                    upper = x_U)
end <- Sys.time() %>% round(units = 'secs') %>% hms::as_hms()
run.time <- hms::as_hms(end - start)



#put some basic information next to the estimated parameters in the table

df_fit <- data.frame(par = c('yc', 'zc', 's1', 'Tu', 'theta_c', 'tau', 'piec', 'Tf', 'Tb', 'slope', 'theta_star', 'Tc'),
                     value = c(res$par,theta_star, Tc),
                     repetition = i,
                     cultivar = cult,
                     counts = res$counts,
                     control.maxtime = control$max.time,
                     date = Sys.Date(),
                     start.time = rep(start, 12),
                     end.time = rep(end, 12),
                     run.time = rep(run.time, 12),
                     rss = res$value,
                     par.start = c(x_0, theta_star, Tc),
                     par.upper = c(x_U, theta_star, Tc),
                     par.lower = c(x_L, theta_star, Tc)) 

#flag if the table is appended or newly created
append_file <- FALSE
add_colnames <- TRUE
if(file.exists(paste0('data/', fname))){
  append_file <- TRUE
  add_colnames <- FALSE
} 
write.table(df_fit, file = paste0('data/', fname), row.names = FALSE, append = append_file, sep = ',', col.names = add_colnames)

