#fit sigmoide curves for frost damage depending on temperature####
#define sigmoid curve
sigmoid <- function(x, L, k, x0) {
  L / (1 + exp(-k * (x - x0)))
}


residual <- function(params, x, y) {
  k <- params[1]
  x0 <- params[2]
  y_pred <- 1 / (1 + exp(-k * (x - x0)))# L could be set to 1 i.e. 100% damage
  sum((y - y_pred)^2)  # Sum of squared errors
}

#function to compute temperature in °C from data in °F 
fahrenheit_to_celsius<-function(Temp){
  celsius<-(Temp-32)*(5/9)
  return(celsius)
}

#read in critical temperatures and transform to °C 
crit_temp<-read.csv2("data/crit_temp_ballard.csv")
crit_temp$kill_10_C<-round(fahrenheit_to_celsius(crit_temp$kill_10_F),digits = 1)
crit_temp$kill_90_C<-round(fahrenheit_to_celsius(crit_temp$kill_90_F),digits = 1)

crit_temp$k_fit<-rep(NA, 9)
crit_temp$x0_fit<-rep(NA, 9)

#fit sigmoid frost damage curves for all stages
for(i in 1:9){#for 9 stages used in Ballard et al. 1981
  x_points<-c(0, crit_temp[i,]$kill_10_C, crit_temp[i,]$kill_90_C)
  y_points <- c(0,0.1, 0.9)
  initial_params <- c(k = 1, x0 = mean(crit_temp[i,]$kill_10_C, crit_temp[i,]$kill_90_C))
  fit <- optim(
    par = initial_params,
    fn = residual,
    x = x_points,
    y = y_points
  )
  crit_temp$k_fit[i] <- fit$par[1]
  crit_temp$x0_fit[i] <- fit$par[2]
}

# Generate data for plotting
x_vals <- seq(-20, 2, length.out = 100)

y_vals_silver_tip <- 1 / (1 + exp(-crit_temp[1,]$k_fit * (x_vals - crit_temp[1,]$x0_fit)))
y_vals_green_tip <- 1 / (1 + exp(-crit_temp[2,]$k_fit * (x_vals - crit_temp[2,]$x0_fit)))
y_vals_half_green <- 1 / (1 + exp(-crit_temp[3,]$k_fit * (x_vals - crit_temp[3,]$x0_fit)))
y_vals_tight_cluster <- 1 / (1 + exp(-crit_temp[4,]$k_fit * (x_vals - crit_temp[4,]$x0_fit)))
y_vals_first_pink <- 1 / (1 + exp(-crit_temp[5,]$k_fit * (x_vals - crit_temp[5,]$x0_fit)))
y_vals_full_pink <- 1 / (1 + exp(-crit_temp[6,]$k_fit * (x_vals - crit_temp[6,]$x0_fit)))
y_vals_first_bloom <- 1 / (1 + exp(-crit_temp[7,]$k_fit * (x_vals - crit_temp[7,]$x0_fit)))
y_vals_full_bloom <- 1 / (1 + exp(-crit_temp[8,]$k_fit * (x_vals - crit_temp[8,]$x0_fit)))
y_vals_post_bloom <- 1 / (1 + exp(-crit_temp[9,]$k_fit * (x_vals - crit_temp[9,]$x0_fit)))

# Plot curves ####

png("figures/plot_sigmoide_curves_percent.png", width = 6000, height = 4500, res = 600)
plot(x_points, y_points*100, pch = 16, col = "red", xlab = "temperature [°C]",
     ylab = "frost damage [%]",
     #main = "Fitted Sigmoid",
     xlim = c((-20),2), ylim = c(0,1*100))
#lines(x_vals, y_vals_silver_tip*100, col = "blue")
lines(x_vals, y_vals_green_tip*100, col = "red")
lines(x_vals, y_vals_half_green*100, col = "yellow")
lines(x_vals, y_vals_tight_cluster*100, col = "black")
lines(x_vals, y_vals_first_pink*100, col = "pink")
lines(x_vals, y_vals_full_pink*100, col = "green")
#points(c(0, crit_temp[1,]$kill_10_C, crit_temp[1,]$kill_90_C),y_points*100,pch = 16, col = "red",)
points(c(0, crit_temp[2,]$kill_10_C, crit_temp[2,]$kill_90_C),y_points*100,pch = 16, col = "red",)
points(c(0, crit_temp[3,]$kill_10_C, crit_temp[3,]$kill_90_C),y_points*100,pch = 16, col = "red",)
points(c(0, crit_temp[4,]$kill_10_C, crit_temp[4,]$kill_90_C),y_points*100,pch = 16, col = "red",)
points(c(0, crit_temp[5,]$kill_10_C, crit_temp[5,]$kill_90_C),y_points*100,pch = 16, col = "red",)
legend((-19), 0.3*100, legend = c(#"silver tip",
                                  "green tip","half green","tight cluster","first pink", "full pink and bloom"),
       fill = c(#"blue",
                "red","yellow","black","pink","green"))
dev.off()

#save results
write.csv(crit_temp, "data/crit_temp_and_curve_fit.csv")
