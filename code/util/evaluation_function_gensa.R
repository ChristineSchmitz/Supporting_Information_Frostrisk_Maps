evalfun_SA_newpar <- function (x, 
                               modelfn, 
                               bloomJDays, 
                               SeasonList, 
                               Tc = 36, 
                               theta_star = 279, 
                               q10_lower = 1.5, 
                               q10_upper = 3.5,
                               q10_penalty = 10^10,
                               na_penalty = 365) 
{
  #add fixed parameters
  x <- c(x[1:4], theta_star, x[5:8], Tc, x[9:10])
  params <- numeric(4)
  params[1] <- x[5]
  params[2] <- x[6]
  params[3] <- x[7]
  params[4] <- x[8]
  
  #convert parameters
  output <- nleqslv::nleqslv(c(500, 15000), solve_nle, 
                             jac = NULL, params, xscalm = "auto", method = "Newton", 
                             control = list(trace = 0, allowSingular = TRUE))
  if (output$termcd >= 3) {
    E0 <- NA
    E1 <- NA
    A0 <- NA
    A1 <- NA
    return(q10_penalty)
  }
  else {
    E0 <- output$x[1]
    E1 <- output$x[2]
    q = 1/params[1] - 1/params[2]
    A1 <- -exp(E1/params[1])/params[3] * log(1 - exp((E0 - 
                                                        E1) * q))
    A0 <- A1 * exp((E0 - E1)/params[2])
  }
  par <- x
  par[5:8] <- c(E0, E1, A0, A1)
  
  #check for q10 criterion, if failed then penalize
  q10_e0 <- exp((10 * par[5])/(297 * 279))
  q10_e1 <- exp((10 * par[6])/(297 * 279))
  if(q10_e0 < q10_lower | q10_e1 > q10_upper | q10_e1 < q10_lower | q10_e1 > q10_upper){
    return(q10_penalty)
  }
  
  #predict bloom
  pred_bloom <- unlist(lapply(X = SeasonList, FUN = modelfn, 
                              par = par))
  pred_bloom <- ifelse(is.na(pred_bloom), yes = na_penalty, 
                       no = pred_bloom)
  return(sum((pred_bloom - bloomJDays)^2))
}

solve_nle <- function (x, params) 
{
  y <- numeric(2)
  par1 <- params[1]
  par2 <- params[2]
  par3 <- params[3]
  par4 <- params[4]
  T1 <- 297
  T2 <- 279
  eta <- 1/3
  q = 1/par1 - 1/par2
  y[1] <- log((x[1] - x[2])/(exp((x[2] - x[1]) * q) - 1)/log(1 - 
                                                               exp((x[1] - x[2]) * q))) - log(x[2])
  A1 <- -exp(x[2]/par1)/par3 * log(1 - exp((x[1] - x[2]) * 
                                             q))
  k1T1 <- A1 * exp(-x[2]/T1)
  k1T2 <- A1 * exp(-x[2]/T2)
  lhs <- (exp((x[2] - x[1])/par2) - exp((x[2] - x[1])/T1))/(exp((x[2] - 
                                                                   x[1])/T2) - exp((x[2] - x[1])/T1))
  rhs <- (1 - exp(-k1T2 * (1 - eta) * par4))/(1 - exp(-(k1T1 * 
                                                          eta + k1T2 * (1 - eta)) * par4))
  y[2] <- log(lhs) - log(rhs)
  return(y)
}

custom_GDH_wrapper <- function (x, par, constraints = FALSE) 
{
  yc = par[1]
  zc = par[2]
  s1 = par[3]
  Tu = par[4]
  E0 = par[5]
  E1 = par[6]
  A0 = par[7]
  A1 = par[8]
  Tf = par[9]
  Tc = par[10]
  Tb = par[11]
  slope = par[12]
  if (constraints) {
    t1 <- Tu <= Tb
    t2 <- Tc <= Tb
    t3 <- exp((10 * E0)/(297 * 279)) < 1.5 | exp((10 * E0)/(297 * 
                                                              279)) > 3.5
    t4 <- exp((10 * E1)/(297 * 279)) < 1.5 | exp((10 * E0)/(297 * 
                                                              279)) > 3.5
    if (any(c(t1, t2, t3, t4))) {
      return(NA)
    }
  }
  bloomindex <- chillR::PhenoFlex(temp = x$Temp, times = seq_along(x$Temp), 
                                  yc = yc, zc = zc, s1 = s1, Tu = Tu, E0 = E0, E1 = E1, 
                                  A0 = A0, A1 = A1, Tf = Tf, Tc = Tc, Tb = Tb, slope = slope, 
                                  Imodel = 0L, basic_output = TRUE)$bloomindex
  if (bloomindex == 0) {
    return(NA)
  }
  JDay <- x$JDay[bloomindex]
  JDaylist <- which(x$JDay == JDay)
  if (length(unique(x$Year)) == 2 & x$Year[bloomindex] == min(x$Year)) {
    JDay <- JDay - 365
  }
  n <- length(JDaylist)
  if (n == 1) {
    return(JDay)
  }
  return(JDay + which(JDaylist == bloomindex)/n - 1/(n/ceiling(n/2)))
}
