library(dplyr)
library(zoo) # For rollapply
#library(pCODE)
library(mgcv)

#Setup
glacier_list <-c('G077333E32259N', 'G077314E43095N','G077314E43095N','G077358E32209N','G007276E45599N', 'G072559E37965N', 'G077749E41787N', 'G078184E41824N', 'G077342E32257N','G077277E43048N','G077513E32227N')
#glacier_list <- c('G072559E37965N')

setwd("C:\\Users\\shash\\OneDrive\\Desktop\\Research\\Glacier Revamp")

source("PCODE.R")

for(glacier in glacier_list){

  outs = readRDS(paste0("output/debug/05_clustering/output/", glacier,"_curves_list.rds"))
  first_cluster = outs[[1]]
  L = first_cluster$mean
  tt = readRDS(paste0("output/debug/03_extract_IP/output/",glacier, "_dates_cut.rds"))
  al = readRDS(paste0("output/debug/03_extract_IP/output/",glacier, "_al.rds"))
  csv_filename = paste0("data/GEE/",glacier,"_env.csv")
  sSmooth = readRDS(paste0("output/debug/04_candidate_paths/output/",glacier,"_sSmooth.rds"))
  term_path = readRDS(paste0("output/debug/04_candidate_paths/output/", glacier, "_candidate_paths.rds"))

  
  dd3 = sSmooth$dd3
  
  closest_indices <- sapply(L, function(target) {
    which.min(abs(al - target))
  })
  
  muT = function( obj, term_path, k){ obj[k,term_path[k]]}
  wts <- sapply( seq(1,nrow(dd3),1), muT, obj = dd3, term_path = closest_indices)
  
  overall_mean_L <- mean(L, na.rm = TRUE)
  L_centered <- L - overall_mean_L
  plot(tt,L_centered, main="L vs time")
  
  env_data <- read.csv(csv_filename)
  env_data$Date <- as.Date(with(env_data, paste(Year, Month, "01", sep="-")), "%Y-%m-%d")
  # lines(env_data$Date, env_data$Skin_Temperature)
  # lines(env_data$Date, env_data$Skin_Temperature)
  
  overall_mean_skin_temperature <- mean(env_data$Skin_Temperature, na.rm = TRUE)
  overall_mean_precipitation <- mean(env_data$Total_Precipitation, na.rm = TRUE)
  
  overall_mean_summer_temperature <- env_data %>%
    filter(Month %in% c(5, 6, 7, 8)) %>%
    summarise(Mean = mean(Temperature_2m, na.rm = TRUE)) %>%
    pull(Mean)
  
  env_data_adjusted <- env_data %>%
    mutate(
      SkinTemperatureDeparture = Skin_Temperature - overall_mean_skin_temperature,
      PrecipitationDeparture = Total_Precipitation - overall_mean_precipitation,
      SummerTemperatureDeparture = ifelse(Month %in% c(5, 6, 7, 8), Temperature_2m - overall_mean_summer_temperature, NA)
    )
  
  annual_data_adjusted <- env_data_adjusted %>%
    group_by(Year) %>%
    summarise(
      AvgSkinTempDeparture = mean(SkinTemperatureDeparture, na.rm = TRUE),
      AvgPrecipDeparture = mean(PrecipitationDeparture, na.rm = TRUE)
    )
  
  summer_data_adjusted <- env_data_adjusted %>%
    filter(Month %in% c(5, 6, 7, 8)) %>%
    group_by(Year) %>%
    summarise(
      SummerAvgTempDeparture = mean(SummerTemperatureDeparture, na.rm = TRUE)
    )
  
  annual_data_rolling_adjusted <- annual_data_adjusted %>%
    mutate(
      SkinTempDepartureRolling5Yr = rollapply(AvgSkinTempDeparture, 5, mean, partial = TRUE, align = "right"),
      PrecipDepartureRolling5Yr = rollapply(AvgPrecipDeparture, 5, mean, partial = TRUE, align = "right")
    )
  
  summer_data_rolling_adjusted <- summer_data_adjusted %>%
    mutate(
      SummerTempDepartureRolling5Yr = rollapply(SummerAvgTempDeparture, 5, mean, partial = TRUE, align = "right")
    )
  
  plot(annual_data_rolling_adjusted$Year, annual_data_rolling_adjusted$TemperatureRolling5Yr, type = "l",xlab = "Year", ylab = "5-Year Rolling Average",col = "blue",main = "5-Year Rolling Averages of Temperature")
  
 # define functions for interpolation
  
  interpolate_summer_temp_rolling <- function(input_year) {
    return(approx(x = summer_data_rolling_adjusted$Year, 
                                 y = summer_data_rolling_adjusted$SummerTempDepartureRolling5Yr, 
                                 xout = input_year, 
                                 rule = 2)$y)
  }
  
  interpolate_precip_rolling <- function(input_year) {
    return(approx(x = annual_data_rolling_adjusted$Year, 
                                 y = annual_data_rolling_adjusted$PrecipDepartureRolling5Yr, 
                                 xout = input_year, 
                                 rule = 2)$y)
  }
  
  
  ode_system <- function(t, state, parameters) {
    with(as.list(c(state, parameters)), {
      T_val = interpolate_summer_temp_rolling(t)
      P_val = interpolate_precip_rolling(t)
      dL_dt <- alpha * T_val + beta * P_val + gamma - X / tau_param
      return(list(dL_dt))
    })
  }

  
  par.names = c('alpha', 'beta', 'gamma', 'tau_param')
  
  # Spline basis for L, T, P
  number_of_knots = min(round(length(tt)/4)+4, 35+4)
  number_of_knots = 10
  times = tt
  breaks <- quantile(times, probs = seq(0, 1, length.out = number_of_knots + 2)) # +2 for the boundary knots
  b_spline_basis = create.bspline.basis(range(tt), norder = 6, breaks = breaks)


  X_t = eval.basis(tt,b_spline_basis)
  
  spline_fit = mgcv::gam(L_centered ~ -1 + X_t, fx = TRUE)
  fitted_values <- predict(spline_fit, newdata = data.frame(X_t))
  plot(tt, L_centered, type = 'l', col = 'blue', xlab = 'Time', ylab = 'L Centered', main = 'Original L Centered vs. Spline Fit')
  lines(tt, fitted_values, col = 'red')
  legend("topright", legend=c("Original L Centered", "Spline Fit"), col=c("blue", "red"), lty=1)
  

  
  par.initial <- c(alpha = 3.2, beta = -1, gamma = -4, tau_param = 20)
  state.names <- c('L_val')
  L_centered = as.numeric(L_centered
                          )
  result <- pcode(data = L_centered, time = times, ode.model = ode_system,
                  par.names = par.names,
                  par.initial = par.initial,
                  state.names = state.names,
                  basis.list = b_spline_basis, lambda = 10, controls = c(max_eval = 40))
  
  
  mean_observed <- mean(L_centered)
  

  
  params_estimated = result$structural.par
  initial_state <- c(X = L_centered[1])
  times = tt
  sim_results <- ode(y = initial_state, times = times, func = ode_system, parms = params_estimated)
  
  total_sum_squares <- sum((L_centered - mean_observed)^2)
  residual_sum_squares <- sum((L_centered - sim_results[,2])^2)
  R_squared <- 1 - (residual_sum_squares / total_sum_squares)
  
  plot_name = paste(glacier,'-',R_squared)
  plot(tt, L_centered, type='l', col='blue', xlab='Time', ylab='L', main=plot_name)
  lines(tt, sim_results[,2], col='green') # Overlay the modeled L
  lines(tt, fitted_values, col = 'red')
  legend('topright', legend=c('Original L', 'Modeled L', 'Spline Fit'), col=c('blue', 'green', 'red'), lty=1)
  
  residuals <- L_centered - sim_results[,2]
  
  plot(tt, residuals, xlab = "Time", ylab = "Residuals", main = plot_name, pch = 20, col = 'blue')
  print(glacier)
  print(result$structural.par)
  

  

  
}
