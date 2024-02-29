library(pCODE)
library(deSolve)
library(fda)
library(MASS)
library(pracma)
library(Hmisc)

glacier_list <- c('G077749E41787N')

for(glacier in glacier_list){
  outs = readRDS(paste0("output/debug/04_candidate_paths/output/", glacier,"_outs.rds"))
  out1 = outs$out1
  L = out1$pred
  fit = out1$err$fit_1
  tt = readRDS(paste0("output/debug/03_extract_IP/output/",glacier, "_dates_cut.rds"))
  csv_filename = paste0("data/GEE/",glacier,"_env.csv")
  
  # L
  L = outs$out1$pred
  overall_mean_L <- mean(L, na.rm = TRUE)
  L_centered <- L - overall_mean_L
  plot(tt,L_centered, main="L vs time")
  
  L_prime = calculate_derivative_using_finite_difference_spline(L_centered, tt)
  
  env_data <- read.csv(csv_filename)
  env_data$Date <- as.Date(with(env_data, paste(Year, Month, "01", sep="-")), "%Y-%m-%d")
  # lines(env_data$Date, env_data$Skin_Temperature)
  # lines(env_data$Date, env_data$Skin_Temperature)
  
  library(dplyr)
  library(zoo) # For rollapply
  
  
  # Calculating annual and summer averages
  library(dplyr)
  library(zoo) # For rollapply
  
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
  
  T_interpolated <- approxfun(env_data_adjusted$Year, env_data_adjusted$SummerTempDepartureRolling5Yr, rule = 2)
  P_interpolated <- approxfun(env_data_adjusted$Year, env_data_adjusted$PrecipDepartureRolling5Yr, rule = 2)
  
  
  tt_subset = tt[2 : (length(tt) -1)]
  p_seq = seq(0,1, length.out = number_of_knots)
  knots_positions = quantile(tt_subset, p = p_seq)
  range_tt = c(tt[1], tt[length(tt)]); 
  b_spline_basis = create.bspline.basis(range_tt, norder = 6, breaks = knots_positions)
  number_of_knots <- 10 # Example value
  knots_positions <- quantile(tt, probs = seq(0, 1, length.out = number_of_knots))
  breaks <- quantile(tt, probs = seq(0, 1, length.out = number_of_knots + 2)) # +2 for the boundary knots
  nbasis <- norder + length(breaks) - 2
  b_spline_basis <- create.bspline.basis(range = range(tt), nbasis = nbasis, norder = norder, breaks = breaks)
  
  data <- data.frame(L = L_centered, Time = tt)
  
  ode.model <- function(time, state, parameters) {
    L <- state # Assuming state is a named vector with 'L'
    alpha <- parameters['alpha']
    beta <- parameters['beta']
    gamma <- parameters['gamma']
    tau <- parameters['tau']
    

    T_val <- T_interpolated(time)
    P_val <- P_interpolated(time)
    
    dL_dt <- alpha * T_val + beta * P_val + gamma - L / tau
    
    return(list(dL_dt))
  }
  
  par.initial <- c(alpha = 0.1, beta = 0.1, gamma = 0.1, tau = 10)
  
  # Executing parameter estimation using pCODE
  pcode.result <- pcode(data = observ, time = times, ode.model = ode.model,
                        par.initial = par.initial, par.names = c('alpha', 'beta', 'gamma', 'tau'),
                        state.names = 'L',
                        basis.list = list(b_spline_basis), lambda = 1e2)
  
  
  
}
  