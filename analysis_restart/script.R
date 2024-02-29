library(dplyr)
library(zoo) # For rollapply
#library(pCODE)
library(mgcv)
library(fda)
library(numDeriv)
library(base)
library(pracma)



#Setup
glacier_list <-c('G077333E32259N', 'G077314E43095N','G077314E43095N','G077358E32209N','G007276E45599N', 'G072559E37965N', 'G077749E41787N', 'G078184E41824N', 'G077342E32257N','G077277E43048N','G077513E32227N')
glacier_list <- c('G007880E45990N')

setwd("C:\\Users\\shash\\OneDrive\\Desktop\\Research\\Glacier Revamp")

source("PCODE.R")
key = "restart"

print("Running for key")
print(key)

for(glacier in glacier_list){
  candidate_paths = readRDS(paste0("output/",key,"/04_candidate_paths/output/", glacier,"_candidate_paths.rds"))
 
  min_indices = readRDS(paste0("output/",key,"/05_clustering/output/", glacier,"_min_cost_indices.rds"))
  al = readRDS(paste0("output/",key,"/03_extract_IP/output/", glacier,"_al.rds"))
  ts_intensity = readRDS(paste0("output/",key,"/03_extract_IP/output/", glacier,"_ts_intensity.rds"))
  
  al = c(0,cumsum(rep(al[length(al)]/(ncol(ts_intensity)-1), (ncol(ts_intensity)-1))))
  tt = readRDS(paste0("output/",key,"/03_extract_IP/output/",glacier, "_dates_cut.rds"))
  csv_filename = paste0("data/GEE/",glacier,"_env.csv")

    print("Loaded all data")
  
  outs <- lapply(candidate_paths, function(indices) {
    al[indices]
  })

  selected_paths = outs[min_indices]
  
  env_data <- read.csv(csv_filename)
  env_data$Date <- as.Date(with(env_data, paste(Year, Month, "01", sep="-")), "%Y-%m-%d")
  # lines(env_data$Date, env_data$Skin_Temperature)
  # lines(env_data$Date, env_data$Skin_Temperature)
  overall_mean_precipitation <- mean(env_data$Total_Precipitation, na.rm = TRUE)
  
  temp_above_273 <- env_data %>%
    filter(Temperature_2m > 273) %>%
    dplyr::select(Year, Month, Temperature_2m)
  
  
  overall_mean_summer_temperature <- temp_above_273 %>%
    summarise(Mean = mean(Temperature_2m, na.rm = TRUE)) %>%
    pull(Mean)
  
  env_data <- env_data %>%
    mutate(
      IsHighTempMonth = paste(Year, Month) %in% paste(temp_above_273$Year, temp_above_273$Month)
    )
  
  env_data <- env_data %>%
    mutate(
      PrecipitationDeparture = Total_Precipitation - overall_mean_precipitation,
      SummerTemperatureDeparture = ifelse(IsHighTempMonth, Temperature_2m - overall_mean_summer_temperature, NA)
    )
  
  env_data <- env_data %>%
    mutate(
      SummerTempDepartureRolling5Yr = rollapply(SummerTemperatureDeparture, 5, mean, partial = TRUE, align = "right"),
      PrecipDepartureRolling5Yr = rollapply(PrecipitationDeparture, 5, mean, partial = TRUE, align = "right")
    )
  
  env_data <- env_data %>%
    mutate(
      SummerTempDepartureRolling5Yr = rollapply(SummerTemperatureDeparture, 5, 
                                                function(x) mean(x, na.rm = TRUE), 
                                                partial = TRUE, align = "right")
    )

     print("Finished calculating env data")
  
  interpolate_summer_temp_rolling <- function(input_year) {
    return(approx(x = env_data$Year, 
                  y = env_data$SummerTempDepartureRolling5Yr, 
                  xout = input_year, 
                  rule = 2)$y)
  }
  
  interpolate_precip_rolling <- function(input_year) {
    return(approx(x = env_data$Year, 
                  y = env_data$PrecipDepartureRolling5Yr, 
                  xout = input_year, 
                  rule = 2)$y)
  }
  
  
  
  i = 0
  for(path in selected_paths){
i = i + 1
    print("Processing path")
    print(i)
    L = path
    overall_mean_L <- mean(L, na.rm = TRUE)
    L_centered <- L - overall_mean_L
    plot(tt,L_centered, main="L vs time")
    
    
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
    number_of_knots = 10
    times = tt
    breaks <- quantile(times, probs = seq(0, 1, length.out = number_of_knots + 2)) # +2 for the boundary knots
    norder <- 4
    nbasis <- norder + length(breaks) - 2
    
    
    b_spline_basis_L <- create.bspline.basis(range = range(times), nbasis = nbasis, norder = norder, breaks = breaks)
    X_t = eval.basis(tt,b_spline_basis_L)
    
    spline_fit = mgcv::gam(L_centered ~ -1 + X_t, fx = TRUE)
    fitted_values <- predict(spline_fit, newdata = data.frame(X_t))
    plot_name =  paste0("analysis_dir/",key,glacier,"_",i,"_original vs fit.png")
    png(plot_name)
    plot(tt, L_centered, type = 'l', col = 'blue', xlab = 'Time', ylab = 'L Centered', main = 'Original L Centered vs. Spline Fit')
    lines(tt, fitted_values, col = 'red')
    legend("topright", legend=c("Original L Centered", "Spline Fit"), col=c("blue", "red"), lty=1)
    dev.off()
    par.initial <- c(alpha = 3.2, beta = -1, gamma = -4, tau_param = 20)
    state.names <- c('L_val')
    L_centered = as.numeric(L_centered)
    
    result <- pcode(data = L_centered, time = times, ode.model = ode_system,
                    par.names = par.names,
                    par.initial = par.initial,
                    state.names = state.names,
                    basis.list = b_spline_basis_L, lambda = 1e2, controls = c(max_eval = 40))
    
    
    mean_observed <- mean(L_centered)
    
    total_sum_squares <- sum((L_centered - mean_observed)^2)
    
    residual_sum_squares <- sum((L_centered - fitted_values)^2)
    
    R_squared <- 1 - (residual_sum_squares / total_sum_squares)
    
    
    
    params_estimated = result$structural.par
    initial_state <- c(X = L_centered[1])
    times = tt
    sim_results <- ode(y = initial_state, times = times, func = ode_system, parms = params_estimated)
    
    saveRDS(list(R_squared = R_squared, params_estimated = params_estimated, result = result, sim_results = sim_results), file = paste0(glacier,"_",i,"_results.rds"))
    plot_name =  paste0("analysis_dir/",key,glacier,"_",i,"_simulated.png")
    png(plot_name)
    plot(tt, L_centered, type='l', col='blue', xlab='Time', ylab='L', main=plot_name)
    lines(tt, sim_results[,2], col='red') # Overlay the modeled L
    legend('topright', legend=c('Original L', 'Modeled L'), col=c('blue', 'red'), lty=1)
    dev.off()
    # Calculate residuals
    residuals <- L_centered - fitted_values
    plot_name =  paste0("analysis_dir/",key,glacier,"_",i,"_residuals.png")
    png(plot_name)
    # Plot the residuals
    plot(tt, residuals, xlab = "Time", ylab = "Residuals", main = plot_name, pch = 20, col = 'blue')
    print(glacier)
    print(result$structural.par)
    dev.off()
    
  }
}

