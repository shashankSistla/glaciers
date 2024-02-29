library(dplyr)
library(zoo) # For rollapply
#library(pCODE)

#Setup
glacier_list <-c('G077333E32259N', 'G077314E43095N','G077314E43095N','G077358E32209N','G007276E45599N', 'G072559E37965N', 'G077749E41787N', 'G078184E41824N', 'G077342E32257N','G077277E43048N','G077513E32227N')
#glacier_list <- c('G077749E41787N')

setwd("C:\\Users\\shash\\OneDrive\\Desktop\\Research\\Glacier Revamp")

source("PCODE.R")

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
  
  env_data <- read.csv(csv_filename)
  env_data$Date <- as.Date(with(env_data, paste(Year, Month, "01", sep="-")), "%Y-%m-%d")
  # lines(env_data$Date, env_data$Skin_Temperature)
  # lines(env_data$Date, env_data$Skin_Temperature)
  
  overall_mean_skin_temperature <- mean(env_data$Skin_Temperature, na.rm = TRUE)
  overall_mean_precipitation <- mean(env_data$Total_Precipitation, na.rm = TRUE)
  
  temp_above_273 <- env_data %>%
    filter(Temperature_2m > 273) %>%
    select(Year, Month, Temperature_2m)
  
  # overall_mean_summer_temperature <- env_data %>%
  #   filter(Month %in% c(5, 6, 7, 8)) %>%
  #   summarise(Mean = mean(Temperature_2m, na.rm = TRUE)) %>%
  #   pull(Mean)
  
  overall_mean_summer_temperature <- temp_above_273 %>%
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
  
  years_range <- floor(min(tt)):floor(max(tt)) + 0.5
  
  #L_interp <- approx(x = tt, y = L, xout = years_range)$y
  L_interp <- approxfun(x = tt, y = L, method = "linear", rule = 2)(years_range)
  
 
  
  data.frame <- data.frame(L = L_interp, temp =  summer_data_rolling_adjusted[[2]], prec =annual_data_rolling_adjusted[[4]])
  pairs(data.frame, main=glacier)
  T_values = summer_data_rolling_adjusted[[2]]
  P_values = annual_data_rolling_adjusted[[4]]
  times = summer_data_rolling_adjusted[[1]]
  
  get_T_val <- function(t){
    index = t - min(times) + 1
    print(index)
    return(T_values[index])
  }
  get_P_val <- function(t){
    index = t - min(times) + 1
    print(index)
    return(P_values[index])
  }
  
  
  #P code stuff begin
  ode_system <- function(t, state, parameters) {
    print("State is")
    print(state)
    with(as.list(c(state, parameters)), {
      T_val = get_T_val(t)
      P_val = get_P_val(t)
      dL_dt <- alpha * T_val + beta * P_val + gamma - L / tau_param
      return(list(dL_dt))
    })
  }
  
  par.names = c('alpha', 'beta', 'gamma', 'tau_param')
  
  # Spline basis for L, T, P
  number_of_knots = 10
  breaks <- quantile(times, probs = seq(0, 1, length.out = number_of_knots + 2)) # +2 for the boundary knots
  norder <- 4
  nbasis <- norder + length(breaks) - 2
  
  b_spline_basis_L <- create.bspline.basis(range = range(times), nbasis = nbasis, norder = norder, breaks = breaks)
  

  data = cbind(L_interp)
  par.initial <- c(alpha = 0.1, beta = 0.2, gamma = 0.3, tau_param = 20)
  state.names <- c('L')
  
  result <- pcode(data = L_interp, time = times, ode.model = ode_system,
                  par.names = par.names,
                  par.initial = par.initial,
                  state.names = state.names,
                  basis.list = b_spline_basis_L, lambda = 1e2)
}

  
# Ensure data is a matrix with correct dimensions
data <- cbind(L_interp, T_values, P_values)
if (!is.matrix(data)) {
  print("OH NO")
  data <- as.matrix(data)
}

# Ensure the 'times' vector matches the observation times exactly
if (length(times) != nrow(data)) {
  stop("Mismatch between the length of 'times' and the number of rows in 'data'")
}

# Check that 'state.names' and 'par.names' match your model's requirements
print(state.names)  # Should match the states your model uses
print(par.names)    # Should match the parameters your model uses

# Before calling pcode, print out dimensions to check
print(dim(data))
print(length(times))
