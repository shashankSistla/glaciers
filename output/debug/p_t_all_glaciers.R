
calculate_derivative_using_finite_difference_spline <- function(y,x, n_basis_functions = 10, eps = 1e-7, x_dense_length = 1000){
  fit <- gam(y ~ s(x, bs = "cr", k = n_basis_functions, fx = TRUE), data = data.frame(x, y))
  summary(fit)
  
  eps <- 1e-7 ## finite difference interval
  # denser x to plot
  x_dense <- seq(min(x), max(x), length.out = x_dense_length)
  x_dense_plus_eps = x_dense + eps
  
  
  x_df0 <- data.frame(x = x_dense)
  x_df1 <- data.frame(x = x_dense_plus_eps)
  
  
  X0 <- predict(fit,x_df0,type="lpmatrix")
  X1 <- predict(fit,x_df1,type="lpmatrix")
  
  y0 <- X0%*%coef(fit)
  
  
  
  Xp <- (X1-X0)/eps ## maps coefficients to (fd approx.) derivatives
  
  deriv <- Xp%*%coef(fit)
  deriv.sd <- rowSums(Xp%*%fit$Vp*Xp)^.5 ## cheap diag(Xi%*%b$Vp%*%t(Xi))^.5
  return(list(deriv = deriv, deriv.sd = deriv.sd, x_dense = x_dense))
}

# Load necessary libraries
library(ggplot2)
library(mgcv)

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  fit <- lm(y ~ x)
  # Calculate R^2
  r2 <- summary(fit)$r.squared
  txt <- format(c(r2, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r2)
}

panel.lm <- function(x, y, ...) {
  points(x, y, ...)
  fit <- lm(y ~ x)
  coefs <- coef(fit)
  if (all(is.finite(coefs))) {
    abline(fit, col = "red")
  } }



##########
#The actual regression
library(zoo)
library(dplyr)
#Setup
glacier_list <-c('G077333E32259N', 'G077314E43095N','G077314E43095N','G077358E32209N','G007276E45599N', 'G072559E37965N', 'G077749E41787N', 'G078184E41824N', 'G077342E32257N','G077277E43048N','G077513E32227N')
#glacier_list <- c('G077749E41787N')

setwd("C:\\Users\\shash\\OneDrive\\Desktop\\Research\\Glacier Revamp")


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


#plot(annual_data_rolling$Year, summer_data_rolling$SummerTemperatureRolling5Yr, type = "l", col = "red", main="5-Year Rolling averages Summer temperatue")

#plot(annual_data_rolling$Year, annual_data_rolling$PrecipitationRolling5Yr, type = "l", col = "darkgreen", main="5 year rolling precipitation")

# I have L and dL/dt, but need to convert them into yearwise values as well
years_range <- floor(min(tt)):floor(max(tt)) + 0.5
L_interp <- approx(x = tt, y = L, xout = years_range)$y
L_interp <- approxfun(x = tt, y = L, method = "linear", rule = 2)(years_range)

df_interp <- approx(x = L_prime$x_dense, y = L_prime$deriv, xout = years_range)$y
df_interp <- approxfun(x = L_prime$x_dense, y =  L_prime$deriv, method = "linear", rule = 2)(years_range)

basis.list <- list(basis_X, basis_Y)

data.frame <- data.frame(dL = df_interp, L = L_interp, temp =  summer_data_rolling_adjusted[[2]], prec =annual_data_rolling_adjusted[[4]])
pairs(data.frame, upper.panel = panel.lm, lower.panel = panel.cor, main=glacier)

# df_cumsum = sapply(data.frame, cumsum)
# pairs(data.frame)
# pairs(df_cumsum)

# rough estimates of derivatives
# T_prime <- c(NA, diff(annual_data$AverageTemperature))
# T_double_prime <- c(NA, diff(T_prime))
# 
# P_prime <- c(NA, diff(annual_data$AveragePrecipitation))
# P_double_prime <- c(NA, diff(P_prime))
# 
# # Glacier Length derivatives (L'' and L''')
# L_double_prime <- c(NA, diff(L_prime$deriv))
# L_triple_prime <- c(NA, diff(L_double_prime))
# 
# # Add these to your data frame
# data.frame <- transform(data.frame, T_prime = T_prime, T_double_prime = T_double_prime,
#                         P_prime = P_prime, P_double_prime = P_double_prime,
#                         L_double_prime = approx(x = L_prime$x_dense, y = L_double_prime, xout = years_range)$y,
#                         L_triple_prime = approx(x = L_prime$x_dense, y = L_triple_prime, xout = years_range)$y)


#trying out the lag formula

# length_df = length(data.frame)
# # Correcting for the appropriate length of the data frame
# data.frame$L_lag1 <- c(NA, data.frame$L[1:(length_df)])
# data.frame$L_lag2 <- c(NA, NA, data.frame$L[1:(length_df-2)])
# data.frame$L_lag3 <- c(NA, NA, NA, data.frame$L[1:(length_df-3)])
# #
# data.frame$T_lag3 <- c(NA, NA, NA, data.frame$temp, -3)
# data.frame$P_lag3 <- c(NA, NA, NA, data.frame$prec, -3)
# relevant_columns <- data.frame[, c("L", "L_lag1", "L_lag2", "L_lag3", "T_lag3", "P_lag3")]
# pairs(relevant_columns, main = "Pairs Plot with Lags")



# pairs(data.frame, main=glacier)

}

library(pCODE)



