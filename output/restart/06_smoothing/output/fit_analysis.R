
library(mgcv)
library(fda)
library(dplyr)
library(zoo)

# Function to get GAM coefficients
gam_to_fda <- function(gam_model, x, rangeval, norder = 4) {
  knots <- gam_model$smooth[[1]]$xp
  
  bspline_basis <- create.bspline.basis(rangeval = rangeval, breaks = knots, norder = norder)
  
  basis_gam <- predict(gam_model, type = "lpmatrix")
  print(dim(basis_gam))
  
  fda_basis_eval <- eval.basis(x, bspline_basis)
  print(dim(fda_basis_eval))
  
  coefficients_matrix <- matrix(nrow = ncol(fda_basis_eval), ncol = ncol(basis_gam))
  
  for (i in 1:ncol(basis_gam)) {
    fit <- lm(basis_gam[, i] ~ fda_basis_eval - 1)  # Fit without intercept
    coefficients_matrix[, i] <- coef(fit)
  }
  
  return(list(coefficients_matrix = coefficients_matrix, bspline_basis = bspline_basis))
}


integrate_BtL <- function(basis, L, t, coef_mat, gam_coefs) {
  B <- eval.basis(t, basis)
  print(dim(B))
  print(dim(B %*% coef_mat))
  dt <- diff(c(0, t))  # Non-uniform spacing
  BtL <- t(B) %*% (L * dt)
  BtB <- integrate_BtB(basis, t, coef_mat)
  beta <- solve(BtB, BtL)
  return(beta)
}


integrate_BtL_deriv <- function(basis, L, t, coef_mat, gam_coefs) {
  B_deriv <- eval.basis(t, basis, Lfdobj = 1) %*% coef_mat
  #print(dim(B_deriv))
  dt <- diff(c(0, t))  # Non-uniform spacing
  BtL_deriv <- t(B_deriv) %*% (L * dt)
  BtB <- integrate_BtB(basis, t, coef_mat)
  print(kappa(BtB))
  beta_deriv <- solve(BtB, -BtL_deriv)
  return(beta_deriv)
}


integrate_BtB <- function(basis, t,coef_mat, gam_coefs) {
  B <- eval.basis(t, basis) %*%  coef_mat
  dt <- diff(c(0, t))  # Non-uniform spacing
  BtB <- t(B) %*% (B*dt)
  return(BtB)
}


setwd("C:\\Users\\shash\\OneDrive\\Desktop\\Research\\Glacier Revamp\\output\\restart\\06_smoothing\\output")
file = readRDS("G007702E46120N_smoothened_paths.rds")

setwd("C:\\Users\\shash\\OneDrive\\Desktop\\Research\\Glacier Revamp\\output\\restart\\04_candidate_paths\\output")
candidate_paths = readRDS("G007702E46120N_candidate_paths.rds")

setwd("C:\\Users\\shash\\OneDrive\\Desktop\\Research\\Glacier Revamp\\output\\restart\\05_clustering\\output")
min_cost_indices = readRDS("G007702E46120N_min_cost_indices.rds")

setwd("C:\\Users\\shash\\OneDrive\\Desktop\\Research\\Glacier Revamp\\output\\restart\\03_extract_IP\\output")
tt = readRDS("G007702E46120N_dates_cut.rds")
ss = readRDS("G007702E46120N_al.rds")

setwd("C:\\Users\\shash\\OneDrive\\Desktop\\Research\\Glacier Revamp\\data\\GEE")

fit = file[[1]]$fit
smooth = fit$smooth[[1]]
selected_candidate_paths = candidate_paths[min_cost_indices]
path = candidate_paths[[1]]

# Read environmental data
env_data <- read.csv("G007702E46120N_env.csv")
env_data$Date <- as.Date(with(env_data, paste(Year, Month, "01", sep="-")), "%Y-%m-%d")

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

env_data_rolling <- env_data %>%
  mutate(
    SummerTempDepartureRolling5Yr = rollapply(SummerTemperatureDeparture, 5, mean, partial = TRUE, align = "right"),
    PrecipDepartureRolling5Yr = rollapply(PrecipitationDeparture, 5, mean, partial = TRUE, align = "right")
  )

env_year <- env_data_rolling %>%
  group_by(Year) %>%
  summarise(
    YearWiseTemperature = mean(SummerTemperatureDeparture, na.rm = TRUE),
    YearWisePrecip = mean(PrecipitationDeparture, na.rm = TRUE)
  )


plot(env_year$Year, env_year$YearWiseTemperature, type = "l")

min_tt <- min(tt)
max_tt <- max(tt)


extrapolated_temp_min <- extrapolate_values(env_year$Year, env_year$YearWiseTemperature, min_tt)
extrapolated_temp_max <- extrapolate_values(env_year$Year, env_year$YearWiseTemperature, max_tt)
extrapolated_precip_min <- extrapolate_values(env_year$Year, env_year$YearWisePrecip, min_tt)
extrapolated_precip_max <- extrapolate_values(env_year$Year, env_year$YearWisePrecip, max_tt)

# Create a dataframe for the boundary years with extrapolated values
boundary_data <- data.frame(
  Year = c(min_tt, max_tt),
  YearWiseTemperature = c(extrapolated_temp_min, extrapolated_temp_max),
  YearWisePrecip = c(extrapolated_precip_min, extrapolated_precip_max)
)

# Filter env_year to include only years within the range of tt
env_year_filtered <- env_year %>%
  filter(Year > min_tt & Year < max_tt)

# Combine the filtered data with the boundary data
env_year_combined <- rbind(boundary_data, env_year_filtered)

# Sort the combined dataframe by Year
env_year <- env_year_combined[order(env_year_combined$Year), ]


padded_temp <- env_year$YearWiseTemperature
padded_precip <- env_year$YearWisePrecip


# Plot the padded data
plot(env_year$Year, padded_temp, col = "blue", type = "l", main = "Temperature Over Time", xlab = "Year", ylab = "Temperature")
legend("topright", legend = c("Extrapolated Temperature"), col = c("blue"), lwd = 2)

plot(env_year$Year, padded_precip, col = "blue", type = "l", main = "Precipitation Over Time", xlab = "Year", ylab = "Precipitation")
legend("topright", legend = c("Extrapolated Precipitation"), col = c("blue"), lwd = 2)


get_b_dash <- function(basis, tt, coef_mat){
  B_deriv <- eval.basis(tt, basis, Lfdobj = 1) %*% coef_mat
  return(B_deriv)
}

integrate_with_b <- function(basis, X, t, coef_mat){
  b = eval.basis(t, basis) %*% coef_mat
  dt <- diff(c(0, t))  # Non-uniform spacing
  int <- t(b) %*% (X * dt)
  return(int)
}




res_path = gam_to_fda(fit, tt, range(tt))
coef_mat = res_path$coefficients_matrix
bspline_basis = res_path$bspline_basis

gam_coefs = fit$coefficients

fda_basis_eval <- eval.basis(tt, bspline_basis)
fda_fitted_values <- fda_basis_eval %*% coef_mat %*% gam_coefs


plot(tt, fda_fitted_values)



dL_dt_beta <- integrate_BtL_deriv(bspline_basis, path, tt, coef_mat)
B_deriv_eval <- eval.basis(tt, bspline_basis) %*% coef_mat

dL_dt_values_path <- B_deriv_eval %*% dL_dt_beta
plot(tt,dL_dt_values_path)



fitted_temp_tt <- fda_basis_eval_path %*% coef_mat_temp %*% gam_coefs
fitted_precip_tt <- fda_basis_eval_precip %*% coef_mat_precip %*% gam_coefs



b_dash = get_b_dash(bspline_basis, tt, coef_mat)
b = fda_basis_eval %*% coef_mat

int_P =  integrate_with_b(bspline_basis,env_year$YearWisePrecip,env_year$Year,coef_mat )
int_T =  integrate_with_b(bspline_basis,env_year$YearWiseTemp,env_year$Year,coef_mat )
int_C = integrate_with_b(bspline_basis,rep(1, length(env_year$YearWiseTemp)),env_year$Year,coef_mat )

t(b) %*% L



plot(tt, path, col = "blue", type = "l", main = "Path and Its Derivative", xlab = "Time", ylab = "Path")
lines(tt, dL_dt_values_path, col = "red", lwd = 2)
legend("topright", legend = c("Path", "Derivative of Path"), col = c("blue", "red"), lwd = 2)


beta_A <- integrate_BtL(bspline_basis_path, rep(1, length(year_range)), year_range)
fitted_A_tt <- fda_basis_eval_path %*% beta_A


regression_matrix <- cbind(
  L = fda_fitted_values_path,
  P = fitted_precip_tt,
  T = fitted_temp_tt,
  A = fitted_A_tt
)


regression_result <- lm(dL_dt_values_path ~ regression_matrix - 1)


regression_coefficients <- coef(regression_result)

alpha <- regression_coefficients[2]
beta <- regression_coefficients[3]
gamma <- regression_coefficients[4]
tau <- 1 / regression_coefficients[1]

print(c(alpha = alpha, beta = beta, gamma = gamma, tau = tau))

fitted_path_values <- regression_matrix %*% regression_coefficients

plot(tt, path, col = "blue", type = "l", main = "Actual vs Fitted Path Values", xlab = "Time", ylab = "Path")
lines(tt, fitted_path_values, col = "red", lwd = 2)
legend("topright", legend = c("Actual Path", "Fitted Path"), col = c("blue", "red"), lwd = 2)
