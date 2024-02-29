setwd("C:\\Users\\shash\\OneDrive\\Desktop\\Research\\Glacier Revamp")
outs = readRDS("output/debug/04_candidate_paths/output/G078184E41824N_outs.rds")
L = outs$out1$pred
tt = readRDS("output/debug/03_extract_IP/output/G078184E41824N_dates_cut.rds")




env_data <- read.csv(csv_filename)
env_data$Date <- as.Date(with(env_data, paste(Year, Month, "01", sep="-")), "%Y-%m-%d")
# lines(env_data$Date, env_data$Skin_Temperature)
# lines(env_data$Date, env_data$Skin_Temperature)

plot(tt,L)

env_data$RollingMean_Temp <- rollapply(env_data$Skin_Temperature, width = 12, FUN = mean, align = "center", partial = TRUE)
lines(env_data$Date,env_data$RollingMean_Temp)

library(zoo)
library(magick)


#############


library(mgcv)

# Generating data
set.seed(123)
n <- 100
x <- seq(0, 10, length.out = n)
y <- sin(x) + rnorm(n, sd = 0.2) # Sine wave with noise

# Fitting spline of degree 6, fx = TRUE removes the penalty term
fit <- gam(y ~ s(x, bs = "cr", k = 6, fx = TRUE), data = data.frame(x, y))
summary(fit)

# denser x to plot
x_dense <- seq(min(x), max(x), length.out = 1000)

# predicting y with denser x
y_pred_dense <- predict(fit, newdata = data.frame(x = x_dense))

plot(x, y, main = "Original Data vs Fitted Spline", xlab = "X", ylab = "Y", xlim = range(x_dense), ylim = range(c(y, y_pred_dense)))
lines(x_dense, y_pred_dense, col = "red", lwd = 2) # This should show a smoother spline
legend("topright", legend = c("Original Data", "Fitted Spline"), col = c("black", "red"), lty = 1)


mat <- predict.gam(fit, type = "lpmatrix")

mat



plot(x, mat[, "s(x).1"], type = "l", main = "1st basis")
plot(x, mat[, "s(x).2"], type = "l", main = "2nd basis")
plot(x, mat[, "s(x).3"], type = "l", main = "3rd basis")
plot(x, mat[, "s(x).4"], type = "l", main = "4th basis")
plot(x, mat[, "s(x).5"], type = "l", main = "5th basis")
plot(x, mat[, "s(x).6"], type = "l", main = "6th basis")



# predicting derivative using 
y_deriv_pred <- predict(fit, newdata = data.frame(x = x_dense), type = "response", deriv = 1)

# plots
plot(x_dense, cos(x_dense), type = 'l', main = "Analytical Derivative vs. Spline Approximation", xlab = "X", ylab = "Y", col = "blue", ylim = range(c(cos(x_dense), y_deriv_pred)))
lines(x_dense, y_deriv_pred, col = "red", lwd = 2)
lines(x_dense, sin(x_dense), type = 'l', main = "Analytical Derivative vs. Spline Approximation", xlab = "X", ylab = "Y", col = "blue", ylim = range(c(cos(x_dense), y_deriv_pred)))

legend("topright", legend = c("Analytical Derivative (cos(x))", "Spline Approximation"), col = c("blue", "red"), lty = 1)


library(mgcv)
## simulate data and fit model...
dat <- gamSim(1,n=300)
b<-gam(y~s(x0)+s(x1)+s(x2)+s(x3),data=dat)
plot(b,pages=1)
## now evaluate derivatives of smooths with associated standard
## errors, by finite differencing...
x.mesh <- seq(0,1,length=200) ## where to evaluate derivatives
newd <- data.frame(x0 = x.mesh,x1 = x.mesh, x2=x.mesh,x3=x.mesh)
X0 <- predict(b,newd,type="lpmatrix")
eps <- 1e-7 ## finite difference interval
x.mesh <- x.mesh + eps ## shift the evaluation mesh
newd <- data.frame(x0 = x.mesh,x1 = x.mesh, x2=x.mesh,x3=x.mesh)
X1 <- predict(b,newd,type="lpmatrix")
Xp <- (X1-X0)/eps ## maps coefficients to (fd approx.) derivatives
colnames(Xp)      ## can check which cols relate to which smooth
par(mfrow=c(2,2))
for (i in 1:4) {  ## plot derivatives and corresponding CIs
  Xi <- Xp*0
  Xi[,(i-1)*9+1:9+1] <- Xp[,(i-1)*9+1:9+1] ## Xi%*%coef(b) = smooth     deriv i
  df <- Xi%*%coef(b)              ## ith smooth derivative
  df.sd <- rowSums(Xi%*%b$Vp*Xi)^.5 ## cheap diag(Xi%*%b$Vp%*%t(Xi))^.5
  plot(x.mesh,df,type="l",ylim=range(c(df+2*df.sd,df-2*df.sd)))
  lines(x.mesh,df+2*df.sd,lty=2);lines(x.mesh,df-2*df.sd,lty=2)
}

library(mgcv)
library(zoo)
##########
# Trying out numerical differentiation
set.seed(123)
n <- 100
x <- seq(0, 12, length.out = n)
y <- sin(x) + rnorm(n, sd = 0.002) # Sine wave with noise

# Fitting spline of degree 6, fx = TRUE removes the penalty term
fit <- gam(y ~ s(x, bs = "cr", k = 10, fx = TRUE), data = data.frame(x, y))
summary(fit)
plot(fit)
points(x,y)
eps <- 1e-7 ## finite difference interval
# denser x to plot
# x_dense <- seq(min(x), max(x), length.out = 1000)
# x_dense_plus_eps = x_dense + eps


# x_df0 <- data.frame(x = x_dense)
# x_df1 <- data.frame(x = x_dense_plus_eps)
x_df <- data.frame(x = x)
x_df_eps <- data.frame(x = x+eps)

X0 <- predict(fit,x_df,type="lpmatrix")
X1 <- predict(fit,x_df_eps,type="lpmatrix")

y0 <- X0%*%coef(fit)
# X0 <- x%*%coef(fit)
# X1 <- x_df1%*%coef(fit)


Xp <- (X1-X0)/eps ## maps coefficients to (fd approx.) derivatives

df <- Xp%*%coef(fit)
df.sd <- rowSums(Xp%*%fit$Vp*Xp)^.5 ## cheap diag(Xi%*%b$Vp%*%t(Xi))^.5
plot(x,  df, type="l", ylim=range(c(df+2*df.sd,df-2*df.sd)))
lines(x, df+2*df.sd, lty=2);
lines(x, df-2*df.sd, lty=2)
lines(x, cos(x), col = "red")



##########
#The actual regression
library(zoo)
library(dplyr)
#Setup
setwd("C:\\Users\\shash\\OneDrive\\Desktop\\Research\\Glacier Revamp")
outs = readRDS("output/debug/04_candidate_paths/output/G078184E41824N_outs.rds")
out1 = outs$out1
L = out1$pred
fit = out1$err$fit_1
tt = readRDS("output/debug/03_extract_IP/output/G078184E41824N_dates_cut.rds")
csv_filename = "data/GEE/G078184E41824N_env.csv"

# L
L = outs$out1$pred
plot(tt,L, main="L vs time")

# dL/dt
eps <- 1e-7 ## finite difference interval
x_dense <- seq(min(tt), max(tt), length.out = 1000)
x_dense_plus_eps = x_dense + eps

x_df0 <- data.frame(tt = x_dense)
x_df1 <- data.frame(tt = x_dense_plus_eps)

X0 <- predict(fit,x_df0,type="lpmatrix")
X1 <- predict(fit,x_df1,type="lpmatrix")

Xp <- (X1-X0)/eps ## maps coefficients to (fd approx.) derivatives

df <- Xp%*%coef(fit)
df.sd <- rowSums(Xp%*%fit$Vp*Xp)^.5 ## cheap diag(Xi%*%b$Vp%*%t(Xi))^.5
plot(x_dense,  df, type="l", ylim=range(c(df+2*df.sd,df-2*df.sd)), main="dL/dt vs time")
lines(x_dense, df+2*df.sd, lty=2);
lines(x_dense, df-2*df.sd, lty=2)


# T and P
env_data <- read.csv(csv_filename)
env_data$Date <- as.Date(with(env_data, paste(Year, Month, "01", sep="-")), "%Y-%m-%d")
# lines(env_data$Date, env_data$Skin_Temperature)
# lines(env_data$Date, env_data$Skin_Temperature)

annual_data <- env_data %>%
  group_by(Year) %>%
  summarise(AverageTemperature = mean(Skin_Temperature, na.rm = TRUE), AveragePrecipitation = mean(Total_Precipitation, na.rm = TRUE))

summer_data <- env_data %>%
  filter(Month %in% c(5, 6, 7, 8)) %>%
  group_by(Year) %>%
  summarise(
    SummerAvgTemperature = mean(Temperature_2m, na.rm = TRUE)
  )

annual_data_rolling <- annual_data %>%
  mutate(
    TemperatureRolling5Yr = rollapply(AverageTemperature, 5, mean, partial = TRUE, align = "right"),
    PrecipitationRolling5Yr = rollapply(AveragePrecipitation, 5, mean, partial = TRUE, align = "right")
  )

summer_data_rolling <- summer_data %>%
  mutate(
    SummerTemperatureRolling5Yr = rollapply(SummerAvgTemperature, 5, mean, partial = TRUE, align = "right")
  )

plot(annual_data_rolling$Year, annual_data_rolling$TemperatureRolling5Yr, 
     type = "l",
     xlab = "Year", 
     ylab = "5-Year Rolling Average",
     col = "blue",
     main = "5-Year Rolling Averages of Temperature")

plot(annual_data_rolling$Year, summer_data_rolling$SummerTemperatureRolling5Yr, type = "l", col = "red", main="5-Year Rolling averages Summer temperatue")

# Lining the Precipitation
plot(annual_data_rolling$Year, annual_data_rolling$PrecipitationRolling5Yr, type = "l", col = "darkgreen", main="5 year rolling precipitation")

# I have L and dL/dt, but need to convert them into yearwise values as well
years_range <- floor(min(tt)):floor(max(tt)) + 0.5
L_interp <- approx(x = tt, y = L, xout = years_range)$y
df_interp <- approx(x = x_dense, y = df, xout = years_range)$y

data.frame <- data.frame(dL = df_interp, L = L_interp, temp =  annual_data[[2]], prec =annual_data[[3]])

pairs(data.frame)


calculate_derivative_using_finite_difference_spline <- function(x,y, n_basis_functions = 10, eps = 1e-7, x_dense_length = 1000){
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



