# Load necessary libraries
library(fda)
library(pracma)

# Generate some example data
set.seed(123)
n <- 100
t <- seq(0, 1, length.out = n)
L <- sin(2 * pi * t) + rnorm(n, sd = 0.2)

# Define the basis functions
nbasis <- 15
basis <- create.bspline.basis(rangeval = c(0, 1), nbasis = nbasis, norder = 4)

# Evaluate the basis functions at observed points
B <- eval.basis(t, basis)

# Evaluate the first derivative of the basis functions at observed points
dB <- eval.basis(t, basis, Lfdobj = 1)

# Calculate the B^T B matrix using the integral approximation
BtB_integral <- matrix(0, nrow = nbasis, ncol = nbasis)
for (i in 1:nbasis) {
  for (j in 1:nbasis) {
    BtB_integral[i, j] <- trapz(t, B[, i] * B[, j])
  }
}

# Calculate the B^T L vector using the integral approximation
BtL_integral <- numeric(nbasis)
for (i in 1:nbasis) {
  BtL_integral[i] <- trapz(t, B[, i] * L)
}

# Scale the terms by dt/n
dt <- 1 / n
BtB_scaled <- BtB_integral * dt
BtL_scaled <- BtL_integral * dt

# Solve the linear system (B^T B * dt/n) beta = (B^T L * dt/n) to get the coefficients beta
beta_step1 <- solve(BtB_scaled, BtL_scaled)

# Calculate the smoothed values using the (B^T L dt/n) step
L_hat_step1 <- B %*% beta_step1

# Perform the trapezoidal rule approximation for the integral of dB^T L
integrate_dB_L <- function(dB, L, t) {
  integral_dB_L <- numeric(ncol(dB))
  for (i in 1:ncol(dB)) {
    integral_dB_L[i] <- trapz(t, dB[, i] * L)
  }
  return(integral_dB_L)
}

# Perform the integration approximation
integral_dB_L <- integrate_dB_L(dB, L, t)

# Scale the integral result by -dt/n
integral_dB_L_scaled <- integral_dB_L * (-dt)

# Solve the linear system (B^T B * dt/n) beta = integral_dB_L to get the coefficients beta
beta_step2 <- solve(BtB_scaled, integral_dB_L_scaled)

# Calculate the smoothed values using the integral approximation
L_hat_step2 <- B %*% beta_step2

# Fit the data using the smoothing spline for comparison
fit_spline <- smooth.basis(t, L, basis)
L_spline_fit <- eval.fd(t, fit_spline$fd)

# Plot the original data, the smoothed data at various steps, and the spline fit
par(mfrow = c(1, 1))

# First graph: projection using the (B^T L dt/n) step
plot(t, L, col = 'blue', pch = 16, main = 'Spline Smoothing - Step 1 (B^T L dt/n)', ylab = 'L(t)', xlab = 't')
lines(t, L_hat_step1, col = 'red', lwd = 2)
lines(t, L_spline_fit, col = 'green', lwd = 2)
legend("topright", legend = c("Original Data", "Smoothed Data (Step 1)", "Spline Fit"), col = c("blue", "red", "green"), pch = 16, lwd = 2)

# Second graph: projection after making the integral approximations
plot(t, L, col = 'blue', pch = 16, main = 'Spline Smoothing - Step 2 (Integration by Parts)', ylab = 'L(t)', xlab = 't')
lines(t, L_hat_step2, col = 'red', lwd = 2)
lines(t, L_spline_fit, col = 'green', lwd = 2)
legend("topright", legend = c("Original Data", "Smoothed Data (Step 2)", "Spline Fit"), col = c("blue", "red", "green"), pch = 16, lwd = 2)

# Print intermediate values for debugging
print("B matrix:")
print(B)
print("dB matrix:")
print(dB)
print("BtB_integral matrix:")
print(BtB_integral)
print("BtL_integral vector:")
print(BtL_integral)
print("BtB_scaled matrix:")
print(BtB_scaled)
print("BtL_scaled vector:")
print(BtL_scaled)
print("Integral dB * L vector:")
print(integral_dB_L)
print("Integral dB * L scaled vector:")
print(integral_dB_L_scaled)
print("L_hat_step1:")
print(L_hat_step1)
print("L_hat_step2:")
print(L_hat_step2)
