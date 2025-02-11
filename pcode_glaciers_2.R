library(pracma)
library(microbenchmark)
library(TMB)
# Define the first multivariable function for Jacobian demonstration
matrix_function_multivar1 <- function(x) {
  X <- matrix(x, nrow = 10, ncol = 10)
  c(sum(X^2), sum(3 * X), 5)
}

# Define the second multivariable function for Jacobian demonstration
matrix_function_multivar2 <- function(x) {
  X <- matrix(x, nrow = 10, ncol = 10)
  c(prod(X + 1), sum(exp(X)))
}
x_vec <- 1:100
time_jacobian1 <- microbenchmark(
  jacobian_result = jacobian(matrix_function_multivar1, x_vec),
  times = 100
)

time_jacobian2 <- microbenchmark(
  jacobian_result = jacobian(matrix_function_multivar2, x_vec),
  times = 100
)

cat("Timing for pracma's jacobian function (function 1):\n")
print(summary(time_jacobian1))

cat("\nTiming for pracma's jacobian function (function 2):\n")
print(summary(time_jacobian2))
writeLines('
#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() () {
  DATA_VECTOR(x);
  PARAMETER_VECTOR(y);
  Type nll = 0.0;
  vector<Type> z1 = x * y;
  vector<Type> z2 = exp(x + y);
  ADREPORT(z1);
  ADREPORT(z2);
  return nll;
}
', "auto_diff_function.cpp")

# Compile the function for automatic differentiation using TMB
compile("auto_diff_function.cpp")
dyn.load(dynlib("auto_diff_function"))

# Define the objective function in R
obj <- MakeADFun(data = list(x = rep(1, 100)), parameters = list(y = rep(0.1, 100)))

# Time the autodiffr method
time_autodiff <- microbenchmark(
  autodiff_result = {
    obj$fn()
    obj$gr()
  },
  times = 100
)

cat("\nTiming for TMB's automatic differentiation method:\n")
print(summary(time_autodiff))
