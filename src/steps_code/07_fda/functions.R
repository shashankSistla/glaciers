gam_to_fda <- function(gam_model, x, rangeval, norder = 4) {
  knots <- gam_model$smooth[[1]]$xp
  
  bspline_basis <- create.bspline.basis(rangeval = rangeval, breaks = knots, norder = norder)
  
  basis_gam <- predict(gam_model, type = "lpmatrix")
  
  fda_basis_eval <- eval.basis(x, bspline_basis)
  
  coefficients_matrix <- matrix(nrow = ncol(fda_basis_eval), ncol = ncol(basis_gam))
  
  for (i in 1:ncol(basis_gam)) {
    fit <- lm(basis_gam[, i] ~ fda_basis_eval - 1)  # Fit without intercept
    coefficients_matrix[, i] <- coef(fit)
  }
  
  return(list(coefficients_matrix = coefficients_matrix, bspline_basis = bspline_basis))
}

gam_to_fda_with_se <- function(gam_model, x, rangeval, norder = 4) {
  knots <- gam_model$smooth[[1]]$xp
  bspline_basis <- create.bspline.basis(rangeval = rangeval, breaks = knots, norder = norder)
  basis_gam <- predict(gam_model, type = "lpmatrix")
  fda_basis_eval <- eval.basis(x, bspline_basis)
  coefficients_matrix <- matrix(nrow = ncol(fda_basis_eval), ncol = ncol(basis_gam))
  for (i in 1:ncol(basis_gam)) {
    fit <- lm(basis_gam[, i] ~ fda_basis_eval - 1)  # Fit without intercept
    coefficients_matrix[, i] <- coef(fit)
  }
  return(list(coefficients_matrix = coefficients_matrix, bspline_basis = bspline_basis))
}