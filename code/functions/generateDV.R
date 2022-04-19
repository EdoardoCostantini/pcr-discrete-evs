# Project:   pcr_discrete
# Objective: Function to generate the dependent variable for the PCR model
# Author:    Edoardo Costantini
# Created:   2022-01-14
# Modified:  2022-01-14

generateDV <- function (X = matrix(), R2 = 0.90, beta = 1){
# Internals -------------------------------------------------------------

  # X     = matrix(rnorm(1e3 * 4), nrow = 1e3, ncol = 4)
  # R2    = .9
  # beta  = 1

# Body ------------------------------------------------------------------
  # Generate a dependent variable on true line
  y_true <- as.vector( X %*% rep(beta, ncol(X)) )

  # Generate random error
  error <- rnorm(nrow(X))

  # Make the error orthogonal to the Xs
  e_ortho <- orthmat(cbind(X, error))[, "error"]

  # sum of squares
  ssqy <- sum(X^2)
  ssqe <- sum(e_ortho^2)

  # Rescale noise to desired level
  e_scale <- sqrt(ssqy * (1 - R2) / (R2 * ssqe))

  # Generate samples for y
  y <- y_true + e_scale * e_ortho

  # What to return
  return(y)
}