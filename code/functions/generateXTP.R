# Project:   pcr_discrete
# Objective: Generate data based on a given PC structure (version giving T)
# Author:    Edoardo Costantini
# Created:   2022-01-05
# Modified:  2022-01-21
# Source:    https://github.com/trbKnl/SCaDS/blob/master/Simulation_study_4.1/generateData.R
#            https://github.com/soogs/SCD-CovR/blob/master/functions/spcovrdata.R

generateXTP <- function(I, J, VAFr = c(.5, .4, .2), VAFsum = 100, CPVE = 0.9){
# Internals -------------------------------------------------------------

  # I    = 100 # sample size
  # J    = 9 # number of variables
  # VAFr = c(.5, .3, .2) # relative variance of each components
  # VAFsum = 100 # total variance of the components
  # CPVE = 0.9 # proportion of explained variance by the R components

# Body ------------------------------------------------------------------
  # Number of components
  R <- length(VAFr)

  # Random sample U
  U <- matrix(
    data = rnorm(I * R),
    nrow = I,
    ncol = R
  )
  U <- scale(U, center = TRUE, scale = FALSE)
  U <- orthmat(U, verbose = FALSE)
  U <- normmat(U)

  # Random sample P
  V <- matrix(
    data = runif(J * R),
    nrow = J,
    ncol = R
  )
  V <- orthmat(V, verbose = FALSE)
  P <- normmat(V)

  # Define D
  D <- diag(c(VAFsum * VAFr))

  # Create X
  Xtrue  <- U %*% D %*% t(P)

  # sample from normal distribution (Ex = Error of X)
  Ex <- MASS::mvrnorm(n = I, mu = rep(0, J), Sigma = diag(J))

  # centering and scaling the Ex matrix
  Ex <- scale(Ex, center = TRUE, scale = FALSE)

  # sum of squares
  ssqXtrue <- sum(Xtrue^2)
  ssqEx <- sum(Ex^2)

  # Compute noise scaling constant
  Escale <- sqrt(ssqXtrue * (1 - CPVE) / (CPVE * ssqEx))

  # Add scaled noise
  X  <- Xtrue + Escale * Ex

  # Scale data for estimation
  X <- scale(X)

  # Define outputs
  return(list(X = data.frame(X),
              T = U %*% D,
              P = P,
              U = U,
              D = D)
  )
}
