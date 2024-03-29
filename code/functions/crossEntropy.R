# Project:   pcr_discrete_evs
# Objective: function to compute corss-entropy
# Author:    Edoardo Costantini
# Created:   2022-04-22
# Modified:  2022-05-16

crossEntropy <- function(p = matrix(), p_hat = matrix()) {
  # Internals -------------------------------------------------------------

  # p = matrix(c(1, 0, 1, 1, 1, 0, 1)) # observed label (true probability)
  # p_hat = matrix(c(.9, .1, .8, .2, .8, .24, .12)) # predicted probability value

  # Body ------------------------------------------------------------------

  # Correction for perfect 0 probability predictions
  eps <- 1e-15
  p_hat <- t(
    apply(p_hat, 1, function (r){
      pmax(r, eps)
    })
  )

  # Compute CE
  ce <- -sum(diag(p %*% t(log(p_hat))))

  # Return
  return(ce)

}
