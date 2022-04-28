# Project:   pcr_discrete_evs
# Objective: function to compute corss-entropy
# Author:    Edoardo Costantini
# Created:   2022-04-22
# Modified:  2022-04-22

crossEntropy <- function(p = matrix(), p_hat = matrix()) {
  # Internals -------------------------------------------------------------

  # p = matrix(c(1, 0, 1, 1, 1, 0, 1)) # observed label (true probability)
  # p_hat = matrix(c(.9, .1, .8, .2, .8, .24, .12)) # predicted probability value

  # Body ------------------------------------------------------------------
  ce <- -sum(diag(p %*% t(log(p_hat))))

  # Return
  return(ce)

}
