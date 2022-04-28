# Project:   pcr_discrete_evs
# Objective: function to compute corss-entropy
# Author:    Edoardo Costantini
# Created:   2022-04-22
# Modified:  2022-04-22

crossEntropy <- function(p, p_hat) {
  # Internals -------------------------------------------------------------

  # p = c(1, 0, 1, 1, 1, 0, 1) # observed label (true probability)
  # p_hat = c(.9, .1, .8, .2, .8, .24, .12) # predicted probability value

  # Body ------------------------------------------------------------------

  # Output size
  os <- length(unique(y))

  # Starting centropy value
  x <- 0

  # For every observation, compute
  for (i in 1:length(p)){
    # Sum the
    x <- x + (p[i] * log(p_hat[i]))
  }

  return(-x)

}