# Project:   pcr_discrete_evs
# Objective: Function to compute the classificaiton error
# Author:    Edoardo Costantini
# Created:   2022-04-29
# Modified:  2022-04-29

correctClass <- function(y_true, preds) {
  # Internals -------------------------------------------------------------

  # preds = factor(c(1, 1, 0, 1, 2, 1, 3, 0, 1)) # factor of predictions
  # y_true = factor(c(0, 1, 0, 1, 1, 1, 3, 2, 1)) # factor of observed values

  # Body ------------------------------------------------------------------

  # Compute proportion of correctly classified cases
  prop_cclass <- mean(preds == y_true)

  # Return it
  return(prop_cclass)

}
