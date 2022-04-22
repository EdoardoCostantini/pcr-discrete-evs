# Project:   pcr_discrete_evs
# Objective: function to drop variables by name
# Author:    Edoardo Costantini
# Created:   2022-04-07
# Modified:  2022-04-07

dropVars <- function(data, variables) {
  # Internals -------------------------------------------------------------

  # data = ... # any data set with named columns
  # variables = ... # character vectors with names of variables to drop

  # Body ------------------------------------------------------------------

  keep <- !colnames(data) %in% variables
  data[, keep]

}
