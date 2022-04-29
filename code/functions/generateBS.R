# Project:   pcr_discrete_evs
# Objective: Funciton to generate a Bootstrap Sample (BS)
# Author:    Edoardo Costantini
# Created:   2022-04-20
# Modified:  2022-04-29

bootstrapSample <- function(dt, test = .8) {
  # Internals -------------------------------------------------------------

  # dt = readRDS("../input/ZA7500_processed.rds")
  # test = .1   # proportion of data to be used as training
  # test = 100L # number of data units to be used as training

  # Body ------------------------------------------------------------------

  # Sample rows
  bs_index <- sample(1:nrow(dt), size = nrow(dt), replace = TRUE)

  # Keep rows of data based on bs_index
  bs_dt <- dt[bs_index, ]

  # Define an indexing for partitioning
  ind   <- sample(1 : nrow(dt))

  # Define the number of test cases
  if(is.integer(test)){
    ind_test <- 1:test
  } else {
    ind_test <- 1:floor(test * nrow(dt))
  }

  # Partition in training and testing
  train <- ind[-ind_test]
  test  <- ind[ind_test]

  # Output
  return(list(dt = bs_dt,
              train = train,
              test = test))

}
