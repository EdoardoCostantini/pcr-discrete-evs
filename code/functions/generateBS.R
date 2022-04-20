# Project:   pcr_discrete_evs
# Objective: Funciton to generate a Bootstrap Sample (BS)
# Author:    Edoardo Costantini
# Created:   2022-04-20
# Modified:  2022-04-20

bootstrapSample <- function(dt, train_p = .8) {
  # Internals -------------------------------------------------------------

  # dt = readRDS("../input/ZA7500_processed.rds")
  # train_p = .8 # proportion of data to be used as training

  # Body ------------------------------------------------------------------

  # Sample rows
  bs_index <- sample(1:nrow(dt), size = nrow(dt), replace = TRUE)

  # Keep rows of data based on bs_index
  bs_dt <- dt[bs_index, ]

  # Partition in training and testing
  ind   <- sample(1 : nrow(dt))
  train <- ind[1 : (train_p * nrow(dt))]
  test  <- ind[(train_p * nrow(dt)+1) : nrow(dt)]

  # Output
  return(list(dt = bs_dt,
              train = train,
              test = test))

}
