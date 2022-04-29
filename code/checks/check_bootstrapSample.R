# Project:   pcr_discrete_evs
# Objective: Check how the sampling function used in the simulation works
# Author:    Edoardo Costantini
# Created:   2022-04-29
# Modified:  2022-04-29

# Set up -----------------------------------------------------------------------

  # Make sure we have a clean environment:
  rm(list = ls())

  # Initialize the environment:
  source("./init.R")

# Generates constants? ---------------------------------------------------------

  set.seed(1234)
  # Storing object
  store <- NULL

  # Sample many times
  for (i in 1:1e3){
    # Generate a bootsrap sample
    bs_dt <- bootstrapSample(
      dt = EVS2017,
      test = 100L
    )
    # Are there any constants for this data?
    store[i] <- sum(sapply(bs_dt$dt, function (x) length(unique(x))) == 1)
  }

  # Check how many times constants were produced
  sum(store)

# Generates constants with smaller sample size? --------------------------------

  set.seed(1234)

  # Storing object
  store <- NULL

  # Sample many times
  for (i in 1:1e3){
    # Generate a bootsrap sample
    bs_dt <- bootstrapSample(
      dt = EVS2017,
      ss = 200,
      test = 100L
    )
    # Are there any constants for this data?
    store[i] <- sum(sapply(bs_dt$dt, function (x) length(unique(x))) == 1)
  }

  # Check how many times constants were produced
  sum(store)
