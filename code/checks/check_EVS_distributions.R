# Project:   pcr_discrete_evs
# Objective: TODO
# Author:    Edoardo Costantini
# Created:   2022-05-12
# Modified:  2022-05-12

# Load and prepare inputs

  # Load pre-processed EVS data
  prova <- EVS2017   <- readRDS("../input/ZA7500_processed.rds")

  # Load variables type description
  var_types <- readRDS("../input/var_types.rds")

# Transform ordinal variables to numeric (polarity not important in this project)
for (j in c(var_types_r$ord, var_types_r$cnts)) {
  lvls_char <- grep("[^0-9]", levels(EVS2017[, j]))
  # If there is a letter in the levels, then as.numeric is the way to
  # transform a factor into a numeric item.
  # If there are only numbers in the levels, it is better to take the actual
  # number as the value, instead of its level position
  if(length(lvls_char) > 0){
    EVS2017[, j] <- as.numeric(EVS2017[, j])
  } else {
    EVS2017[, j] <- as.numeric(as.character(EVS2017[, j]))
  }
}

# Explore distributions of all variables

  par(mfrow = c(3, 3))
  lapply(var_types$ord, function (x){
    if(length(unique(EVS2017[, x])) >= 7){
      barplot(table(EVS2017[, x]),
              main = x)
    }
  })
