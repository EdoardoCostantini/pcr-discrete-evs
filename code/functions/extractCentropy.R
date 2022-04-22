# Project:   pcr_discrete_evs
# Objective: Function to fit glm and extract test data cross-entropy
# Author:    Edoardo Costantini
# Created:   2022-04-22
# Modified:  2022-04-22

extractCentropy <- function(y = vector(),
                            x = matrix(),
                            train = vector("integer"),
                            test = vector("integer")) {
  # Internals -------------------------------------------------------------

  # x = ToothGrowth[, -2]
  # y = as.numeric(ToothGrowth[, 2])-1
  # ind   = sample(1 : nrow(x))
  # train = ind[1 : (.9*nrow(x))]
  # test  = ind[(.9*nrow(x)+1) : nrow(x)]

  # Body ------------------------------------------------------------------

  ## Transform into data frame
  dt_df <- data.frame(y = y, x = x)

  ## Check column names have no spaces
  colnames(dt_df) <- sapply(colnames(dt_df), str_replace, " ", "")

  ## Estimate model
  vars <- colnames(dt_df)
  glm_out <- glm(formula = paste0(vars[1],
                                  " ~ ",
                                  paste0(vars[-1], collapse = " + ")),
                 family = binomial(link='logit'),
                 data = dt_df[train, ])

  ## Generate test-set predictions (i.e., y-hats):
  preds <- predict(glm_out, newdata = dt_df[test, ], type = "response")

  ## Generate test-set MSEs:
  centropy <- crossEntropy(p = dt_df[test, 1], p_hat = preds)

  ## Return
  return(centropy)

}
