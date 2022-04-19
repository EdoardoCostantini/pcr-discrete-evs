# Project:   ordinality
# Objective: Extract MSE from a single dataset
# Author:    Edoardo Costantini
# Created:   2021-10-19
# Modified:  2021-10-19

extractMSE <- function(y = vector(),
                       x = matrix(),
                       train = vector("integer"),
                       test = vector("integer")){
  ## Description
  # Given a dependent variable and a list of PCs, this regresses y on
  # the PCs and extracts then it extracts the test MSE

  ## Example input
  # x = MASS::mvrnorm(1e2, rep(0, 3), diag(3))
  # y = x %+% rep(1, 3) + rnorm(nrow(x))
  # ind   = sample(1 : nrow(x))
  # train = ind[1 : (.9*nrow(x))]
  # test  = ind[(.9*nrow(x)+1) : nrow(x)]

  ## Transform into data frame
  dt_df <- data.frame(y = y, x = x)

  ## Check column names have no spaces
  colnames(dt_df) <- sapply(colnames(dt_df), str_replace, " ", "")

  ## Estimate model
  vars <- colnames(dt_df)
  lm_out <- lm(formula = paste0(vars[1],
                                " ~ ",
                                paste0(vars[-1], collapse = " + ")),
               data = dt_df[train, ])

  ## Generate test-set predictions (i.e., y-hats):
  preds <- predict(lm_out, newdata = dt_df[test, ])

  ## Generate test-set MSEs:
  mse <- MSE(y_pred = preds, y_true = dt_df[test, 1])

  # Output
  return(mse)
}