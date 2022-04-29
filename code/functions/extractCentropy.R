# Project:   pcr_discrete_evs
# Objective: Function to fit glm and extract test data cross-entropy
# Author:    Edoardo Costantini
# Created:   2022-04-22
# Modified:  2022-04-29

extractCentropy <- function(y = vector(),
                            x = matrix(),
                            train = vector("integer"),
                            test = vector("integer")) {
  # Internals -------------------------------------------------------------

  ## Example with binary data
  # x = mtcars[, -9]
  # y = mtcars[, 9]
  ## Example with multicategorical variable
  # x = iris[, -5]
  # y = iris[, 5]
  ## Example with study data
  # x = dts_pcs[[1]]
  # y = bs_dt$dt[, parms$DVs$bin]
  # ind   = sample(1 : nrow(x))
  # train = ind[1 : (.9*nrow(x))]
  # test  = ind[(.9*nrow(x)+1) : nrow(x)]

  # Body ------------------------------------------------------------------

  # Transform into data frame
  dt_df <- data.frame(y = y, x = x)

  # Check column names have no spaces
  colnames(dt_df) <- sapply(colnames(dt_df), str_replace, " ", "")

  # Estimate model
  vars <- colnames(dt_df)

  # Fit model (if binary dv it does logit)
  glm_out <- multinom(paste0(vars[1],
                             " ~ ",
                             paste0(vars[-1], collapse = " + ")),
                      data = dt_df[train, ]) # saturated model

  # Generate test-set probs predictions (i.e., y-hats):
  preds_probs <- predict(glm_out,
                         newdata = dt_df[test, , drop = FALSE],
                         type = "probs")

  # Generate test-set class predictions:
  preds_class <- predict(glm_out,
                         newdata = dt_df[test, , drop = FALSE],
                         type = "class")

  # True labels matrix
  # Logit
  if(length(unique(y)) == 2){
    # Store outcome as 0 and 1s
    p <- model.matrix( ~ ., dt_df[test, 1, drop = FALSE])[, -1, drop = FALSE]

    # Make sure probs are stored as matrix for consistenty with multinom obj
    preds_probs <- matrix(preds_probs, ncol = 1)
  }
  # Multinomial
  if(length(unique(y)) > 2){
    # Store outcome as 0 and 1s
    p <- tab.disjonctif(dt_df[test, 1, drop = FALSE])
  }

  # Compute test-set cross-entropy:
  centropy <- crossEntropy(p = p, p_hat = preds)

  ## Return
  return(centropy)

}
