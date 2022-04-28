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

  # x = iris[, -5]
  # y = iris[, 5]
  # x = dts_pcs[[1]] mtcars[, -9]
  # y = mtcars[, 9]
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

  ## Logitic
  if(length(unique(y)) == 2){

    # Fit model
    glm_out <- glm(formula = paste0(vars[1],
                                    " ~ ",
                                    paste0(vars[-1], collapse = " + ")),
                   family = binomial(link='logit'),
                   data = dt_df[train, ])

    # Generate test-set predictions (i.e., y-hats):
    preds <- predict(glm_out, newdata = dt_df[test, ], type = "response")

    # Structure it as a matrix for consistency with multinom results
    preds <- matrix(preds, ncol = 1)

    # True labels matrix
    p <- model.matrix( ~ ., dt_df[test, 1, drop = FALSE])[, -1, drop = FALSE]

  }

  ## Multinomial
  if(length(unique(y)) > 2){

    # Fit model
    glm_out <- multinom(paste0(vars[1],
                               " ~ ",
                               paste0(vars[-1], collapse = " + ")),
                        data = dt_df[train, ]) # saturated model

    # Generate test-set predictions (i.e., y-hats):
    preds <- predict(glm_out,
                     newdata = dt_df[test, , drop = FALSE],
                     type = "probs")

    # True labels matrix
    p = tab.disjonctif(dt_df[test, 1, drop = FALSE])

  }

  # Compute test-set cross-entropy:
  centropy <- crossEntropy(p = p, p_hat = preds)

  ## Return
  return(centropy)

}
