# Project:   pcr_discrete_evs
# Objective: function to produce predictions
# Author:    Edoardo Costantini
# Created:   2022-05-02
# Modified:  2022-05-02

getPreds <- function(y = vector(),
                     x = matrix(),
                     train = vector("integer"),
                     test = vector("integer"),
                     type = c("lm", "glm")[1]) {
  # Internals -------------------------------------------------------------

  ## Example normal data
  # x = MASS::mvrnorm(1e2, rep(0, 3), diag(3))
  # y = drop(x %+% rep(1, 3) + rnorm(nrow(x)))
  # type = "lm"
  ## Example with binary data
  # x = mtcars[, -9]
  # y = mtcars[, 9]
  # type = "glm"
  ## Example with multicategorical variable
  # x = iris[, -5]
  # y = iris[, 5]
  # type = "glm"
  ## Example with study data
  # x = dts_pcs[[1]]
  # y = bs_dt$dt[, parms$DVs$bin]
  # ind   = sample(1 : nrow(x))
  # train = ind[1 : (.9*nrow(x))]
  # test  = ind[(.9*nrow(x)+1) : nrow(x)]

  # Body -----------------------------------------------------------------------

  # 1. prepare data ------------------------------------------------------------

  # Transform into data frame
  dt_df <- data.frame(y = y, x = x)

  # Check column names have no spaces
  colnames(dt_df) <- sapply(colnames(dt_df), str_replace, " ", "")

  # Get banes fi tge varuavkes
  vars <- colnames(dt_df)

  # 2. Estimate model ----------------------------------------------------------

  if(type == "lm"){
    lm_out <- lm(formula = paste0(vars[1],
                                  " ~ ",
                                  paste0(vars[-1], collapse = " + ")),
                 data = dt_df[train, ])
  }
  if(type == "glm"){
    glm_out <- multinom(paste0(vars[1],
                               " ~ ",
                               paste0(vars[-1], collapse = " + ")),
                        data = dt_df[train, ])
  }

  # 3. Generate predictions ----------------------------------------------------

  # Store true values for the predicted values
  y_true_test <- dt_df[test, "y"]

  if(type == "lm"){
    # Get predictions
    preds <- predict(lm_out, newdata = dt_df[test, ])

    # Store empty objects for results irrelevant in the lm case
    preds_probs <- y_true_test_mat <- NULL
  }

  if(type == "glm"){
    # Get predicted probabilities
    preds_probs <- predict(glm_out,
                           newdata = dt_df[test, , drop = FALSE],
                           type = "probs")

    # Get predicted class memberships
    preds <- predict(glm_out,
                     newdata = dt_df[test, , drop = FALSE],
                     type = "class")

    # True labels matrix
    if(length(unique(y)) == 2){
      # Store outcome as 0 and 1s
      y_true_test_mat <- model.matrix( ~ ., dt_df[test, 1, drop = FALSE])[, -1, drop = FALSE]

      # Make sure probs are stored as matrix for consistenty with multinom obj
      preds_probs <- matrix(preds_probs, ncol = 1)
    }
    # Multinomial
    if(length(unique(y)) > 2){
      # Store outcome as 0 and 1s
      y_true_test_mat <- tab.disjonctif(dt_df[test, 1, drop = FALSE])
    }
  }

  # 4. Define returned values --------------------------------------------------

  return(list(y_true = y_true_test,
              y_true_mat = y_true_test_mat,
              y_hat  = preds,
              p_hat  = preds_probs))

}
