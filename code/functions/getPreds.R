# Project:   pcr_discrete_evs
# Objective: function to produce predictions
# Author:    Edoardo Costantini
# Created:   2022-05-02
# Modified:  2022-05-12

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
  # y = bs_dt$dt[, cond$dv]
  # ind   = sample(1 : nrow(x))
  # train = ind[1 : (.9*nrow(x))]
  # test  = ind[(.9*nrow(x)+1) : nrow(x)]

  # Body -----------------------------------------------------------------------

  # 1. prepare data ------------------------------------------------------------

  if(type == "lm"){
    y <- as.numeric(y)
  }

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
                        maxit = 1e3,
                        data = dt_df[train, ])
  }

  # 3. Generate predictions ----------------------------------------------------

  if(type == "lm"){
    # Get predictions
    preds <- predict(lm_out, newdata = dt_df[test, ])

    # Store empty objects for results irrelevant in the lm case
    preds_probs <- p_true <- NULL
  }

  if(type == "glm"){
    # Get predicted probabilities
    preds_probs <- predict(glm_out,
                           newdata = dt_df[test, , drop = FALSE],
                           type = "probs")

    if(nlevels(y) == 2){
      # Make sure probs are stored as matrix for consistenty with multinom obj
      preds_probs <- cbind(cat_0 = 1 - preds_probs,
                           cat_1 = preds_probs)
    }

    # Get predicted class memberships
    preds <- predict(glm_out,
                     newdata = dt_df[test, , drop = FALSE],
                     type = "class")

    # Store true values
    p_true <- tab.disjonctif(dt_df[test, 1, drop = FALSE])
  }

  # 4. Define returned values --------------------------------------------------

  # Return a list
  return(list(y_true = dt_df[test, "y"],
              y_hat  = preds,
              p_true = p_true,
              p_hat  = preds_probs))

}
