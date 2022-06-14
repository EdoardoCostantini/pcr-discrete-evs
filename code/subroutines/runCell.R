# Project:  pcr_discrete_evs
# Author:   Edoardo Costantini
# Created:  2022-04-06
# Modified: 2022-05-12
# Note:     A "cell" is a cycle through the set of conditions.
#           The function in this script generates 1 data set, performs
#           imputations for every condition in the set.

runCell <- function(cond,
                    parms,
                    fs,
                    rp) {

  # Example Internals -------------------------------------------------------

  # set.seed(1234)
  # cond    = conds[1, ]
  # rp = 1
  tryCatch({
    ### START TRYCATCH EXPRESSION
    # Data Generation ---------------------------------------------------------

    bs_dt <- bootstrapSample(
      dt   = EVS2017,
      ss   = parms$ss,
      test = parms$test
    )

    # Define the type of prediction we are doing in this condition
    dv_type <- sapply(var_types, function (x){ sum(x %in% cond$dv) }) != 0
    dv_type <- names(which(dv_type))
    pred_type <- ifelse(dv_type == "ord", "lm", "glm")

    # Get rid of DVs from the variable type object
    var_types_r <- lapply(var_types, function (x){
      x[!x %in% as.character(cond$dv)]
    })

    # Drop empty factor level (if the bootstrap sample happens to have them)
    for (j in 1:ncol(bs_dt$dt)) {
      if (is.factor(bs_dt$dt[, j])) {
        bs_dt$dt[, j] <- droplevels(bs_dt$dt[, j])
      }
    }

    # All variables as factors ---------------------------------------------------
    dt_cat <- bs_dt$dt

    # Oridnal variables as numeric -----------------------------------------------
    dt_mix <- bs_dt$dt

    # Transform ordinal variables to numeric (polarity not important in this project)
    for (j in c(var_types_r$ord, var_types_r$cnts)) {
      lvls_char <- grep("[^0-9]", levels(dt_mix[, j]))
      # If there is a letter in the levels, then as.numeric is the way to
      # transform a factor into a numeric item.
      # If there are only numbers in the levels, it is better to take the actual
      # number as the value, instead of its level position
      if(length(lvls_char) > 0){
        dt_mix[, j] <- as.numeric(dt_mix[, j])
      } else {
        dt_mix[, j] <- as.numeric(as.character(dt_mix[, j]))
      }
    }

    # Items for polychoric -----------------------------------------------------
    dt_cho <- dt_mix

    # Transform binary variables to 0 and 1s
    for (j in var_types_r$bin) {
      dt_cho[, j] <- as.numeric(dt_cho[, j]) - 1
    }

    sapply(dt_cho[, var_types_r$ord], function(x) length(unique(x)))

    # Extract the index for this variable in the columns of the dataset
    dv_index <- which(colnames(bs_dt$dt) %in% cond$dv)

    # Analysis ----------------------------------------------------------------

    # continuous + categorical (PCAmix)
    pcs_mix_pcamix <- extractPCAmix(
      in_dt = dt_mix[, -dv_index],
      index_cont = c(
        var_types_r$ord,
        var_types_r$cnts
      ),
      index_disc = c(
        var_types_r$bin,
        var_types_r$cat
      ),
      keep = as.character(cond$npcs)
    )

    # continuous + categorical (dummy)
    pcs_mix_dummy <- extractPCs(
      in_dt = dt_mix[, -dv_index],
      index_cont = c(
        var_types_r$ord,
        var_types_r$cnts
      ),
      index_disc = c(
        var_types_r$bin,
        var_types_r$cat
      ),
      keep = as.character(cond$npcs),
      coding = "dummy"
    )

    # continuous + categorical (disjunctive)
    pcs_mix_disj <- extractPCs(
      in_dt = dt_mix[, -dv_index],
      index_cont = c(
        var_types_r$ord,
        var_types_r$cnts
      ),
      index_disc = c(
        var_types_r$bin,
        var_types_r$cat
      ),
      keep = as.character(cond$npcs),
      coding = "disj"
    )

    # all categorical (PCAmix)
    pcs_cat_mca <- extractPCAmix(
      in_dt = dt_cat[, -dv_index],
      index_cont = NULL,
      index_disc = c(
        var_types_r$bin,
        var_types_r$ord,
        var_types_r$cnts,
        var_types_r$cat
      ),
      keep = as.character(cond$npcs)
    )

    # all categorical (dummy)
    pcs_cat_dummy <- extractPCs(
      in_dt = dt_cat[, -dv_index],
      index_cont = NULL,
      index_disc = c(
        var_types_r$cat,
        var_types_r$bin,
        var_types_r$ord,
        var_types_r$cnts
      ),
      keep = as.character(cond$npcs),
      coding = "dummy"
    )

    # all categorical (disjunctive)
    pcs_cat_disj <- extractPCs(
      in_dt = dt_cat[, -dv_index],
      index_cont = NULL,
      index_disc = c(
        var_types_r$cat,
        var_types_r$bin,
        var_types_r$ord,
        var_types_r$cnts
      ),
      keep = as.character(cond$npcs),
      coding = "disj"
    )

    # Append results
    pcs_list <- list(
      mix_pcamix = pcs_mix_pcamix,
      mix_dummy  = pcs_mix_dummy,
      mix_disj   = pcs_mix_disj,
      cat_mca    = pcs_cat_mca,
      cat_dummy  = pcs_cat_dummy,
      cat_disj   = pcs_cat_disj
    )

    # Extract datasets of PC predictors
    dts_pcs <- lapply(pcs_list, "[[", "T")

    # Extract number of PCs extracted
    npcs <- sapply(pcs_list, "[[", "npcs")

    # Extract CPVE by the npcs
    r2 <- sapply(pcs_list, "[[", "r2")

    # Get predicitons for each PC type
    model_preds <- lapply(dts_pcs,
                          getPreds,
                          y = bs_dt$dt[, cond$dv],
                          train = bs_dt$train,
                          test = bs_dt$test,
                          type = pred_type)

    # Extract performance measures
    if(pred_type == "lm"){
      # Extract mse
      mses <- sapply(1:length(model_preds), function (i){
        MSE(y_pred = model_preds[[i]]$y_true,
            y_true = model_preds[[i]]$y_hat)
      })

      # Structure in a data.frame
      res <- data.frame(rp = rp,
                        cond_npcs = cond$npcs,
                        cond_dv = cond$dv,
                        method = names(model_preds),
                        measure = rep(c("mse",
                                        "rmse",
                                        "npcs",
                                        "r2"),
                                      each = length(model_preds)),
                        value = c(mses, sqrt(mses), npcs, r2))
    }

    if(pred_type == "glm"){

      # Extract cross-entropy
      centropy <- sapply(1:length(model_preds), function (i){
        crossEntropy(p = model_preds[[i]]$p_true,
                     p_hat = model_preds[[i]]$p_hat)
      })

      # Exrtact classification error
      cclass <- sapply(1:length(model_preds), function (i){
        correctClass(y_true = model_preds[[i]]$y_true,
                     y_hat = model_preds[[i]]$y_hat)
      })

      # Structure in a data.frame
      res <- data.frame(rp = rp,
                        cond_npcs = cond$npcs,
                        cond_dv = cond$dv,
                        method = names(model_preds),
                        measure = rep(c("centropy",
                                        "cclass",
                                        "npcs",
                                        "r2"),
                                      each = length(model_preds)),
                        value = c(centropy, cclass, npcs, r2))
    }

    # Store Output ------------------------------------------------------------

    # Return it
    saveRDS(res,
            file = paste0(
              fs$outDir,
              "rep", rp,
              "_cond", cond$tag,
              ".rds"
            )
    )
    ### END TRYCATCH EXPRESSION
  }, error = function(e){
    err <- paste0("Original Error: ", e)
    err_res <- data.frame(rp = rp,
                          cond_npcs = cond$npcs,
                          cond_dv = cond$dv,
                          Error = err)
    saveRDS(err_res,
            file = paste0(fs$outDir,
                          "rep_", rp, "_", cond$tag,
                          "_ERROR",
                          ".rds")
    )
  }
  )
}