# Project:  pcr_discrete_evs
# Author:   Edoardo Costantini
# Created:  2022-04-06
# Modified: 2022-04-06
# Note:     A "cell" is a cycle through the set of conditions.
#           The function in this script generates 1 data set, performs
#           imputations for every condition in the set.

runCell <- function(cond,
                    parms,
                    fs,
                    rp) {

# Example Internals -------------------------------------------------------
  
  # set.seed(1234)
  # cond    = conds[2, ]
  # rp = 1

# Data Generation ---------------------------------------------------------

  # TEMP GET RID OF CONSTANTS
  var_types$bin <- var_types$bin[!var_types$bin %in% c("v227", "v230", "v232")]
  index <- which(colnames(EVS2017) %in% c("v227", "v230", "v232")) # in the small toy sample these are constants

  set.seed(1234)

  bs_dt <- bootstrapSample(dt = EVS2017[, -index],
                           train_p = .8)

  # Drop empty factor level (if the bootstrap sample happens to have them)
  for (j in 1:ncol(bs_dt$dt)){
    if(is.factor(bs_dt$dt[, j])){
      bs_dt$dt[, j] <- droplevels(bs_dt$dt[, j])
    }
  }

  # All variables as factors -----------------------------------------------
  dt_cat <- bs_dt$dt

  # Oridnal variables as numeric -----------------------------------------------
  dt_mix <- bs_dt$dt

  # Transform ordinal variables to numeric (polarity not important in this project)
  for (j in c(var_types$ord, var_types$cnts)){
    dt_mix[, j] <- as.numeric(dt_mix[, j])
  }

  # Transform binary variables to numeric 0, 1
  for (j in var_types$bin){
    dt_mix[, j] <- as.numeric(dt_mix[, j]) - 1
  }

  # Define the dependent variable
  dv_name <- var_types$bin[1]

  # Get rid of it from the vector of variables to avoid confusion down the line
  var_types$bin <- var_types$bin[-1]

  # Extract the index for this variable in the columns of the dataset
  dv_index <- which(colnames(bs_dt$dt) %in% dv_name)

# Analysis ----------------------------------------------------------------

  # continuous + categorical (PCAmix)
  pcs_mix_pcamix <- extractPCAmix(dt = dt_mix[, -dv_index],
                                  index_cont = c(var_types$bin,
                                                 var_types$ord,
                                                 var_types$cnts),
                                  index_disc = var_types$cat,
                                  keep = as.character(cond$npcs))

  # continuous + categorical (dummy)
  pcs_mix_dummy <- extractPCs(dt = dt_mix[, -dv_index],
                              index_cont = c(var_types$bin,
                                             var_types$ord,
                                             var_types$cnts),
                              index_disc = var_types$cat,
                              keep = as.character(cond$npcs),
                              coding = "dummy")

  # continuous + categorical (disjunction)
  pcs_mix_disj <- extractPCs(dt = dt_mix[, -dv_index],
                              index_cont = c(var_types$bin,
                                             var_types$ord,
                                             var_types$cnts),
                              index_disc = var_types$cat,
                              keep = as.character(cond$npcs),
                              coding = "disj")

  # all categorical (PCAmix)
  pcs_cat_mca <- extractPCAmix(dt = dt_cat[, -dv_index],
                               index_cont = NULL,
                               index_disc = c(var_types$bin,
                                              var_types$ord,
                                              var_types$cnts,
                                              var_types$cat),
                               keep = as.character(cond$npcs))

  # all categorical (dummy)
  pcs_cat_dummy <- extractPCs(dt = dt_cat[, -dv_index],
                              index_cont = NULL,
                              index_disc = c(var_types$cat,
                                             var_types$bin,
                                             var_types$ord,
                                             var_types$cnts),
                              keep = as.character(cond$npcs),
                              coding = "dummy")

  # all categorical (disjunction)
  pcs_cat_disj <- extractPCs(dt = dt_cat[, -dv_index],
                             index_cont = NULL,
                             index_disc = c(var_types$cat,
                                            var_types$bin,
                                            var_types$ord,
                                            var_types$cnts),
                             keep = as.character(cond$npcs),
                             coding = "disj")

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
  npcs <- lapply(pcs_list, "[[", "npcs")

  # Extract CPVE by the npcs
  r2 <- lapply(pcs_list, "[[", "r2")

  # PCR MSE
  mses <- lapply(dts_pcs, extractMSE, y = dt_mix[, dv_index],
                 train = bs_dt$train, test = bs_dt$test)

# Store Output ------------------------------------------------------------

  ## Define storing object
  output <- cbind(cond, npcs = npcs, r2 = r2, cors = cors, mses = mses)

  ## Return it
  saveRDS(output,
          file = paste0(fs$outDir,
                        "rep", rp,
                        "_cond", cond$tag,
                        ".rds")
  )
}