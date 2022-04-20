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
  # cond    = conds[133, ]
  # rp = 1

# Data Generation ---------------------------------------------------------

  # TEMP GET RID OF CONSTANTS
  var_types$bin <- var_types$bin[!var_types$bin %in% c("v227", "v230", "v232")]
  index <- which(colnames(EVS2017) %in% c("v227", "v230", "v232")) # in the small toy sample these are constants

  bs_dt <- bootstrapSample(dt = EVS2017[, -index],
                           train_p = .8)

  # All variables as categorical
  dt_cat <- bs_dt$dt

  # Oridnal variables as numeric
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
  pcs_mix <- extractPCAmix(dt = dt_mix[, -dv_index],
                           index_cont = c(var_types$bin,
                                          var_types$ord,
                                          var_types$cnts,
                                          var_types$num),
                           index_disc = var_types$cat)

  # continuous + categorical (dummy)
  pcs_mix <- extractPCAmix(dt = dt_mix[, -dv_index],
                           index_cont = c(var_types$bin,
                                          var_types$ord,
                                          var_types$cnts,
                                          var_types$num),
                           index_disc = var_types$cat)

  # continuous + categorical (disjunction)
  pcs_mix <- extractPCAmix(dt = dt_mix[, -dv_index],
                           index_cont = c(var_types$bin,
                                          var_types$ord,
                                          var_types$cnts,
                                          var_types$num),
                           index_disc = var_types$cat)

  # all categorical (PCAmix)
  pcs_mca <- extractPCAmix(dt = dt_cat[, -dv_index],
                           index_cont = var_types$num,
                           index_disc = c(var_types$bin,
                                          var_types$ord,
                                          var_types$cnts,
                                          var_types$cat))

  # all categorical (dummy)
  pcs_mix <- extractPCAmix(dt = dt_mix[, -dv_index],
                           index_cont = c(var_types$bin,
                                          var_types$ord,
                                          var_types$cnts,
                                          var_types$num),
                           index_disc = var_types$cat)

  # all categorical (disjunction)
  pcs_mix <- extractPCAmix(dt = dt_mix[, -dv_index],
                           index_cont = c(var_types$bin,
                                          var_types$ord,
                                          var_types$cnts,
                                          var_types$num),
                           index_disc = var_types$cat)


  # PCA Original
  pcs_orig <- extractPCs(dt = dat_orig,
                         keep = as.character(cond$npcs))

  # PCA Numerical
  pcs_nume <- extractPCs(dt = dat_disc,
                         keep = as.character(cond$npcs))

  # PCA Disjunction table
  pcs_disj <- extractPCs(dt = dat_disj,
                         keep = as.character(cond$npcs))

  # PCA Dummy
  pcs_dumm <- extractPCs(dt = dat_dumm,
                         keep = as.character(cond$npcs))

  # PCA Polychoric
  pcs_poly <- extractPCsMixed(dt = dat_disc,
                              keep = as.character(cond$npcs))

  # PCAmix
  pcs_PCAmix <- extractPCAmix(dt = dat_disc,
                              keep = as.character(cond$npcs),
                              index_cont = index_cont,
                              index_disc = index_disc)

  # Append results
  pcs_list <- list(
    orig = pcs_orig,
    nume = pcs_nume,
    poly = pcs_poly,
    disj = pcs_disj,
    dumm = pcs_dumm,
    PCAmix = pcs_PCAmix
  )

  # Extract datasets of PC predictors
  dts_pcs <- lapply(pcs_list, "[[", "T")

  # Extract number of PCs extracted
  npcs <- lapply(pcs_list, "[[", "npcs")

  # Extract CPVE by the npcs
  r2 <- lapply(pcs_list, "[[", "r2")

  # Cor with dv
  cors <- lapply(dts_pcs, function(x){
    abs(colMeans(cor(x = x, y = y)))
  })

  # PCR MSE
  mses <- lapply(dts_pcs, extractMSE, y = y, train = train, test = test)

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