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

  # Get a sample from EVS data
  XTP <- generateXTP(I = parms$N,
                     J = parms$P,
                     VAFr = parms$XTP_VAFr,
                     VAFsum = parms$XTP_VAFsum,
                     CPVE = parms$XTP_R2)

  # Define Training and Testing data
  ind   <- sample(1 : nrow(dat_orig))
  train <- ind[1 : (.8*nrow(dat_orig))]
  test  <- ind[(.8*nrow(dat_orig)+1) : nrow(dat_orig)]

# Analysis ----------------------------------------------------------------

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