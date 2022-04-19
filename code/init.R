# Title:    Defining Fixed Parameters
# Project:  Ordinality
# Author:   Edoardo Costantini
# Created:  2021-06-10
# Modified: 2022-01-25

# Packages ----------------------------------------------------------------

  pack_list <- c("parallel",
                 "MASS",
                 "lavaan",
                 "rlecuyer",
                 "parallel",
                 "MLmetrics",
                 "PCAmixdata",
                 "psych",
                 "stringr",
                 "dplyr",
                 "sn",          # for Multivariate Skewed Distribution
                 "fungible",    # for Multivariate Skewed Distribution
                 "testthat",
                 "moments",     # for skewness
                 "nFactors",    # for non-graphical solutions to npcs
                 "FactoMineR")

  lapply(pack_list, library, character.only = TRUE, verbose = FALSE)

# Load Functions ----------------------------------------------------------

  # Subroutines
  all_subs <- paste0("./subroutines/",
                     list.files("./subroutines/"))
  lapply(all_subs, source)

  # Functions
  all_funs <- paste0("./functions/",
                     list.files("./functions/"))
  lapply(all_funs, source)

  # Helper
  all_help <- paste0("./helper/",
                     list.files("./helper/"))
  lapply(all_help, source)

# Fixed Parameters --------------------------------------------------------

  # Empty List
  parms    <- list()

  # Simulation parameter
  parms$dt_rep   <- 1e3 # number of data repetitionsù
  parms$seed     <- 2021
  parms$nStreams <- 1000

  # Data generation
  parms$N          <- 1e3 # sample size
  parms$P          <- 12  # number of variables
  parms$XTP_VAFr   <- c(.5, .3, .2) # relative variance of each component
  parms$XTP_VAFsum <- 100 # total variance of the components
  parms$XTP_R2     <- 0.8
  parms$yT_R2      <- 0.8
  parms$yT_beta    <- 1
  parms$min_bin    <- 0.05 # target minimum proportion of cases in every
                           # category of discretized variables

# Experimental Conditions -------------------------------------------------

  # Number of categories for the discretized variables
  n_cate <- c(7, 5, 3, 2)

  # Proportion of variables discretized in X
  p_cate <- c(1/3, 2/3, 1)

  # Number of components kept by the PCA extraction
  npcs <- c("naf", "nkaiser",       # non-graphical screeplot solutions
            parms$XTP_R2,           # (true) CPVE based
            1,                      # most summary
            length(parms$XTP_VAFr), # (true) number of components
            parms$P)                # least summary

  # Discretization happens with equal intervals or not
  interval <- c(TRUE, FALSE)

  # Make Conditionsa
  conds <- expand.grid(K = n_cate, # number of categories
                       D = p_cate, # ordinality degree
                       npcs = npcs,
                       interval = interval,
                       stringsAsFactors = TRUE)

  # Append Condition Tag
  conds$tag <- sapply(1:nrow(conds), function(i) {
    conds$D <- round(conds$D*100,0) # to improve tag name
    paste0(colnames(conds), conds[i, ], collapse = "_")
  }
  )

