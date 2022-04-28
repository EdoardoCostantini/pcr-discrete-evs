# Project:   pcr_discrete_evs
# Objective: Defining Fixed Parameters
# Author:    Edoardo Costantini
# Created:   2021-06-10
# Modified:  2022-04-22

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
                 "nFactors",
                 "dplyr",
                 "testthat",
                 "FactoMineR",
                 "nnet" # for multinomial regression
  )

  # lapply(pack_list, install.packages)
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

# Load inputs

  EVS2017   <- readRDS("../input/ZA7500_processed.rds")
  var_types <- readRDS("../input/var_types.rds")

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

  # Number of components kept by the PCA extraction
  npcs <- c("naf", "nkaiser",       # non-graphical screeplot solutions
            1,                      # most summary
            seq(0.1, 0.9, .1),      # CPVE based
            parms$P)                # least summary

  # Make Conditionsa
  conds <- expand.grid(npcs = npcs,
                       stringsAsFactors = TRUE)

  # Append Condition Tag
  conds$tag <- sapply(1:nrow(conds), function(i) {
    paste0(colnames(conds), conds[i, ], collapse = "_")
  }
  )

