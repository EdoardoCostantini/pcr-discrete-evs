# Project:   pcr_discrete_evs
# Objective: Defining Fixed Parameters
# Author:    Edoardo Costantini
# Created:   2021-06-10
# Modified:  2022-05-03

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

# Fixed Parameters --------------------------------------------------------

# Load and prepare inputs

  # Load pre-processed EVS data
  EVS2017   <- readRDS("../input/ZA7500_processed.rds")

  # Load variables type description
  var_types <- readRDS("../input/var_types.rds")

  # Create empty list to store parameters
  parms    <- list()

  # Simulation parameter
  parms$ss       <- 2e3 # bootstrap sample size
  parms$dt_rep   <- 1e3 # number of data repetitionsù
  parms$seed     <- 2021
  parms$nStreams <- 1000


# Experimental Conditions -------------------------------------------------

  # Variable to predict
  dv <- c(
    num = "v174_LR", # left right voting
    bin = "v31",     # most people can be trusted yes / no
    cat = "v261"     # marital status
    # num = "v160",  # prostitution justified? 10 ordered cats
    # bin = "v112",  # would you fight for country
    # cat = "v243_ISCED_1" # educational level
  )

  # Number of components kept by the PCA extraction
  npcs <- c("naf", "nkaiser",       # non-graphical screeplot solutions
            1,                      # most summary
            10,
            seq(0.1, 0.9, .1),      # CPVE based
            parms$P)                # least summary

  # Make Conditionsa
  conds <- expand.grid(dv = dv,
                       npcs = npcs,
                       stringsAsFactors = TRUE)

  # Append Condition Tag
  conds$tag <- sapply(1:nrow(conds), function(i) {
    paste0(colnames(conds), conds[i, ], collapse = "_")
  }
  )

