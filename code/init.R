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

# Fixed Parameters --------------------------------------------------------

  # Empty List
  parms    <- list()

  # Simulation parameter
  parms$dt_rep   <- 1e3 # number of data repetitionsù
  parms$seed     <- 2021
  parms$nStreams <- 1000

  # Data generation
  parms$DVs <- list(num = "v102",
                    bin = "v10",
                    cat = "v244")

# Load and prepare inputs

  # Load pre-processed EVS data
  EVS2017   <- readRDS("../input/ZA7500_processed.rds")

  # Load variables type description
  var_types <- readRDS("../input/var_types.rds")

  # Get rid of DVs from the variable type object
  var_types <- lapply(var_types, function (x){
    x[!x %in% unlist(parms$DVs)]
  })

# Experimental Conditions -------------------------------------------------

  # Number of components kept by the PCA extraction
  npcs <- c("naf", "nkaiser",       # non-graphical screeplot solutions
            1,                      # most summary
            10,
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

