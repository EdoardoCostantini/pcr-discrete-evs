# Project:   pcr_discrete_evs
# Objective: Subroutine doRep (windows parallelization framework)
# Author:    Edoardo Costantini
# Created:   2021-06-10
# Modified:  2022-05-03

# Simulation set up ------------------------------------------------------------

  # Make sure we have a clean environment:
  rm(list = ls())

  # Initialize the environment:
  source("./init.R")

  # Prepare storing results
  source("./fs.R")

  # Create file system required objects
  dir.create(fs$outDir)
  file.create(paste0(fs$outDir, fs$fileName_prog, ".txt"))

  # Initialize report txt file
  cat(paste0("SIMULATION PROGRESS REPORT",
             "\n",
             "Starts at: ", Sys.time(),
             "\n", "------", "\n" ),
      file = paste0(fs$outDir, fs$fileName_prog, ".txt"),
      sep = "\n",
      append = TRUE)

  # Define repetitions and clusters
  reps <- 1 : 500
  clus <- makeCluster(15)

  # export fs object from the global env to worker nodes
  clusterExport(cl = clus, varlist = "fs", envir = .GlobalEnv)

  # export scripts to be executed to worker nodes
  clusterEvalQ(cl = clus, expr = source("./init.R"))

# Run simulation ---------------------------------------------------------------

  # Get start of run time
  sim_start <- Sys.time()

  # Run the computations in parallel on the 'clus' object:
  out <- parLapply(cl    = clus,
                   X     = reps,
                   fun   = doRep,
                   conds = conds,
                   parms = parms,
                   fs = fs)

  # Kill the cluster:
  stopCluster(clus)

  # Get end of run time
  sim_ends <- Sys.time()

  # Close report script
  cat(paste0("\n", "------", "\n",
             "Ends at: ", Sys.time(), "\n",
             "Run time: ",
             round(difftime(sim_ends, sim_start, units = "hours"), 3), " h",
             "\n", "------", "\n"),
      file = paste0(fs$outDir, fs$fileName_prog, ".txt"),
      sep = "\n",
      append = TRUE)

# Attach Extract Info Objects --------------------------------------------------

  out_support <- list()
  out_support$parms <- parms
  out_support$conds <- conds
  out_support$session_info <- devtools::session_info()

# Save output -------------------------------------------------------------

  saveRDS(out_support,
          paste0(fs$outDir, "sInfo.rds"))

# Zip output folder -------------------------------------------------------

  writeTarGz(fs$fileName_res)