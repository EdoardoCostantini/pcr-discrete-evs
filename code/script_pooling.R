# Project:   pcr_discrete_evs
# Objective: Script to pool the rds files together
# Author:    Edoardo Costantini
# Created:   2022-04-29
# Modified:  2022-04-29

  ## Make sure we have a clean environment:
  rm(list = ls())

  ## Support Functions
  source("./init.R")

# Load Results ----------------------------------------------------------

  inDir <- "../output/"
  target_tar <- "20220503_113756.tar.gz"
  output <- readTarGz(target_tar)

# Restructure Results -----------------------------------------------------
# list of conditions containing results for every repetition

  # Give unique name to all objects
  names(output$out) <- output$file_names

  # Separate errors and good results
  idx_errors <- grep("ERROR", output$file_names)
  output_scc <- output$out[-idx_errors]
  output_err <- output$out[idx_errors]

  # Punt into a single data.frame
  out <- do.call(rbind, output_scc)

  # Store
  saveRDS(out,
          file = paste0("../output/",
                        output$name_run,
                        "_out",
                        ".rds")
  )

  # Read
  file_name <- grep("out", list.files(inDir), value = TRUE)[1]
  run_name <- gsub("_out.rds", "", file_name)
  out <- readRDS(paste0("../output/", file_name))

  tag_column <- grep("tag", colnames(out))

# Restructure for Box plot ------------------------------------------------
  gg_shape <- reshape2::melt(out, id.var = colnames(out)[1:tag_column])

  # Save
  saveRDS(gg_shape,
          file = paste0("../output/",
                        output$name_run,
                        "_box",
                        ".rds")
  )