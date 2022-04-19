# Title:    Pooling results
# Author:   Edoardo Costantini
# Project:  Ordinality
# Created:  2021-06-10
# Modified: 2022-01-25

  ## Make sure we have a clean environment:
  rm(list = ls())

  ## Support Functions
  source("./init.R")

# Load Results ----------------------------------------------------------

  inDir <- "../output/"
  target_tar <- "20220125_143543.tar.gz"
  output <- readTarGz(target_tar)

# Restructure Results -----------------------------------------------------
# list of conditions containing results for every repetition

  # Give unique name to all objects
  names(output$out) <- output$file_names

  # Punt into a single data.frame
  out <- do.call(rbind, output$out)

  # Store
  saveRDS(out,
          file = paste0("../output/",
                        output$name_run,
                        "_out",
                        ".rds")
  )

  # Read
  file_name <- grep("out", list.files(inDir), value = TRUE)[4]
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