# Project:   pcr_discrete_evs
# Objective: Script to plot results
# Author:    Edoardo Costantini
# Created:   2022-05-03
# Modified:  2022-05-03

  # Make sure we have a clean environment:
  rm(list = ls())

# Packages ----------------------------------------------------------------

  pack_list <- c("ggplot2",
                 "dplyr",
                 "forcats",
                 "stringr")

  lapply(pack_list, library, character.only = TRUE, verbose = FALSE)

# Load Results ----------------------------------------------------------

  # Support Functions
  source("./init.R")

  # Read output
  gg_shape <- readRDS("../output/20220503_113756_out.rds")

# Plots -------------------------------------------------------------------

  # Define which outcome measure to plot
  result <- unique(gg_shape$measure)[1]
  
  # Define which conditions to plot and order of some factors
  target_npcs    <- rev(sort(unique(gg_shape$cond_npcs)))#[2]
  target_dv      <- unique(gg_shape$cond_dv)[1]
  target_measure <- unique(gg_shape$measure)

  # Define the caption of the plot
  caption <- paste0("Target variable: ", target_dv)

  # Obtain plot
  plot1 <- gg_shape %>%
    # Subset
    filter(grepl(target_dv, cond_dv)) %>%
    # Main Plot
    ggplot(aes(x = method, y = value)) +
    geom_boxplot() +
    # Grid
    facet_grid(rows = vars(factor(measure)),
               cols = vars(factor(cond_npcs)),
               scales = "free") +
    # Format
    theme(text = element_text(size = 15),
          plot.title = element_text(hjust = 0.5),
          axis.text = element_text(size = 15),
          axis.text.x = element_text(angle = 90, hjust = 0.95),
          axis.title = element_text(size = 15)) +
    labs(title = stringr::str_remove(result, "\\."),
         x     = NULL,
         y     = NULL,
         caption = caption)

  # Look at plot
  plot1

# Save plots --------------------------------------------------------------

  file_format <- ".pdf"
  plot_name <- paste0("outcome_", target_dv)
  out_dir <- "~/Desktop/"
  file_name <- paste0(out_dir, plot_name, file_format)
  if(file_format == ".pdf"){
    pdf(file_name, width = 15, height = 15)
  }
  if(file_format == ".png"){
    png(file_name, width = 15, height = 15, units = "in", res = 384)
  }
  plot1
  dev.off()