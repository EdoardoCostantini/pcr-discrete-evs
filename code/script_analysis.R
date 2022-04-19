# Title:    Analysing results
# Author:   Edoardo Costantini
# Project:  Ordinality
# Created:  2021-06-10
# Modified: 2022-01-25

  ## Make sure we have a clean environment:
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
  inDir <- "../output/"
  grep("_box", list.files(inDir), value = TRUE)
  gg_shape <- readRDS(paste0(inDir, "20220125_143543_box.rds"))

# Plots -------------------------------------------------------------------

  # Define which outcome measure to plot
  result <- c("mses.", "npcs.", "r2.", "cors.")[1]

  # Define which conditions to plot and order of some factors
  K_conditions <- rev(sort(unique(gg_shape$K)))#[2]
  D_conditions <- sort(unique(gg_shape$D))[1]
  int_conditions <- unique(gg_shape$interval)[2]
  methods <- paste(
    c("orig", "nume", "poly", "dumm", "disj", "PCAmix"),
    collapse = "|"
  )
  npcs_conditions <- levels(gg_shape$npcs)#[1]

  # Define the caption of the plot
  caption <- paste0("y axis: ", stringr::str_remove(result, "\\."),
                    "; interval: ", int_conditions,
                    "; discrete: ", round(D_conditions, 2))

  # Obtain plot
  plot1 <- gg_shape %>%
    # Obtain Root MSE
    mutate(rmse = sqrt(value)) %>%
    # Subset
    filter(grepl(result, variable)) %>%
    filter(grepl(methods, variable)) %>%
    filter(D %in% D_conditions) %>%
    filter(K %in% K_conditions) %>%
    filter(interval %in% int_conditions) %>%
    filter(npcs %in% npcs_conditions) %>%
    # Change labels of X axis
    mutate(variable = fct_relabel(variable, str_replace, result, "")
    ) %>%
    # Main Plot
    ggplot(aes(x = variable, y = rmse)) +
    geom_boxplot() +
    # Grid
    facet_grid(rows = vars(factor(K)),
               cols = vars(factor(npcs)),
               scales = "fixed") +
    # Format
    # coord_cartesian(ylim = c(.9, 2.5)) +
    theme(text = element_text(size = 15),
          plot.title = element_text(hjust = 0.5),
          axis.text = element_text(size = 15),
          axis.text.x = element_text(angle = 45, hjust = 0.95),
          axis.title = element_text(size = 15)) +
    labs(title = stringr::str_remove(result, "\\."),
         x     = NULL,
         y     = NULL,
         caption = caption)

  # Look at plot
  plot1

# Save plots --------------------------------------------------------------

  file_format <- ".pdf"
  plot_name <- paste0("outcome_", stringr::str_remove(result, "\\."),
                      "_interval_", int_conditions,
                      "_discrete_", D_conditions)
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