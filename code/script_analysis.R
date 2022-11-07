# Project:   pcr_discrete_evs
# Objective: Script to plot results
# Author:    Edoardo Costantini
# Created:   2022-05-03
# Modified:  2022-06-14

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
  gg_shape <- readRDS("../output/20220513_145604_out.rds") # only 1 catergorical dv
  gg_shape <- readRDS("../output/20220516_150224_out.rds") # 2 categorical dvs

# Plots -------------------------------------------------------------------

  # Define which outcome measure to plot
  result <- unique(gg_shape$measure)[2]

  # Define which conditions to plot and order of some factors
  target_npcs    <- rev(sort(unique(gg_shape$cond_npcs)))#[c(1:4, 9, 13)]
  target_dv      <- unique(gg_shape$cond_dv)[4]
  target_measure <- unique(gg_shape$measure)[c(2)]

  # Define the caption of the plot
  caption <- paste0("Target variable: ", target_dv)

  # Obtain plot
  plot_rmse <- gg_shape %>%
    # Subset
    # filter(grepl("mse|centropy|cclass", gg_shape$x_factor_grid)) %>%
    filter(grepl(target_dv, cond_dv)) %>%
    filter(grepl(paste0(target_npcs, collapse = "|"), cond_npcs)) %>%
    filter(grepl(paste0(target_measure, collapse = "|"), measure)) %>%
    # Main Plot
    ggplot(aes(x = method, y = value)) +
    geom_boxplot() +
    # Grid
    facet_grid(rows = vars(factor(measure)),
               cols = vars(factor(cond_npcs)),
               scales = "free") +
    coord_cartesian(ylim = c(1, 5)) +
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
  plot_rmse

# > NPCS and R2 ----------------------------------------------------------------

  # Define which conditions to plot and order of some factors
  target_npcs    <- rev(sort(unique(gg_shape$cond_npcs)))#[c(1:4, 9, 13)]
  target_dv      <- unique(gg_shape$cond_dv)[1]
  target_measure <- unique(gg_shape$measure)[c(3)]

  # Define the caption of the plot
  caption <- paste0("Target variable: ", target_dv)

  # Obtain plot
  plot_npcs <- gg_shape %>%
    # Subset
    # filter(grepl("mse|centropy|cclass", gg_shape$x_factor_grid)) %>%
    filter(grepl(target_dv, cond_dv)) %>%
    filter(grepl(paste0(target_npcs, collapse = "|"), cond_npcs)) %>%
    filter(grepl(paste0(target_measure, collapse = "|"), measure)) %>%
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
    labs(title = NULL,
         x     = NULL,
         y     = NULL,
         caption = caption)

  # Look at plot
  plot_npcs

  as.data.frame(gg_shape %>%
                  # Subset
                  # filter(grepl(target_dv, cond_dv)) %>%
                  filter(grepl(paste0(target_npcs, collapse = "|"), cond_npcs)) %>%
                  filter(grepl(paste0(target_measure, collapse = "|"), measure)) %>%
                  # GRoup by
                  group_by(cond_dv, cond_npcs) %>%
                  summarize(average_npcs = round(mean(value), 1),
                            min_npcs = min(value),
                            max_npcs = max(value)))

# > Categorical plot -----------------------------------------------------------

  # Define which conditions to plot and order of some factors
  target_npcs    <- rev(sort(unique(gg_shape$cond_npcs)))
  target_dv      <- unique(gg_shape$cond_dv)[5]
  target_measure <- unique(gg_shape$measure)[c(5, 6)]

  # Define the caption of the plot
  caption <- paste0("Target variable: ", target_dv)

  # Obtain plot
  plot_cat <- gg_shape %>%
    # Subset
    # filter(grepl("mse|centropy|cclass", gg_shape$x_factor_grid)) %>%
    filter(grepl(target_dv, cond_dv)) %>%
    filter(grepl(paste0(target_npcs, collapse = "|"), cond_npcs)) %>%
    filter(grepl(paste0(target_measure, collapse = "|"), measure)) %>%
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
    labs(title = NULL,
         x     = NULL,
         y     = NULL,
         caption = caption)

  # Look at plot
  plot_cat

# > Binary plot ----------------------------------------------------------------

  # Define which conditions to plot and order of some factors
  target_npcs    <- rev(sort(unique(gg_shape$cond_npcs)))
  target_dv      <- unique(gg_shape$cond_dv)[3]
  target_measure <- unique(gg_shape$measure)[c(5, 6)]

  # Define the caption of the plot
  caption <- paste0("Target variable: ", target_dv)

  # Obtain plot
  plot_bin <- gg_shape %>%
    # Subset
    # filter(grepl("mse|centropy|cclass", gg_shape$x_factor_grid)) %>%
    filter(grepl(target_dv, cond_dv)) %>%
    filter(grepl(paste0(target_npcs, collapse = "|"), cond_npcs)) %>%
    filter(grepl(paste0(target_measure, collapse = "|"), measure)) %>%
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
    labs(title = NULL,
         x     = NULL,
         y     = NULL,
         caption = caption)

  # Look at plot
  plot_bin

# > Density plot ---------------------------------------------------------------

  # Define which outcome measure to plot
  result <- unique(gg_shape$measure)[1]

  # Define which conditions to plot and order of some factors
  target_npcs    <- rev(sort(unique(gg_shape$cond_npcs)))#[2]
  target_dv      <- unique(gg_shape$cond_dv)[1]
  target_measure <- unique(gg_shape$measure)[1]

  # Obtain plot
  plot1 <- gg_shape %>%
    # Subset
    # filter(grepl("mse|centropy|cclass", gg_shape$x_factor_grid)) %>%
    filter(grepl(target_dv, cond_dv)) %>%
    # filter(grepl(target_npcs, cond_npcs)) %>%
    filter(grepl(target_measure, measure)) %>%
    mutate(value = sqrt(value)) %>%
    # Main Plot
    ggplot(aes(x = value)) +
    geom_density() +
    # Grid
    facet_grid(rows = vars(factor(method)),
               cols = vars(factor(cond_npcs)),
               scales = "fixed") +
    geom_vline(aes(xintercept = mean(value)),
            color="blue", linetype="dashed", size = 1) +
    coord_cartesian(xlim = c(0, 5)) +
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