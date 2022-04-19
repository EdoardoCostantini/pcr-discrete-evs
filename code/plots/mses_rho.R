# Project:   ordinality
# Objective: Plot the performance of methods against correlation among vars
# Author:    Edoardo Costantini
# Created:   2021-11-03
# Modified:  2021-11-17

  rm(list = ls())

# Packages ----------------------------------------------------------------

  pack_list <- c("ggplot2",
                 "dplyr",
                 "forcats",
                 "stringr")

  lapply(pack_list, library, character.only = TRUE, verbose = FALSE)

# Load Results ----------------------------------------------------------

  inDir <- "../output/"
  file_lin <- grep("_lin", list.files(inDir), value = TRUE)

  # Read output
  gg_line <- readRDS(paste0(inDir, file_lin[3]))

  # Support Functions
  source("./init.R")

# Plot --------------------------------------------------------------------

# Inputs
  dat = gg_line
  plot_x_axis = "rho"
  plot_y_axis = "rmses_md"
  x_axis_name = "Predictors Correlation"
  y_axis_name = "rmse"
  moderator = "method"
  grid_x_axis = "K"
  grid_y_axis = "blocks"
  scales = NULL # or "free"
  error_bar = FALSE
  scale_x_cont = FALSE
  filters = list(K        = unique(gg_line$K),
                 D        = unique(gg_line$D)[c(5)],
                 rho      = unique(gg_line$rho),
                 blocks   = unique(gg_line$blocks),
                 skew     = unique(gg_line$skew)[1],
                 interval = unique(gg_line$interval),
                 method   = unique(gg_line$method)[1:5]#[c(1, 2, 5)]
                 )

  # Subset data
  for (f in seq_along(filters)){
    filter_factor <- names(filters)[f]
    filter_lvels <- filters[[f]]
    dat <- dat %>%
      filter(!!as.symbol(filter_factor) %in% filter_lvels)
  }

  # Main Plot
  plot_main <- dat %>%
    ggplot(aes_string(x = plot_x_axis,
                      y = plot_y_axis,
                      group = moderator)) +
    geom_line(aes_string(linetype = moderator), size = 1) +
    geom_point(size = 0) +
    scale_linetype_manual(values=c("solid", "longdash", "dashed", "dotdash", "dotted"))

  # Add error bars if wanted
  if(error_bar == TRUE){
    plot_main <- plot_main +
      geom_errorbar(aes(ymin = lwr_avg,
                        ymax = upr_avg,
                        group = method),
                    width = .1)
  }

  # Grid
  plot_grid <- plot_main +
    facet_grid(reformulate(grid_x_axis, grid_y_axis),
               labeller = label_both,
               scales = scales)

  # Format
  plot_themed <- plot_grid +
    theme(text = element_text(size = 15),
          plot.title = element_text(hjust = 0.5),
          axis.text = element_text(size = 15),
          axis.text.x = element_text(angle = 45, hjust = 0.95),
          axis.title = element_text(size = 10)) +
    labs(title = NULL,
         x     = x_axis_name,
         y     = paste(y_axis_name))
  plot_themed