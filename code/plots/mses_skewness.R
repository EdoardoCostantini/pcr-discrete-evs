# Project:   ordinality
# Objective: Plot the performance of methods as skewness varies
# Author:    Edoardo Costantini
# Created:   2021-11-03
# Modified:  2021-11-17

  ## Make sure we have a clean environment:
  rm(list = ls())

# Packages ----------------------------------------------------------------

  pack_list <- c("ggplot2",
                 "dplyr",
                 "forcats",
                 "stringr")

  lapply(pack_list, library, character.only = TRUE, verbose = FALSE)

# Load Results ----------------------------------------------------------

  inDir <- "../output/"
  file_box <- grep("_box", list.files(inDir), value = TRUE)
  file_lin <- grep("_lin", list.files(inDir), value = TRUE)

  # Read output
  gg_shape <- readRDS(paste0(inDir, file_box[1]))
  gg_line <- readRDS(paste0(inDir, file_lin[2]))

  # Support Functions
  source("./init.R")

  colnames(gg_line)

# Inputs
  dat = gg_line
  plot_x_axis = "method"
  plot_y_axis = "rmses_md"
  x_axis_name = "Skewness"
  y_axis_name = "rmse"
  moderator = "skew"
  grid_x_axis = "K"
  grid_y_axis = "blocks"
  scales = NULL # or "free"
  error_bar = FALSE
  scale_x_cont = FALSE
  filters = list(K        = unique(gg_line$K),
                 D        = unique(gg_line$D)[c(5)],
                 rho      = unique(gg_line$rho)[c(7)],
                 blocks   = unique(gg_line$blocks),
                 skew     = unique(gg_line$skew),
                 interval = unique(gg_line$interval),
                 method   = unique(gg_line$method)
                 )

  # Subset data
  for (f in seq_along(filters)){
    filter_factor <- names(filters)[f]
    filter_lvels <- filters[[f]]
    dat <- dat %>%
      filter(!!as.symbol(filter_factor) %in% filter_lvels)
  }

  # Make sure moderator is factor
  dat[, moderator] <- factor(dat[, moderator])

  # Main Plot
  plot_main <- dat %>%
    ggplot(aes_string(x = plot_x_axis,
                      y = plot_y_axis,
                      fill = moderator)) +
    geom_bar(stat = "identity",
             position = "dodge") +
    scale_fill_manual(values = gray.colors(length(unique(dat[, moderator])),
                                           start = .7,
                                           end = .1))

  # Grid
  plot_grid <- plot_main +
    facet_grid(reformulate(grid_x_axis, grid_y_axis),
               labeller = label_both,
               scales = scales)

  # Format
  plot_themed <- plot_grid +
    theme(text = element_text(size = 15),
          plot.title = element_text(size = 10, hjust = 0.5),
          axis.text = element_text(size = 15),
          axis.text.x = element_text(angle = 45, hjust = 0.95),
          axis.title = element_text(size = 10)) +
    labs(title = paste0("Condition = ",
                        paste0(names(filters),
                               " = ",
                               filters, collapse = "; ")),
         x     = x_axis_name,
         y     = paste(y_axis_name)) +
    coord_cartesian(ylim = c(0, 15))
  plot_themed

