# Project:   pcr_discrete_evs
# Objective: extract PCs with the PCAmix method
# Author:    Edoardo Costantini
# Created:   2022-01-16
# Modified:  2022-04-29

extractPCAmix <- function(in_dt = matrix(), keep = 1L, index_cont, index_disc) {
# Internals ---------------------------------------------------------------

  # in_dt = dat_disc # MASS::mvrnorm(1e2, rep(0, 10), diag(10))
  # keep = .9 # either and integer specifying the number of components or a
  #           # double specifying the proportion of variance explained that
  #           # should be kept
  # index_cont = c(1:2)
  # index_disc = c(3:10)

# Body --------------------------------------------------------------------
  # Define indexing objects for variable types
  dt_quanti <- in_dt[, index_cont, drop = FALSE]
  dt_quali  <- in_dt[, index_disc, drop = FALSE]

  # Compute the max number of dimensions
  maxdim <- ncol(dt_quanti) + ncol(tab.disjonctif(dt_quali))

  # Extract components
  if(ncol(dt_quanti) == 0){
    pcamix <- PCAmix(X.quali = dt_quali,
                     rename.level = TRUE,
                     ndim = maxdim, graph = FALSE)
  }
  if(ncol(dt_quanti) != 0){
    pcamix <- PCAmix(X.quanti = dt_quanti,
                     X.quali  = dt_quali,
                     rename.level = TRUE,
                     ndim = maxdim, graph = FALSE)
  }

  # Store the PC scores
  T <- pcamix$ind$coord

  # Compute a vector of cumulative proportions of explained variances
  CPVE <- pcamix$eig[, "Cumulative"]/100

  # Check if keep is a non-graphical solution
  keep_nScree <- suppressWarnings(is.na(as.numeric(keep)))

  # Define npcs and CPVE based on type of keep
  if(keep_nScree){
    # Store the eigenvalues
    eigenvalues <- pcamix$eig[, "Eigenvalue"]

    # Compute all non-graphical solutions
    non_graph_scree <- nScree(x = eigenvalues)

    # Keep the result of the one with desired name
    npcs <- non_graph_scree$Components[, keep]
  } else {
    # Convert keep to number
    keep <- as.numeric(as.character(keep))
    if(keep < 1) {
      # Set npcs to the firt PC that explains more than target
      npcs <- Position(function(x) x >= keep, CPVE)
    } else {
      # Set npcs to the integer value provided
      npcs <- keep
    }
  }

  # Store the CPVE associated with this npcs
  r2 <- CPVE[npcs]

  # Store
  return(list(T    = T[, 1:npcs, drop = FALSE],
              npcs = npcs,
              r2   = round(r2, 3)))

}
