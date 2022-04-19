# Project:   pcr_discrete
# Objective: Extract Principal Components with different methods
# Author:    Edoardo Costantini
# Created:   2021-10-19
# Modified:  2022-01-21

extractPCs <- function(dt = matrix(), keep = 1L){
# Description -------------------------------------------------------------

  # Given a data set A in matrix for, it extracts the first keep principal
  # components from A, and returns a dataset
  # with the first column of A cobined with the extracted components.
  # It also retunrs the info regarding the proportion of explained variance
  # by the defined number of components
  # when @cor_tupe = "mixed", psych::principal recognizes which variables
  # need pearson, polyserial, polychoric, tetrachoric correlations

# Internals ---------------------------------------------------------------

  # dt = dat_disc # MASS::mvrnorm(1e2, rep(0, 3), diag(3))
  # keep = c("naf", "nkaiser", ".8", "3")[2] # how should we decide what pcs to keep?

# Body --------------------------------------------------------------------
  # Make sure data is scaled
  dt <- scale(dt)

  # SVD decomposition
  svd_out <- svd(dt)

  # Compute the PC scores
  T <- (svd_out$u %*% diag(svd_out$d))

  # Compute a vector of cumulative proportions of explained variances
  CPVE <- cumsum(prop.table(svd_out$d^2))

  # Check if keep is a non-graphical solution
  keep_nScree <- suppressWarnings(is.na(as.numeric(keep)))

  # Define npcs and CPVE based on type of keep
  if(keep_nScree){
    # Store the eigenvalues
    eigenvalues <- svd_out$d^2

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
