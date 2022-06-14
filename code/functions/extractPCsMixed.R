# Project:   pcr_discrete_evs
# Objective: extract PCs with the choric method
# Author:    Edoardo Costantini
# Created:   2022-05-18
# Modified:  2022-05-18

extractPChoric <- function(in_dt = matrix(), keep = 1L) {
# Internals ---------------------------------------------------------------

  # in_dt = dt_cho # MASS::mvrnorm(1e2, rep(0, 3), diag(3)) # some data.frame
  # keep = .9 # Y variable used for cross-validation to choose CVs

# Body --------------------------------------------------------------------
  # Extract PCs with psych mixed method (polychoric etc.)
  pcr_out <- psych::principal(
    in_dt[, c("v9", "v10", "v11", "v1", "v2", "v3", "v185")],
    nfactors = 3,
    cor = "mixed",
    rotate = "none",
    p = c("v1", "v2", "v3"),
    d = c("v9", "v10", "v11")
  )

  cmat  <- mixedCor(    data = in_dt[, c("v9", "v10", "v11", "v1", "v2", "v3", "v185")],
    c = "v185",
    p = c("v1", "v2", "v3"),
    d = c("v9", "v10", "v11")
  )

  psych::principal(
    r = cmat$rho,
    nfactors = 3,
    rotate = "none"
  )

  psych::principal(
    r = cor(in_dt[, c("v9", "v10", "v11", "v1", "v2", "v3")]),
    nfactors = 3,
    rotate = "none"
  )

  # Store the PC scores
  T <- pcr_out$scores

  # Compute a vector of cumulative proportions of explained variances
  CPVE <- cumsum(prop.table(pcr_out$values))

  # Check if keep is a non-graphical solution
  keep_nScree <- suppressWarnings(is.na(as.numeric(keep)))

  # Define npcs and CPVE based on type of keep
  if(keep_nScree){
    # Store the eigenvalues
    eigenvalues <- pcr_out$values

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
