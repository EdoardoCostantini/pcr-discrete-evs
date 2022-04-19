# Project:   pcr_discrete
# Objective: Function to orthogonalize the columns of a matrix
# Author:    Edoardo Costantini
# Created:   2022-01-14
# Modified:  2022-01-14

orthmat <- function(X, verbose = FALSE) {
# Internals -------------------------------------------------------------

    # X    = matrix(rnorm(1e3 * 4), nrow = 1e3, ncol = 4)

# Body ------------------------------------------------------------------
    for (i in 2:ncol(X)) {
        for (j in 1:(i - 1)) {
            if (verbose == TRUE) {
                print(paste0("Adjusting piar ", i, "-", j))
            }
            A <- X[, j]
            b <- X[, i]

            # Find projection of b on A
            B <- as.vector(b - (t(A) %*% b / t(A) %*% A) %*% A)

            # Replace in original X the orthogonalized columns
            X[, j] <- A
            X[, i] <- B
        }
    }
    return(X)
}