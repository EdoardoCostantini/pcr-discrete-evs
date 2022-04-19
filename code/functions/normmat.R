# Project:   pcr_discrete
# Objective: Function to normalize the columns of a matrix
# Author:    Edoardo Costantini
# Created:   2022-01-14
# Modified:  2022-01-14

normmat <- function(X){
# Internals -------------------------------------------------------------

    # X    = matrix(rnorm(1e3 * 4), nrow = 1e3, ncol = 4)

# Body ------------------------------------------------------------------
    X <- apply(X, 2, function(j) j / sqrt(sum(j^2)))
    return(X)
}
