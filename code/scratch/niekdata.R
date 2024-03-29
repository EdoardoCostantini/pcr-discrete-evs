# Project:   pcr_discrete
# Objective: Copy of work from Niek for data generation
# Author:    Edoardo Costantini
# Created:   2021-12-15
# Modified:  2021-12-15
# Source:    https://github.com/trbKnl/sparseWeightBasedPCA/blob/master/R/makeData.R

#divide the columns of matrix by 2norm
divideByNorm <- function(mat){
    A <- 1 / matrix(apply(mat, 2, function(x){sqrt(sum(x^2))}),
                  nrow(mat), ncol(mat), byrow = T)
    return(mat * A)
}

#Orthogonalize two columns of a matrix by using gramm-schmid
#only use intersection of the non-zero coefficients of the two vectors
#to enforce that the zero's do not change
orthogonalize <- function(A, index1, index2){
    int <- intersect(which(A[, index1] != 0), which(A[, index2] != 0))

    u <- A[int, index1]
    v <- A[int, index2]

    newv <- v - as.numeric((t(u) %*% v) / (t(u) %*% u)) * u
    A[int, index2] <- newv

    return(A)
}

#create an orthonormal matrix
#where the zero's are in fixed position
makeP <- function(A){
    counter <- 0
    #while matrix A is not orthonormal and max iterations is not reached
    #do orthogonalize all columns with respect to each other
    #Do this until the columns are orthogonal,
    #sometimes multiple passes are needed
    while(TRUE != all.equal(t(A) %*% A, diag(ncol(A))) &&  counter < 1000 ){
        for(i in 2:ncol(A)){
            for(j in 1:(i-1)){
                A <- orthogonalize(A, j, i)
            }
        }
        A <- divideByNorm(A)
        counter <- counter + 1
    }
    if(counter < 1000){
        return(list(A=A, status=1))
    } else {
        return(list(A=A, status=0))
    }
}



#' makeVariance: Create variances for the components in data generation
#'
#' A function that creates realistic variances for J components. The variances of the first Q components are set by the user, the remaining J - Q variances
#' are decreasing on a reversed log scale.
#'
#' @param varianceOfcomp The variances of the first Q components of interest
#' @param J The total number of components (the number of variables)
#' @param error The total variance of J-Q components compared to the variance of the Q components
#' @return
#' A vector with J variances (eigenvalues) to simulate data from.
#' @export
#' @examples
#'
#' variances <- makeVariance(c(120, 100, 80), J = 50, error = 0.05)
#' plot(variances) #realistic eigenvalues of a data matrix X
#'
makeVariance <- function(varianceOfComps, J, error){
    #function to create variances for the components
    #you have to supply the variances of the components you are interested in
    #the variances of the other non interesting components are on a log scale
    #starting with mean(variances of interesting components) /2
    #these variances then get scaled such that error ratio is gotten.
    ncomp <- length(varianceOfComps)
    varsOfUnimportantComps <- exp(seq(log(0.0001), log(min(varianceOfComps)/2),
                                      length.out = J-ncomp))[(J-ncomp):1]

    x <- (-error*sum(varianceOfComps) / (error-1)) / sum(varsOfUnimportantComps)

    return(c(varianceOfComps, x * varsOfUnimportantComps))
}

#' Sparsify: shoots a percentage of zero's in non-zero elements of columns of a matrix
#'
#' At random set a percentage the non-zero elements of columns of a matrix to zero. This can be used to create a zero structure for the component weights/loadings from which to simulate data.
#'
#' @param comdis A data matrix of class \code{matrix}, with a common or distinctive structure. (i.e. zero and ones in pre specified locations)
#' @param sparsity The percentage of non-zero elements in the columns of \code{comdis} that should be set to zero. This is a value between 0 and 1
#' @return
#' A matrix with a percentage of the non-zero elements in \code{comdis} set to zero
#' @export
#' @examples
#'
#' comdis <- matrix(1, 40, 3)
#' sparsify(comdis, 0.7) #set 70% of the non-zero elements in comdis to zero
#'
sparsify <- function(comdis, sparsity){
    amounts <- round(apply(comdis, 2, function(x){sum(x != 0)}) * sparsity)
    TF <- apply(comdis, 2, function(x){x != 0})
    where <- apply(TF, 2, which)

    #If by accident the vectors in where are of the same size
    #apply returns a matrix instead of a list
    #therefore, if where isnt a list, split the columns and put them in a list
    if(!is.list(where)){
        where <- lapply(apply(where, 2, list), unlist)
    }

    for(i in 1:length(where)){
        comdis[sample(where[[i]], amounts[i]), i]  <- 0
    }
    return(comdis)
}


#' makeDat: creates simulated data based on a sparse PCA model
#'
#' A function that generates a realistic dataset based on a sparse PCA model \eqn{X = XWP^T}, with a sparse \eqn{W} and with W = P and \eqn{W^T W = I}
#'
#' @param n The number of objects the data should have
#' @param ncomp The number of components that are of interest
#' @param comdis A \code{matrix} specifying the zero structure of \eqn{W}, the data will have ncomp = \code{ncol(comdis)} "important components" and J = \code{nrow(comdis)} variables
#' @param variances specifying the variances of the J components these are the J eigenvalues of \eqn{X^T X}
#' @return A list with the following items: \cr
#' \code{X} A data matrix generated from MASS::mvrnorm() with a zero mean structure and Sigma = P \%*\% diag(variances) \%*\% t(P), empirical is set \code{FALSE} \cr
#' \code{P} A matrix of dimension J x J, with the loadings/weights the first Q columns have the sparsity structure specified in \code{comdis}, the other Q-J columns are non-sparse. \cr
#' \code{Sigma} The covariance matrix that is used to generate the data from Sigma \code{= P \%*\% diag(variances) \%*\% t(P)} \cr
#' In case of failure the function returns \code{NA}. The function can fail if the \code{comdis} structure specified in P is not possible, i.e. linear dependency
#'
#' @export
#' @examples
#'
#' ncomp <- 3
#' J <- 30
#' comdis <- matrix(1, J, ncomp)
#' comdis <- sparsify(comdis, 0) #set 70% of the 1's to zero
#' variances <- makeVariance(varianceOfComps = c(100, 80, 70), J = J, error = 0.05) #create realistic eigenvalues
#' dat <- makeDat(n = 100, comdis = comdis, variances = variances)
#'
makeDat <- function(n, comdis, variances){
    #Generate random P and fix the zero structure
    #and make P square
    ncomp <- ncol(comdis)
    p <- nrow(comdis)
    P <- matrix(rnorm(p*ncomp), p, ncomp)
    P[comdis == 0] <- 0
    P <- cbind(P, matrix(rnorm(p*(p-ncomp)), p, p-ncomp))

    result <- makeP(P)
    if(result$status == 1){
        P <- result$A
        Sigma <- P %*% diag(variances) %*% t(P)
        X <- MASS::mvrnorm(n, mu = rep(0, p), Sigma, empirical=F)
        return(list(X=X, P=P, Sigma=Sigma))
    } else {
        cat("Failed\n")
        return(NA)
    }
}

cond  <- conds[952, ]
J     <- 6
ncomp <- 2
dat   <- makeDat(n = 1e3,
                 comdis = matrix(1, J, ncomp),
                 variances = makeVariance(varianceOfComps = seq(10, 5, length.out = ncomp),
                                          J = J,
                                          error = 0.05))

round(makeVariance(varianceOfComps = seq(10, 5, length.out = ncomp),
                                          J = J,
                                          error = 0.01), 3)

eig <- makeVariance(varianceOfComps = seq(10, 5, length.out = ncomp),
                    J = J,
                    error = 0.05)

sum(eig[-(1:2)])/sum(eig)
round(eig, 5)

p <- 10
ncomp <- c(1, 2, 5)

c(10, 9.5, 8.5, 7.5, 6.5)
c(10, 9.5, 8.5, 7.5, 6.5)

seq(10, 5, length.out = 5)
# total_sum should be 30
# devide 30 to total numebr of components

dat   <- makeDat(n = 1e3,
                 comdis = matrix(1, J, ncomp),
                 variances = makeVariance(varianceOfComps = c(3, 1, rep(.001, 3)),
                                          J = J,
                                          error = 0.5))

round(eigen(cov(dat$X))$values, 3)
round(eigen(cor(dat$X))$values, 3)
round(cor(dat$X),2)

dat$P
dat$Sigma

T