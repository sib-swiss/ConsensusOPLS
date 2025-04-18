#' @title RVConsensusOPLS
#' @description
#' Consensus OPLS-DA with RV coefficients weighting and 
#' DQ2 computation for discriminant analysis.
#' 
#' @param data A list of numeric matrices.
#' @param Y A vector, factor, dummy matrix or numeric matrix for the response.
#' @param maxPcomp Maximum number of Y-predictive components. 
#' @param maxOcomp Maximum number of Y-orthogonal components.
#' @param nfold Number of cross-validation rounds (integer).
#' @param cvType Type of cross-validation used. Either \code{nfold} for n-fold
#' cross-validation, \code{mccv} for Monte Carlo CV or \code{mccvb} for Monte 
#' Carlo class-balanced CV.
#' @param cvFrac numeric. Fraction of observations used in the training set. 
#' Default, 2/3. Fraction of data to be used for \code{mccv} and \code{mccvb}
#' cross-validation
#' @param modelType type of OPLS regression model. Can be defined as \code{reg} 
#' for regression or \code{da} for discriminant analysis. Default, \code{da}.
#' @param mc.cores Number of cores for parallel computing. Default, 1.
#' @param kernelParams List of parameters for the kernel. Default,
#' \code{list(type='p', params = c(order=1.0))}.
#' @param verbose Logical which indicates whether the user wants to see the 
#' progress bar printed in the \code{ConsensusOLPSCV} function.
#'
#' @returns A consensus OPLS model.
#'
#' @examples
#' data(demo_3_Omics)
#' ConsensusOPLS:::RVConsensusOPLS(data=demo_3_Omics[c("MetaboData", "MicroData", "ProteoData")], 
#'                                 Y=demo_3_Omics$Y, modelType="da", maxPcomp=1, mc.cores=1, nfold=3)
#' @import parallel
#' @keywords internal
#' @noRd
#' 
RVConsensusOPLS <- function(data,
                            Y,
                            maxPcomp = 1,
                            maxOcomp = 5,
                            modelType = 'da',
                            cvType = 'nfold',
                            nfold = 5,
                            nMC = 100,
                            cvFrac = 4/5,
                            kernelParams = list(type='p',
                                                params = c(order=1.0)),
                            mc.cores = 1,
                            verbose = FALSE) {
    # Variable format control
    if (!is.list(data))
        stop("data is not a list.")
    if (!is.matrix(Y) && !is.vector(Y) && !is.factor(Y))
        stop("Y is not either matrix, vector or factor.")
    if (!is.numeric(maxPcomp))
        stop("maxPcomp is not numeric.")
    if (!is.numeric(maxOcomp))
        stop("maxOcomp is not numeric.")
    if (!is.numeric(nfold))
        stop("nfold is not numeric.")
    if (!is.character(cvType))
        stop("cvType is not a character.")
    else if (!(cvType %in% c("nfold", "mccv", "mccvb")))
        stop("cvType must be `nfold`, `mccv` or `mccvb`.")
    if (!is.character(modelType))
        stop("modelType is not a character.")
    else if (!(modelType %in% c("reg", "da")))
        stop("modelType must be `reg` or `da`.")
    if (!is.logical(verbose))
        stop("verbose must be logical `TRUE` or `FALSE`.")
    
    # Check collection dimension
    ntable <- length(data)
    nsample <- nrow(data[[1]])
    nvar <- sapply(data, ncol)
    
    # Initialize parameters
    # TODO: control all possible options
    preProcK <- "mc"
    preProcY <- "mc"
    
    if (modelType == "reg") {
        Y <- as.matrix(Y)
        if (ncol(Y) > 1 || !is.numeric(Y))
            stop("modelType is preferably `da`.")
        koplsScale <- koplsScale(X = Y,
                                 centerType = preProcY,
                                 scaleType = "no")
        Yc <- koplsScale$X
    } else {
        if (nlevels(as.factor(Y)) > length(Y)/2) {
            # TODO: something better than this check: if at least 2 samples belong to every class
            stop("modelType is preferably `reg`")
        }
        if (is.vector(Y) || is.factor(Y) || ncol(Y) == 1)
            Y <- koplsDummy(as.vector(Y))
        if (is.null(colnames(Y)))
            colnames(Y) <- 1:ncol(Y)
        Yc <- Y
    }
    
    # For each data block
    RA <- mclapply(X = 1:ntable, mc.cores = mc.cores, FUN = function(i) {
        # Produce the kernel of the data block
        kerneli <- koplsKernel(X1 = data[[i]], X2 = NULL, 
                               type = kernelParams$type,
                               params = kernelParams$params)
        # Normalize the Kernel
        AMat <- kerneli/norm(x=kerneli, type='F')
        # RV coefficient for AMat
        RV <- (RVmodified(X = AMat, Y = Yc) + 1) / 2
        
        return (list(RV=RV, AMat=AMat))
    })
    names(RA) <- names(data)
    # RV coefficient 
    RV <- mclapply(RA, mc.cores = mc.cores, FUN = function(x) x$RV)
    # Normalized kernel
    AMat <- mclapply(RA, mc.cores = mc.cores, FUN = function(x) x$AMat)
    # Calculate the weighted sum of blocks kernel by the RV coeff
    W_mat <- Reduce("+", mclapply(1:ntable, mc.cores = mc.cores,
                                  FUN = function(i) {
                                      RA[[i]]$RV * RA[[i]]$AMat
                                  }))
    
    ## TODO: check for numerical issue
    #if (det(W)==0 && rankMatrix(W)==nrow(W))
    ##
    
    # Control maxOcomp
    maxOcomp <- min(c(maxOcomp, nsample, nvar))
    
    # Perform a Kernel-OPLS cross-validation for W_mat
    modelCV <- ConsensusOPLSCV(K = W_mat,
                               Y = Y,
                               maxPcomp = maxPcomp,
                               maxOcomp = maxOcomp,
                               modelType = modelType,
                               cvType = cvType,
                               nfold = nfold,
                               nMC = nMC,
                               cvFrac = cvFrac,
                               preProcK = preProcK,
                               preProcY = preProcY,
                               mc.cores = mc.cores,
                               verbose = verbose)
    
    Ylarg <- ncol(Y)
    
    # Search for the optimal model
    if (modelType == 'da') { # Search for the optimal model based on DQ2
        mc <- min((maxOcomp+1)*Ylarg, mc.cores)
        mcj <- min(sqrt(mc), Ylarg)
        mci <- max(floor(mc.cores/mcj), 1)
        
        results <- mclapply(X = 0:maxOcomp, mc.cores = mci, FUN = function(i) {
            mclapply(X = 1:Ylarg, mc.cores = mcj, FUN = function(j) {
                # For each Y column, perform the DQ2
                result <- DQ2(
                    Ypred = matrix(
                        data = modelCV$cv$AllYhat[, Ylarg*i+j],
                        ncol = 1), 
                    Y = Y[unlist(modelCV$cv$cvTestIndex), j, drop=F])
                return (result)
            })
        })
        DQ2mat <- do.call(
            rbind,
            mclapply(X = 0:maxOcomp, mc.cores = mci, FUN = function(i) {
                do.call(
                    cbind,
                    mclapply(X = 1:Ylarg, mc.cores = mcj, FUN = function(j) {
                        return (results[[i+1]][[j]]$DQ2)
                        #TODO: why two columns are the same
                    }))
            }))
        PRESSD <- do.call(
            rbind,
            mclapply(X = 0:maxOcomp, mc.cores = mci, FUN = function(i) {
                do.call(
                    cbind,
                    mclapply(X = 1:Ylarg, mc.cores = mcj, FUN = function(j) {
                        return (results[[i+1]][[j]]$PRESSD)
                    }))
            }))
        
        # DQ2 for models with different number of orthogonal components
        DQ2s <- rowMeans(DQ2mat)
        index <- maxPcomp + 1 #TODO: this is to have nOcompOpt > 0
        
        # Find the optimal number of orthogonal components as a function of DQ2
        while (index < (maxOcomp+maxPcomp) && 
               !is.na((DQ2s[index+1] - DQ2s[index])) &&
               (DQ2s[index+1] - DQ2s[index]) > 0.01) {
            index <- index + 1
        }
        
        # Add DQ2 to the model object
        modelCV$cv$DQ2Yhat <- setNames(DQ2s, c("p", paste0("po", 1:maxOcomp)))
        # Add optimal number of orthogonal components to the model object
        modelCV$cv$nOcompOpt <- index - maxPcomp
    } else { # if modelType == "reg", search for the optimal model based on Q2
        index <- maxPcomp + 1
        
        # Find the optimal number of orthogonal components as a function of Q2Yhat
        while (index < (maxOcomp+maxPcomp) && 
               modelCV$cv$Q2Yhat[index+1] - modelCV$cv$Q2Yhat[index] > 0.01) {
            index <- index + 1
        }
        # Add optimal number of orthogonal components to the model object
        modelCV$cv$nOcompOpt <- index - maxPcomp
    }
    
    # Simplifies the name to be used afterwards
    nOcompOpt <- modelCV$cv$nOcompOpt

    # Recompute the optimal model using nOcompOpt parameters
    modelCV$koplsModel <- koplsModel(K = W_mat, Y = Y,
                                     A = maxPcomp, nox = nOcompOpt, 
                                     preProcK = preProcK, preProcY = preProcY)
    
    # Adjust Yhat to the selected model size
    #modelCV$cv$Yhat <- modelCV$cv$AllYhat[, Ylarg*nOcompOpt + 1:Ylarg, drop=F]

    # Compute the blocks contributions for the selected model
    lambda <- cbind(
        do.call(rbind,
                mclapply(X = 1:ntable, mc.cores = mc.cores, FUN = function(j) {
                    diag(crossprod(
                        x = modelCV$koplsModel$scoresP[, 1:maxPcomp], 
                        y = crossprod(
                            x = t(AMat[[j]]), 
                            y = modelCV$koplsModel$scoresP[, 1:maxPcomp])))
                })),
        do.call(rbind,
                mclapply(X = 1:ntable, mc.cores = mc.cores, FUN = function(j) {
                    diag(crossprod(
                        x = modelCV$koplsModel$scoresO[, 1:nOcompOpt], 
                        y = crossprod(
                            x = t(AMat[[j]]), 
                            y = modelCV$koplsModel$scoresO[, 1:nOcompOpt])))
                })))
    rownames(lambda) <- names(data)
    colnames(lambda) <- c(paste0("p_", 1:maxPcomp),
                          paste0("o_", 1:nOcompOpt))
    
    # Store raw lambda coefficient values in the model object
    modelCV$koplsModel$lambda <- lambda
    
    # Contribution of each block as normalized lambda values
    modelCV$koplsModel$blockContribution <- sweep(x = lambda,
                                                  MARGIN = 2,
                                                  STATS = colSums(lambda),
                                                  FUN = '/')
    
    # Compute the loadings for the selected model size
    loadings.list <- list(
        P = mclapply(X = 1:ntable, mc.cores = mc.cores, FUN = function(i) {
            lpi <- tcrossprod(
                x = crossprod(x = data[[i]], 
                              y = modelCV$koplsModel$scoresP[, 1:maxPcomp,
                                                        drop=F]),
                y = diag(diag(
                    crossprod(modelCV$koplsModel$scoresP[, 1:maxPcomp,
                                                         drop=F]))^(-1),
                    ncol=maxPcomp))
            colnames(lpi) <- paste0("p", 1:maxPcomp)
            return (lpi)
        }),
        O = mclapply(X = 1:ntable, mc.cores = mc.cores, FUN = function(i) {
            loi <- tcrossprod(
                x = crossprod(x = data[[i]],
                              y = modelCV$koplsModel$scoresO[, 1:nOcompOpt,
                                                        drop=F]),
                y = diag(diag(
                    crossprod(modelCV$koplsModel$scoresO[, 1:nOcompOpt,
                                                         drop=F]))^(-1),
                    ncol=nOcompOpt))
            colnames(loi) <- paste0("o", 1:nOcompOpt)
            return (loi)
        }))
    loadings <- mclapply(X = 1:ntable, mc.cores = mc.cores, FUN = function(i) { 
        li <- cbind(loadings.list$P[[i]], loadings.list$O[[i]])
        colnames(li) <- c(paste0("p_", 1:maxPcomp),
                          paste0("o_", 1:nOcompOpt))
        return (li)
    })
    names(loadings) <- names(data)
    
    # Add kernelParams to the model object
    modelCV$kernelParams <- kernelParams
    # Add RV coefficients to the model object
    modelCV$RV <- unlist(RV)
    # Add normalized kernels to the model object
    modelCV$normKernels <- AMat
    # Add the loadings to the model object
    modelCV$loadings <- loadings
    # Add the scores to the model object
    modelCV$scores <- cbind(modelCV$koplsModel$scoresP,
                            modelCV$koplsModel$scoresO)
    # Add the initial data to the model object
    modelCV$data <- data

    class(modelCV) <- "RVConsensusOPLS"
    
    return (modelCV)
}
