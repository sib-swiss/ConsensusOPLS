#' @title \code{ConsensusOPLS} S4 class
#' @description An object returned by the \code{ConsensusOPLS} function,
#' of class \code{ConsensusOPLS}, and representing a fitted Consensus OPLS
#' model.
#' @slot modelType The type of requested OPLS regression model.
#' @slot response The provided response variable (Y).
#' @slot nPcomp Number of Y-predictive components (latent variables) of the 
#' optimal model.
#' @slot nOcomp Number of Y-orthogonal components (latent variables) of the 
#' optimal model.
#' @slot blockContribution Relative contribution of each block (normalized 
#' \code{lambda} values) to the latent variables.
#' @slot scores Representation of the samples in the latent variables of the 
#' optimal model.
#' @slot loadings Contribution of each block's variables to the latent
#' variables of the optimal model.
#' @slot VIP Variable importance in projection (VIP) for each block of data,
#' assessing the relevance of the variables in explaining the variation in the
#' response.
#' @slot R2X Proportion of variation in data blocks explained by the optimal
#' model.
#' @slot R2Y Proportion of variation in the response explained by the optimal
#' model.
#' @slot Q2 Predictive ability of the optimal model.
#' @slot DQ2 Predictive ability of the optimal discriminant model.
#' @slot permStats Assessment of models with permuted response.
#' @slot model The optimal model.
#' @slot cv Cross-validation result towards the optimal model. Contains 
#' \code{AllYhat} (all predicted Y values as a concatenated matrix), 
#' \code{cvTestIndex} (indexes for the test set observations during the 
#' cross-validation rounds), \code{DQ2Yhat} (total discriminant Q-square result
#' for all Y-orthogonal components), \code{nOcompOpt} (optimal number of 
#' Y-orthogonal components (latent variables) for the optimal model), and 
#' \code{Q2Yhat} (total Q-square result for all Y-orthogonal components).
#' @name ConsensusOPLS-class
#' @rdname ConsensusOPLS-class
#' @import methods utils
#' @export
#' 
setClass("ConsensusOPLS",
         slots = list(
             modelType         = "character",
             response          = "vector",
             nPcomp            = "numeric",
             nOcomp            = "numeric",
             blockContribution = "matrix",
             scores            = "matrix",
             loadings          = "list",
             VIP               = "list",
             R2X               = "numeric",
             R2Y               = "numeric",
             Q2                = "numeric",
             DQ2               = "numeric",
             permStats         = "list",
             model             = "list",
             cv                = "list")
)


#' @noRd
#' 
setMethod(
    f = "show", signature = "ConsensusOPLS",
    definition = function(object) {
        cat("***Optimal Consensus OPLS model***\n")
        cat("\nMode: ", object@modelType, "\n")
        cat("\nNumber of predictive components: ", object@nPcomp, "\n")
        cat("\nNumber of orthogonal components: ", object@nOcomp, "\n")
        cat("\nBlock contribution:\n")
        print(object@blockContribution)
        cat("\nExplained variance R2 in response:",
            object@R2Y[length(object@R2Y)], "\n")
        cat("\nPredictive ability (cross validation Q2): ",
            object@Q2[length(object@Q2)], "\n")
    }
)


#' @title Block contribution plot
#' @param object An object of class \code{ConsensusOPLS}.
#' @param col A vector of color codes or names, one for each block. Default,
#' NULL, 2 to number of blocks + 1.
#' @param ... \code{barplot} arguments.
#' @import graphics
#' @export
#' @docType methods
#' @rdname plotContribution
#'  
setGeneric(
    name = "plotContribution",
    def = function(object,
                   col = NULL,
                   ...) {
        standardGeneric("plotContribution")
    }
)


#' @title Block contribution plot
#' @description
#' Plot of relative contribution of each data block in the optimal model.
#' @param object An object of class \code{ConsensusOPLS}.
#' @param col A vector of color codes or names, one for each block. Default,
#' NULL, 2 to number of blocks + 1.
#' @param ... \code{barplot} arguments.
#' @returns No return value, called for side effects.
#' @import graphics
#' @importFrom reshape2 melt
#' @export
#' @docType methods
#' @rdname plotContribution
#'  
setMethod(
    f = "plotContribution",
    signature = "ConsensusOPLS",
    definition = function(object,
                          col = NULL,
                          ...) {
        contributions <- reshape2::melt(object@blockContribution)
        colnames(contributions) <- c("Block", "Component", "Contribution")
        if (is.null(col)) col <- 1:nrow(object@blockContribution) + 1
        
        # restore standard options
        oldpar <- par(no.readonly=TRUE)
        on.exit(par(oldpar))
        
        par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
        barplot(data=contributions,
                Contribution ~ Block + Component, beside=T,
                col=col,
                legend.text=T, 
                args.legend=list(x="topright", inset=c(-0.3, 0), 
                                 title="Block",
                                 cex=0.8, bty='n'),
                ...)
    }
)


#' @title Score plot
#' @param object An object of class \code{ConsensusOPLS}.
#' @param comp1 Latent variable for abscissa. Default, the first predictive
#' component, \code{p_1}.
#' @param comp2 Latent variable for ordinate. Default, the first orthogonal
#' component, \code{o_1}.
#' @param col A vector of color codes or names. Default, NULL, generated
#' following the \code{response}.
#' @param pch Graphic symbol. Default, 19.
#' @param ... \code{plot} arguments.
#' @import graphics grDevices
#' @export
#' @rdname plotScores
#'  
setGeneric(
    name = "plotScores",
    def = function(object,
                   comp1 = "p_1",
                   comp2 = "o_1",
                   col = NULL,
                   pch = 19,
                   ...) {
        standardGeneric("plotScores")
    }
)


#' @title Score plot
#' @description 
#' Plot of samples in the space of latent variables of the optimal model.
#' @param object An object of class \code{ConsensusOPLS}.
#' @param comp1 Latent variable for abscissa. Default, the first predictive
#' component, \code{p_1}.
#' @param comp2 Latent variable for ordinate. Default, the first orthogonal
#' component, \code{o_1}.
#' @param col A vector of color codes or names. Default, NULL, generated
#' following the \code{response}.
#' @param pch Graphic symbol. Default, 19.
#' @param ... \code{plot} arguments.
#' @returns No return value, called for side effects.
#' @import graphics grDevices
#' @export
#' @rdname plotScores
#'  
setMethod(
    f = "plotScores",
    signature = "ConsensusOPLS",
    definition = function(object, 
                          comp1 = "p_1",
                          comp2 = "o_1",
                          col = NULL,
                          pch = 19,
                          ...) {
        stopifnot(comp1 %in% colnames(object@scores) && 
                      comp2 %in% colnames(object@scores))
        if (is.null(col)) {
            if (object@modelType=='da') {
                response <- factor(
                    as.character(object@response),
                    levels=sort(unique(as.character(object@response))))
                col <- c(1:nlevels(response)+1)[response]
                
                # restore standard options
                oldpar <- par(no.readonly=TRUE)
                on.exit(par(oldpar))
                
                par(mar=c(5.1, 4.1, 4.1, 6.1), xpd=TRUE)
                plot(object@scores[, c(comp1, comp2)], col=col, pch=pch, ...)
                legend('topright', inset=c(-0.3, 0), legend=levels(response),
                       col=c(1:nlevels(response)+1), pch=pch,
                       title="Response", cex=0.8, bty='n')
            } else {
                col.breaks <- cut(object@response, breaks=10)
                val.breaks <- as.numeric(unique(
                    gsub("\\(|\\]", "", unlist(strsplit(levels(col.breaks),
                                                        split=",")))))
                rbPal <- colorRampPalette(c('blue', "ivory"))
                col <- rbPal(10)[as.numeric(col.breaks)]
                
                layout(matrix(1:2, ncol=2), widths=c(3,1), heights=c(1,1))
                plot(object@scores[, c(comp1, comp2)], col=col, pch=pch, ...)
                legend_image <- as.raster(matrix(sort(col), ncol=1))
                plot(x=c(0,2.2), y=c(0,1), type='n', axes=F,
                     xlab='', ylab='', main="Response", cex.main=0.9)
                text(x=1.7,
                     y=seq(from=0.7, to=1, length.out=3),
                     labels=round(seq(from=min(val.breaks), to=max(val.breaks),
                                      length.out=3), 0),
                     cex=0.8)
                rasterImage(legend_image, 0, 0.7, 1.2, 1)
            }
        } else {
            plot(object@scores[, c(comp1, comp2)], col=col, pch=pch, ...)
        }
    }
)


#' @title Loading plot
#' @param object An object of class \code{ConsensusOPLS}.
#' @param comp1 Latent variable for X-axis. Default, the first predictive
#' component, \code{p_1}.
#' @param comp2 Latent variable for Y-axis. Default, the first orthogonal
#' component, \code{o_1}.
#' @param blockId The positions or names of the blocks for the plot.
#' Default, NULL, all.
#' @param col A vector of color codes or names, one for each block. Default,
#' NULL, 2 to \code{length(blockId)+1}.
#' @param pch A vector of graphic symbols, one for each block. Default, NULL,
#' 1 to \code{length(blockId)}.
#' @param ... \code{plot} arguments.
#' @import graphics
#' @export
#' @rdname plotLoadings
#'  
setGeneric(
    name = "plotLoadings",
    def = function(object,
                   comp1 = "p_1",
                   comp2 = "o_1",
                   blockId = NULL,
                   col = NULL,
                   pch = NULL,
                   ...) {
        standardGeneric("plotLoadings")
    }
)


#' @title Loading plot
#' @description Plot of variable loadings in the optimal model.
#' @param object An object of class \code{ConsensusOPLS}.
#' @param comp1 Latent variable for X-axis. Default, the first predictive
#' component, \code{p_1}.
#' @param comp2 Latent variable for Y-axis. Default, the first orthogonal
#' component, \code{o_1}.
#' @param blockId The positions or names of the blocks for the plot.
#' Default, NULL, all.
#' @param col A vector of color codes or names, one for each block. Default,
#' NULL, 2 to \code{length(blockId)+1}.
#' @param pch A vector of graphic symbols, one for each block. Default, NULL,
#' 1 to \code{length(blockId)}.
#' @param ... \code{plot} arguments.
#' @returns No return value, called for side effects.
#' @import graphics
#' @export
#' @rdname plotLoadings
#'  
setMethod(
    f = "plotLoadings",
    signature = "ConsensusOPLS",
    definition = function(object, 
                          comp1 = "p_1",
                          comp2 = "o_1",
                          blockId = NULL,
                          col = NULL,
                          pch = NULL,
                          ...) {
        stopifnot(comp1 %in% colnames(object@scores) && 
                      comp2 %in% colnames(object@scores))
        
        if (is.null(blockId)) blockId <- names(object@loadings)
        if (is.null(col)) col <- 1:length(blockId) + 1
        if (is.null(pch)) pch <- 1:length(blockId)
        
        loadings <- do.call(rbind.data.frame, object@loadings[blockId])
        
        # restore standard options
        oldpar <- par(no.readonly=TRUE)
        on.exit(par(oldpar))
        
        par(mar=c(5.1, 4.1, 4.1, 6.1), xpd=TRUE)
        plot(loadings[, c(comp1, comp2)], 
             col=col[factor(unlist(lapply(blockId, function(x) 
                 rep(x, nrow(object@loadings[[x]])))), levels=blockId)],
             pch=pch[factor(unlist(lapply(blockId, function(x) 
                 rep(x, nrow(object@loadings[[x]])))), levels=blockId)],
             ...)
        legend("topright", inset=c(-0.3, 0), 
               legend=names(object@loadings[blockId]),
               col=col, pch=pch, title="Block",
               cex=0.8, bty='n')
    }
)


#' @title VIP plot
#' @param object An object of class \code{ConsensusOPLS}.
#' @param comp1 Latent variable for loadings on Y-axis. Default, the first
#' predictive component, \code{p_1}.
#' @param comp2 Latent variable for VIPs on X-axis. Default, the predictive
#' component, \code{p}.
#' @param blockId The positions or names of the blocks for the plot.
#' Default, NULL, all.
#' @param col A vector of color codes or names, one for each block. Default,
#' NULL, 2 to \code{length(blockId)+1}.
#' @param pch A vector of graphic symbols, one for each block. Default, NULL,
#' 1 to \code{length(blockId)}.
#' @param xlab X-axis label. Default, NULL, Loading on \code{comp}.
#' @param ylab Y-axis label. Default, NULL, VIP on \code{comp}.
#' @param ... \code{plot} arguments.
#' @import graphics
#' @export
#' @rdname plotVIP
#'  
setGeneric(
    name = "plotVIP",
    def = function(object,
                   comp1 = "p_1",
                   comp2 = "p",
                   blockId = NULL,
                   col = NULL,
                   pch = NULL,
                   xlab = NULL,
                   ylab = NULL,
                   ...) {
        standardGeneric("plotVIP")
    }
)


#' @title VIP plot
#' @description Plot of VIP versus variable loadings in the optimal model.
#' @param object An object of class \code{ConsensusOPLS}.
#' @param comp1 Latent variable for loadings on Y-axis. Default, the first
#' predictive component, \code{p_1}.
#' @param comp2 Latent variable for VIPs on X-axis. Default, the predictive
#' component, \code{p}.
#' @param blockId The positions or names of the blocks for the plot.
#' Default, NULL, all.
#' @param col A vector of color codes or names, one for each block. Default,
#' NULL, 2 to \code{length(blockId)+1}.
#' @param pch A vector of graphic symbols, one for each block. Default, NULL,
#' 1 to \code{length(blockId)}.
#' @param xlab X-axis label. Default, NULL, Loading on \code{comp}.
#' @param ylab Y-axis label. Default, NULL, VIP on \code{comp}.
#' @param ... \code{plot} arguments.
#' @returns No return value, called for side effects.
#' @import graphics
#' @export
#' @rdname plotVIP
#'  
setMethod(
    f = "plotVIP",
    signature = "ConsensusOPLS",
    definition = function(object, 
                          comp1 = "p_1",
                          comp2 = "p",
                          blockId = NULL,
                          col = NULL,
                          pch = NULL,
                          xlab = NULL,
                          ylab = NULL,
                          ...) {
        stopifnot(comp1 %in% colnames(object@scores) && 
                      comp2 %in% c("p", "o", "tot"))
        
        if (is.null(blockId)) blockId <- names(object@loadings)
        if (is.null(col)) col <- 1:length(blockId) + 1
        if (is.null(pch)) pch <- 1:length(blockId)
        
        loadings <- do.call(rbind.data.frame, object@loadings[blockId])
        loadings <- loadings[, comp1, drop=F]
        VIPs <- do.call(rbind.data.frame, object@VIP[blockId])
        VIPs <- VIPs[, comp2, drop=F]
        loadings_VIP <- cbind.data.frame(loadings, VIPs[rownames(loadings),])
        colnames(loadings_VIP) <- c("loadings", "VIP")
        
        # restore standard options
        oldpar <- par(no.readonly=TRUE)
        on.exit(par(oldpar))
        
        par(mar=c(5.1, 4.1, 4.1, 6.1), xpd=TRUE)
        plot(loadings_VIP, 
             col=col[factor(unlist(lapply(blockId, function(x) 
                 rep(x, nrow(object@loadings[[x]])))), levels=blockId)],
             pch=pch[factor(unlist(lapply(blockId, function(x)
                 rep(x, nrow(object@loadings[[x]])))), levels=blockId)],
             xlab=if (is.null(xlab)) paste0("Loadings on ", comp1) else xlab,
             ylab=if (is.null(ylab)) paste0("VIP on ", comp2) else ylab,
             ...)
        legend("topright", inset=c(-0.3, 0), 
               legend=names(object@loadings[blockId]),
               col=col, pch=pch, title="Block",
               cex=0.8, bty='n')
    }
)


#' @title Q2 plot
#' @param object An object of class \code{ConsensusOPLS}.
#' @param breaks See \code{hist}.
#' @param xlab See \code{hist}.
#' @param main See \code{hist}.
#' @param col A color code or name for Q2 in the optimal model. Default, 2.
#' See \code{abline}.
#' @param lty A line type code or name for Q2 in the optimal model. Default, 2.
#' See \code{abline}.
#' @param ... \code{hist} arguments.
#' @import graphics
#' @export
#' @rdname plotQ2
#'  
setGeneric(
    name = "plotQ2",
    def = function(object,
                   breaks = 10,
                   xlab = "Q2",
                   main = "Q2 in models with permuted response",
                   col = "blue",
                   lty = "dashed",
                   ...) {
        standardGeneric("plotQ2")
    }
)


#' @title Q2 plot
#' @description Plot of Q2 of models with permuted response.
#' @param object An object of class \code{ConsensusOPLS}.
#' @param breaks See \code{hist}.
#' @param xlab See \code{hist}.
#' @param main See \code{hist}.
#' @param col A color code or name for Q2 in the optimal model. Default, 2.
#' See \code{abline}.
#' @param lty A line type code or name for Q2 in the optimal model. Default, 2.
#' See \code{abline}.
#' @param ... \code{hist} arguments.
#' @returns No return value, called for side effects.
#' @import graphics
#' @export
#' @rdname plotQ2
#'  
setMethod(
    f = "plotQ2",
    signature = "ConsensusOPLS",
    definition = function(object,
                          breaks = 10,
                          xlab = "Q2",
                          main = "Q2 in models with permuted response",
                          col = "blue",
                          lty = "dashed",
                          ...) {
        d <- density(object@permStats$Q2Y)
        h <- hist(object@permStats$Q2Y, breaks=breaks, plot=F)
        hist(object@permStats$Q2Y, breaks=breaks, probability=T,
             xlab=xlab, ylim=c(min(d$y), max(d$y, h$density)), main=main, ...)
        lines(d)
        abline(v=object@permStats$Q2Y[1], col=col, lty=lty, xpd=F)
    }
)


#' @title DQ2 plot
#' @param object An object of class \code{ConsensusOPLS}.
#' @param breaks See \code{hist}.
#' @param xlab See \code{hist}.
#' @param main See \code{hist}.
#' @param col A color code or name for DQ2 in the optimal model. Default, 2.
#' See \code{abline}.
#' @param lty A line type code or name for DQ2 in the optimal model. Default, 2.
#' See \code{abline}.
#' @param ... \code{hist} arguments.
#' @import graphics
#' @export
#' @rdname plotDQ2
#'  
setGeneric(
    name = "plotDQ2",
    def = function(object,
                   breaks = 10,
                   xlab = "DQ2",
                   main = "DQ2 in models with permuted response",
                   col = "blue",
                   lty = "dashed",
                   ...) {
        standardGeneric("plotDQ2")
    }
)


#' @title DQ2 plot
#' @description Plot of DQ2 of models with permuted response.
#' @param object An object of class \code{ConsensusOPLS}.
#' @param breaks See \code{hist}.
#' @param xlab See \code{hist}.
#' @param main See \code{hist}.
#' @param col A color code or name for DQ2 in the optimal model. Default, 2.
#' See \code{abline}.
#' @param lty A line type code or name for DQ2 in the optimal model. Default, 2.
#' See \code{abline}.
#' @param ... \code{hist} arguments.
#' @returns No return value, called for side effects.
#' @import graphics
#' @export
#' @rdname plotDQ2
#'  
setMethod(
    f = "plotDQ2",
    signature = "ConsensusOPLS",
    definition = function(object,
                          breaks = 10,
                          xlab = "DQ2",
                          main = "DQ2 in models with permuted response",
                          col = "blue",
                          lty = "dashed",
                          ...) {
        stopifnot(length(object@permStats$DQ2Y) > 0)
        
        d <- density(object@permStats$DQ2Y)
        h <- hist(object@permStats$DQ2Y, breaks=breaks, plot=F)
        hist(object@permStats$DQ2Y, breaks=breaks, probability=T,
             xlab=xlab, ylim=c(min(d$y), max(d$y, h$density)), main=main, ...)
        lines(d)
        abline(v=object@permStats$DQ2Y[1], col=col, lty=lty, xpd=F)
    }
)


#' @title R2 plot
#' @param object An object of class \code{ConsensusOPLS}.
#' @param breaks See \code{hist}.
#' @param xlab See \code{hist}.
#' @param main See \code{hist}.
#' @param col A color code or name for R2 in the optimal model. Default, 2.
#' See \code{abline}.
#' @param lty A line type code or name for R2 in the optimal model. Default, 2.
#' See \code{abline}.
#' @param ... \code{hist} arguments.
#' @import graphics
#' @export
#' @rdname plotR2
#'  
setGeneric(
    name = "plotR2",
    def = function(object,
                   breaks = 10,
                   xlab = "R2",
                   main = "R2 in models with permuted response",
                   col = "blue",
                   lty = "dashed",
                   ...) {
        standardGeneric("plotR2")
    }
)


#' @title R2 plot
#' @description Plot of R2 of models with permuted response.
#' @param object An object of class \code{ConsensusOPLS}.
#' @param breaks See \code{hist}.
#' @param xlab See \code{hist}.
#' @param main See \code{hist}.
#' @param col A color code or name for R2 in the optimal model. Default, 2.
#' See \code{abline}.
#' @param lty A line type code or name for R2 in the optimal model. Default, 2.
#' See \code{abline}.
#' @param ... \code{hist} arguments.
#' @returns No return value, called for side effects.
#' @import graphics
#' @export
#' @rdname plotR2
#'  
setMethod(
    f = "plotR2",
    signature = "ConsensusOPLS",
    definition = function(object,
                          breaks = 10,
                          xlab = "R2",
                          main = "R2 in models with permuted response",
                          col = "blue",
                          lty = "dashed",
                          ...) {
        d <- density(object@permStats$R2Y)
        h <- hist(object@permStats$R2Y, breaks=breaks, plot=F)
        hist(object@permStats$R2Y, breaks=breaks, probability=T,
             xlab=xlab, ylim=c(min(d$y), max(d$y, h$density)), main=main, ...)
        lines(d)
        abline(v=object@permStats$R2Y[1], col=col, lty=lty, xpd=F)
    }
)


#' @title Model prediction
#' @param object An object of class \code{ConsensusOPLS}.
#' @param newdata A list of data frames of new data to predict. If omitted, the
#' fitted values in \code{object} are returned.
#' @param nOcomp Number of Y-orthogonal components to consider. Default, NULL,
#' the number of Y-orthogonal components in the optimal model.
#' @export
#' @rdname predict
#'  
setGeneric(
    name = "predict",
    def = function(object,
                   newdata = NULL,
                   nOcomp = NULL) {
        standardGeneric("predict")
    }
)


#' @title Model prediction
#' @description
#' Predicts the response on new data with a fitted model.
#' @param object An object of class \code{ConsensusOPLS}.
#' @param newdata A list of data frames of new data to predict. If omitted, the
#' fitted values in \code{object} are returned.
#' @param nOcomp Number of Y-orthogonal components to consider. Default, NULL,
#' the number of Y-orthogonal components in the optimal model.
#' @export
#' @rdname predict
#'  
setMethod(
    f = "predict",
    signature = "ConsensusOPLS",
    definition = function(object,
                          newdata = NULL,
                          nOcomp = NULL) {
        if (is.null(nOcomp)) nOcomp <- object@nOcomp
        
        Ktrain <- Reduce(
            '+',
            lapply(names(object@model$RV), function(tab)
                object@model$RV[tab] * object@model$normKernels[[tab]]))
        
        if (is.null(newdata)) {
            Ktest <- KteTr <- Ktrain
        } else {
            Kte <- lapply(names(newdata), function(tab) {
                KteTe <- koplsKernel(X1 = newdata[[tab]],
                                     X2 = NULL,
                                     type = object@model$kernelParams$type,
                                     params = object@model$kernelParams$params)
                KteTe <- KteTe/norm(KteTe, type='F')
                
                KteTr <- koplsKernel(X1 = newdata[[tab]],
                                     X2 = object@model$data[[tab]],
                                     type = object@model$kernelParams$type,
                                     params = object@model$kernelParams$params)
                KteTr <- KteTr/norm(KteTr, type='F')
                
                return (list(KteTe = KteTe,
                             KteTr = KteTr))
            })
            names(Kte) <- names(newdata)
            Ktest <- Reduce('+',
                            lapply(names(newdata), function(tab)
                                object@model$RV[tab] * Kte[[tab]]$KteTe))
            KteTr <- Reduce('+',
                            lapply(names(newdata), function(tab)
                                object@model$RV[tab] * Kte[[tab]]$KteTr))
        }
        
        ## koplsPredict will center KteTr and Ktest according to
        ## model$params$preProcK while doing nothing on Ktrain
        newPred <- koplsPredict(KteTr = KteTr, Ktest = Ktest, Ktrain = Ktrain,
                                model = object@model$koplsModel, nox = nOcomp,
                                rescaleY = FALSE)
        if (object@modelType=='da') {
            classPred <- data.frame(
                class = koplsMaxClassify(newPred$Yhat),
                # margin between the max and second max
                margin = apply(newPred$Yhat, 1, function(x) {
                    -diff(sort(x, decreasing = T))[1]
                }),
                # softmax probability
                softmax = t(apply(newPred$Yhat, 1, function(x) {
                    x[x<0] <- 0
                    x[x>1] <- 1
                    x <- x/norm(x, "2")
                    exp_x <- exp(-1/x)
                    return (exp_x/sum(exp_x))
                }))
            )
            # normalize margin into [0, 1]
            #classPred$margin <- classPred$margin/max(classPred$margin)
        } else classPred <- NA
        
        return (list(Y=newPred$Yhat,
                     class=classPred))
    }
)


#' @title ConsensusOPLS
#' @description
#' Constructs the consensus OPLS model with the optimal number of orthogonal 
#' components for given data blocks and response, and evaluate the model 
#' quality w.r.t other models built with randomly permuted responses.
#' 
#' @param data A list of data blocks. Each element of the list must be of matrix 
#' type. Rows and columns can be identified (names), in which case this will be 
#' retained during analysis. Any pre-processing of the data (e.g. scaling) must 
#' be carried out before building the model.
#' @param Y A vector, factor, dummy matrix or numeric matrix for the response. 
#' The type of answer given will condition the model to be used: a numeric 
#' vector for linear regression, a factor or dummy matrix for logistic 
#' regression or a discriminant model.
#' @param maxPcomp Maximum number of Y-predictive components used to build the 
#' optimal model. Default, 1.
#' @param maxOcomp Maximum number of Y-orthogonal components used to build the 
#' optimal model. Default, 5.
#' @param modelType String for type of OPLS regression model, either \code{reg} 
#' for regression or \code{da} for discriminant analysis. Default, \code{da}.
#' @param nperm Number of random permutations desired in response Y. Default, 
#' 100.
#' @param cvType String for type of cross-validation used. Either \code{nfold} 
#' for n-fold cross-validation, where \code{nfold} is look up, or \code{mccv} 
#' for Monte Carlo cross-validation, or \code{mccvb} for Monte Carlo 
#' class-balanced cross-validation, where \code{nMC} and \code{cvFrac} are used.
#' Default, \code{nfold}, i.e. \code{nMC} and \code{cvFrac} are ignored.
#' @param nfold Number of folds performed in n-fold cross-validation. This can 
#' be set to the number of samples to perform Leave-One-Out cross validation. 
#' Default, 5.
#' @param nMC An integer indicating the number of rounds performed when 
#' \code{cvType} is \code{mccv} or \code{mccvb}. Default, 100.
#' @param cvFrac A numeric value indicating the fraction of observations from 
#' \code{data} used in the training set for \code{mccv} or \code{mccvb} 
#' cross-validation. Default, 4/5 = 0.8.
#' @param kernelParams List of parameters for the kernel. Either \code{p}
#' for polynomial kernel, which implies specifying the order of the polynomial 
#' by the \code{order} parameter, or \code{g} for Gaussian kernel. Default, 
#' \code{list(type='p', params = c(order=1.0))}.
#' @param mc.cores Number of cores for parallel computing. Default, 1.
#' @param verbose A logical indicating if detailed information (cross
#' validation) will be shown. Default, FALSE.
#'
#' @returns An object of class \code{ConsensusOPLS} representing the consensus
#' OPLS model fit.
#' @examples
#' data(demo_3_Omics)
#' datablocks <- lapply(
#'     demo_3_Omics[c("MetaboData", "MicroData", "ProteoData")], scale)
#' res <- ConsensusOPLS(data=datablocks, 
#'                      Y=demo_3_Omics$Y,
#'                      maxPcomp=1, maxOcomp=2, 
#'                      modelType='da',
#'                      nperm=5)
#' res
#' @importFrom reshape2 melt
#' @import parallel 
#' @export
#' 
ConsensusOPLS <- function(data,
                          Y,
                          maxPcomp = 1,
                          maxOcomp = 5,
                          modelType = 'da',
                          nperm = 100,
                          cvType = 'nfold',
                          nfold = 5,
                          nMC = 100,
                          cvFrac = 4/5,
                          kernelParams = list(type='p', params = c(order=1.0)),
                          mc.cores = 1,
                          verbose = FALSE) {
    # Variable format control
    if (!is.list(data))
        stop("data is not a list.")
    if (!is.matrix(Y) && !is.vector(Y) && !is.factor(Y))
        stop("Y is not either matrix, vector or factor.")
    if (nperm != as.integer(nperm))
        stop("nperm is not an integer.")
    if (maxPcomp != as.integer(maxPcomp))
        stop("maxPcomp is not an integer")
    if (maxOcomp != as.integer(maxOcomp))
        stop("maxOcomp is not an integer")
    
    if (modelType == "reg") {
        if (is.factor(Y)) {
            warning("Logistic regression is performed.")
            if (nlevels(Y) > 2) stop("Y should have two levels.")
            Y <- koplsDummy(as.vector(Y))[, 1, drop=F]
        } else {
            Y <- as.matrix(Y)
        }
        if (ncol(Y) > 1 || !is.numeric(Y))
            stop("modelType is preferably `da`.")
    } else {
        if (nlevels(as.factor(Y)) > length(Y)/2) {
            # TODO: something better than this check: if at least 2 samples belong to every class
            stop("modelType is preferably `reg`.")
        }
        if (is.vector(Y) || is.factor(Y) || ncol(Y) == 1) {
            Y <- koplsDummy(as.vector(Y))
        }
    }
    
    # Set maximum number of predictive components to the number of classes - 1
    maxPcomp <- min(maxPcomp, max(ncol(Y)-1, 1))
    
    # Set colnames of Y
    if (is.matrix(Y) && is.null(colnames(Y)))
        colnames(Y) <- 1:ncol(Y)
    
    # Random permutation of Y rows
    Ypermid <- cbind(
        1:nrow(Y), 
        unlist(replicate(nperm, sample(x = 1:nrow(Y), size = nrow(Y),
                                       replace = FALSE, prob = NULL))))
    Ylist <- lapply(1:(1+nperm), function(i) {
        Y[Ypermid[, i], , drop=F]
    })
    
    # Init parameters
    permStats <- list()
    # Loaded packages
    loaded.package.names <- c(sessionInfo()$basePkgs,
                              names(sessionInfo()$otherPkgs))
    # Create parallel cluster
    globLibPaths <- .libPaths()
    cl <- makeCluster(mc.cores)
    clusterExport(cl, ls(name=1, all.names=TRUE))
    # Load the packages on all the cluster
    parLapply(cl, 1:length(cl), function(i) {
        lapply(loaded.package.names, function(lpn) {
            .libPaths(globLibPaths)
            library(lpn, character.only=TRUE)
        })
    })
    
    # Permutations
    perms <- parLapply(cl, X=1:(1+nperm), function(i) {
        # Permuted response
        Ys <- Ylist[[i]]
        
        # Redo the Consensus OPLS-DA with RV coefficients weighting
        modelCV <- RVConsensusOPLS(data = data,
                                   Y = Ys,
                                   maxPcomp = maxPcomp,
                                   maxOcomp = maxOcomp,
                                   modelType = modelType,
                                   cvType = cvType,
                                   nfold = nfold,
                                   nMC = nMC,
                                   cvFrac = cvFrac,
                                   mc.cores = 1,
                                   kernelParams = kernelParams,
                                   verbose = verbose)
        VIP <- VIP(data = data, Y = Ys, model=modelCV$koplsModel)
        
        return (list(Ys=Ys,
                     modelCV=modelCV,
                     VIP=VIP)
        )
    })
    permStats$nOcomp  <- unlist(parLapply(cl, X=1:(1+nperm), function(i) {
        perms[[i]]$modelCV$koplsModel$params$nOcomp
    }))
    permStats$R2Yhat  <- unlist(parLapply(cl, X=1:(1+nperm), function(i) {
        perms[[i]]$modelCV$koplsModel$R2Yhat[permStats$nOcomp[i]+1]
    }))
    permStats$DQ2Yhat <- unlist(parLapply(cl, X=1:(1+nperm), function(i) {
        perms[[i]]$modelCV$cv$DQ2Yhat[permStats$nOcomp[i]+1]
    }))
    permStats$Q2Yhat  <- unlist(parLapply(cl, X=1:(1+nperm), function(i) {
        perms[[i]]$modelCV$cv$Q2Yhat[permStats$nOcomp[i]+1]
    }))
    permStats$Y       <- unlist(parLapply(cl, X=1:(1+nperm), function(i) {
        perms[[i]]$Ys
    }))
    permStats$RV      <- unlist(parLapply(cl, X=1:(1+nperm), function(i) {
        RVmodified(X=Y, Y=perms[[i]]$Ys)
    }))
    if (!verbose) {
        permStats$VIP <- list()
        permStats$loadings <- list()
    } else {
        # VIP of permuted model
        permVIPs <- lapply(1:length(data), function(j) {
            permVIPj.list <- parLapply(cl, X=1:(1+nperm), function(i) {
                perms[[i]]$VIP[[j]]
            })
            permVIPj.array <- array(unlist(permVIPj.list),
                                    dim = c(nrow(permVIPj.list[[1]]),
                                            ncol(permVIPj.list[[1]]),
                                            1+nperm),
                                    dimnames = list(
                                        rownames(permVIPj.list[[1]]),
                                        colnames(permVIPj.list[[1]]),
                                        paste0('perm', 0:nperm)))
            return (permVIPj.array)
        })
        names(permVIPs) <- names(data)
        permStats$VIP <- permVIPs
        
        # loadings of permuted model
        permLoadings <- lapply(1:length(data), function(j) {
            permLoadingj.list <- parLapply(cl, X=1:(1+nperm), function(i) {
                perms[[i]]$modelCV$loadings[[j]]
            })
            permLoadingj.array <- array(unlist(permLoadingj.list),
                                    dim = c(nrow(permLoadingj.list[[1]]),
                                            ncol(permLoadingj.list[[1]]),
                                            1+nperm),
                                    dimnames = list(
                                        rownames(permLoadingj.list[[1]]),
                                        colnames(permLoadingj.list[[1]]),
                                        paste0('perm', 0:nperm)))
            return (permLoadingj.array)
        })
        names(permLoadings) <- names(data)
        permStats$loadings <- permLoadings
    }
    
    ## Stop parallel clusters
    stopCluster(cl)
    
    ## Result ConsensusOPLS object
    resObj <- new(
        "ConsensusOPLS",
        modelType         = modelType,
        response          = if (modelType=='da') koplsReDummy(Y) else
            as.vector(Y),
        nPcomp            = perms[[1]]$modelCV$koplsModel$params$nPcomp,
        nOcomp            = perms[[1]]$modelCV$koplsModel$params$nOcomp,
        blockContribution = perms[[1]]$modelCV$koplsModel$blockContribution,
        scores            = perms[[1]]$modelCV$scores,
        loadings          = perms[[1]]$modelCV$loadings,
        VIP               = perms[[1]]$VIP,
        R2X               = perms[[1]]$modelCV$koplsModel$R2X,
        R2Y               = perms[[1]]$modelCV$koplsModel$R2Yhat,
        Q2                = perms[[1]]$modelCV$cv$Q2Yhat[
            1:(perms[[1]]$modelCV$koplsModel$params$nOcomp+1)],
        DQ2               = if (modelType=='da') 
            perms[[1]]$modelCV$cv$DQ2Yhat[
                1:(perms[[1]]$modelCV$koplsModel$params$nOcomp+1)] else
                    numeric(),
        permStats         = permStats,
        model             = perms[[1]]$modelCV[setdiff(
            names(perms[[1]]$modelCV),
            c("scores", "loadings", "cv"))],
        cv                = if (verbose) perms[[1]]$modelCV$cv else list())
    
    return (resObj)
}
