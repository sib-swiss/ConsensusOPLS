% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ConsensusOPLS.R
\docType{class}
\name{ConsensusOPLS-class}
\alias{ConsensusOPLS-class}
\title{\code{ConsensusOPLS} S4 class}
\description{
An object returned by the \code{ConsensusOPLS} function,
of class \code{ConsensusOPLS}, and representing a fitted Consensus OPLS
model.
}
\section{Slots}{

\describe{
\item{\code{modelType}}{The type of requested OPLS regression model.}

\item{\code{response}}{The provided response variable (Y).}

\item{\code{nPcomp}}{Number of Y-predictive components (latent variables) of the 
optimal model.}

\item{\code{nOcomp}}{Number of Y-orthogonal components (latent variables) of the 
optimal model.}

\item{\code{blockContribution}}{Relative contribution of each block (normalized 
\code{lambda} values) to the latent variables.}

\item{\code{scores}}{Representation of the samples in the latent variables of the 
optimal model.}

\item{\code{loadings}}{Contribution of each block's variables to the latent
variables of the optimal model.}

\item{\code{VIP}}{Variable importance in projection (VIP) for each block of data,
assessing the relevance of the variables in explaining the variation in the
response.}

\item{\code{R2X}}{Proportion of variation in data blocks explained by the optimal
model.}

\item{\code{R2Y}}{Proportion of variation in the response explained by the optimal
model.}

\item{\code{Q2}}{Predictive ability of the optimal model.}

\item{\code{DQ2}}{Predictive ability of the optimal discriminant model.}

\item{\code{permStats}}{Assessment of models with permuted response.}

\item{\code{model}}{The optimal model.}

\item{\code{cv}}{Cross-validation result towards the optimal model. Contains 
\code{AllYhat} (all predicted Y values as a concatenated matrix), 
\code{cvTestIndex} (indexes for the test set observations during the 
cross-validation rounds), \code{DQ2Yhat} (total discriminant Q-square result
for all Y-orthogonal components), \code{nOcompOpt} (optimal number of 
Y-orthogonal components (latent variables) for the optimal model), and 
\code{Q2Yhat} (total Q-square result for all Y-orthogonal components).}
}}

