% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NewDataConversion.R
\name{convertToCyclopsData}
\alias{convertToCyclopsData}
\alias{convertToCyclopsData.ffdf}
\alias{convertToCyclopsData.data.frame}
\title{Convert data from two data frames or ffdf objects into a CyclopsData object}
\usage{
convertToCyclopsData(outcomes, covariates, modelType = "lr",
  addIntercept = TRUE, checkSorting = TRUE, checkRowIds = TRUE,
  normalize = NULL, quiet = FALSE, floatingPoint = 64)

\method{convertToCyclopsData}{ffdf}(outcomes, covariates,
  modelType = "lr", addIntercept = TRUE, checkSorting = TRUE,
  checkRowIds = TRUE, normalize = NULL, quiet = FALSE,
  floatingPoint = 64)

\method{convertToCyclopsData}{data.frame}(outcomes, covariates,
  modelType = "lr", addIntercept = TRUE, checkSorting = TRUE,
  checkRowIds = TRUE, normalize = NULL, quiet = FALSE,
  floatingPoint = 64)
}
\arguments{
\item{outcomes}{A data frame or ffdf object containing the outcomes with predefined columns (see below).}

\item{covariates}{A data frame or ffdf object containing the covariates with predefined columns (see below).}

\item{modelType}{Cyclops model type. Current supported types are "pr", "cpr", lr", "clr", or "cox"}

\item{addIntercept}{Add an intercept to the model?}

\item{checkSorting}{Check if the data are sorted appropriately, and if not, sort.}

\item{checkRowIds}{Check if all rowIds in the covariates appear in the outcomes.}

\item{normalize}{String: Name of normalization for all non-indicator covariates (possible values: stdev, max, median)}

\item{quiet}{If true, (warning) messages are surpressed.}

\item{floatingPoint}{Specified floating-point representation size (32 or 64)}
}
\value{
An object of type cyclopsData
}
\description{
\code{convertToCyclopsData} loads data from two data frames or ffdf objects, and inserts it into a Cyclops data object.
}
\details{
These columns are expected in the outcome object:
\tabular{lll}{
  \verb{stratumId}    \tab(integer) \tab (optional) Stratum ID for conditional regression models \cr
  \verb{rowId}  	\tab(integer) \tab Row ID is used to link multiple covariates (x) to a single outcome (y) \cr
  \verb{y}    \tab(real) \tab The outcome variable \cr
  \verb{time}    \tab(real) \tab For models that use time (e.g. Poisson or Cox regression) this contains time \cr
                 \tab        \tab(e.g. number of days) \cr
  \verb{weight} \tab(real) \tab Non-negative weight to apply to outcome
}

These columns are expected in the covariates object:
\tabular{lll}{
  \verb{stratumId}    \tab(integer) \tab (optional) Stratum ID for conditional regression models \cr
  \verb{rowId}  	\tab(integer) \tab Row ID is used to link multiple covariates (x) to a single outcome (y) \cr
  \verb{covariateId}    \tab(integer) \tab A numeric identifier of a covariate  \cr
  \verb{covariateValue}    \tab(real) \tab The value of the specified covariate \cr
}

Note: If checkSorting is turned off, the outcome table should be sorted by stratumId (if present)
and then rowId except for Cox regression when the table should be sorted by
stratumId (if present), -time, y, and rowId. The covariate table should be sorted by covariateId, stratumId
(if present), rowId except for Cox regression when the table should be sorted by covariateId,
stratumId (if present), -time, y, and rowId.
}
\section{Methods (by class)}{
\itemize{
\item \code{ffdf}: Convert data from two \code{ffdf}

\item \code{data.frame}: Convert data from two \code{data.frame}
}}

\examples{
#Convert infert dataset to Cyclops format:
covariates <- data.frame(stratumId = rep(infert$stratum, 2),
                         rowId = rep(1:nrow(infert), 2),
                         covariateId = rep(1:2, each = nrow(infert)),
                         covariateValue = c(infert$spontaneous, infert$induced))
outcomes <- data.frame(stratumId = infert$stratum,
                       rowId = 1:nrow(infert),
                       y = infert$case)
#Make sparse:
covariates <- covariates[covariates$covariateValue != 0, ]

#Create Cyclops data object:
cyclopsData <- convertToCyclopsData(outcomes, covariates, modelType = "clr",
                                    addIntercept = FALSE)

#Fit model:
fit <- fitCyclopsModel(cyclopsData, prior = createPrior("none"))

}
