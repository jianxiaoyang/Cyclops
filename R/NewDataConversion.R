# @file NewDataConversion.R
#
# Copyright 2014 Observational Health Data Sciences and Informatics
#
# This file is part of cyclops
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Convert data from two data frames or ffdf objects into a CyclopsData object
#'
#' @description
#' \code{convertToCyclopsData} loads data from two data frames or ffdf objects, and inserts it into a Cyclops data object.
#'
#' @param outcomes      A data frame or ffdf object containing the outcomes with predefined columns (see below).
#' @param covariates    A data frame or ffdf object containing the covariates with predefined columns (see below).
#' @param modelType     Cyclops model type. Current supported types are "pr", "cpr", lr", "clr", or "cox"
#' @param addIntercept  Add an intercept to the model?
#' @param checkSorting  Check if the data are sorted appropriately, and if not, sort.
#' @param checkRowIds   Check if all rowIds in the covariates appear in the outcomes.
#' @param quiet         If true, (warning) messages are surpressed.
#'
#' @details
#' These columns are expected in the outcome object:
#' \tabular{lll}{
#'   \verb{stratumId}    \tab(integer) \tab (optional) Stratum ID for conditional regression models \cr
#'   \verb{rowId}  	\tab(integer) \tab Row ID is used to link multiple covariates (x) to a single outcome (y) \cr
#'   \verb{y}    \tab(real) \tab The outcome variable \cr
#'   \verb{time}    \tab(real) \tab For models that use time (e.g. Poisson or Cox regression) this contains time \cr
#'                  \tab        \tab(e.g. number of days) \cr
#' }
#'
#' These columns are expected in the covariates object:
#' \tabular{lll}{
#'   \verb{stratumId}    \tab(integer) \tab (optional) Stratum ID for conditional regression models \cr
#'   \verb{rowId}  	\tab(integer) \tab Row ID is used to link multiple covariates (x) to a single outcome (y) \cr
#'   \verb{covariateId}    \tab(integer) \tab A numeric identifier of a covariate  \cr
#'   \verb{covariateValue}    \tab(real) \tab The value of the specified covariate \cr
#' }
#'
#' Note: If checkSorting is turned off, the outcome table should be sorted by stratumId (if present)
#' and then rowId except for Cox regression when the table should be sorted by
#' stratumId (if present), -time, and rowId. The covariate table should be sorted by covariateId, stratumId
#' (if present), rowId except for Cox regression when the table should be sorted by covariateId,
#' stratumId (if present), -time, and rowId.
#'
#' @return
#' An object of type cyclopsData
#'
#' @examples
#' #Convert infert dataset to Cyclops format:
#' covariates <- data.frame(stratumId = rep(infert$stratum, 2),
#'                          rowId = rep(1:nrow(infert), 2),
#'                          covariateId = rep(1:2, each = nrow(infert)),
#'                          covariateValue = c(infert$spontaneous, infert$induced))
#' outcomes <- data.frame(stratumId = infert$stratum,
#'                        rowId = 1:nrow(infert),
#'                        y = infert$case)
#' #Make sparse:
#' covariates <- covariates[covariates$covariateValue != 0, ]
#'
#' #Create Cyclops data object:
#' cyclopsData <- newConvertToCyclopsData(outcomes, covariates, modelType = "clr",
#'                                     addIntercept = FALSE)
#'
#' #Fit model:
#' fit <- fitCyclopsModel(cyclopsData, prior = createPrior("none"))
#'
#' @export
newConvertToCyclopsData <- function(outcomes,
                                    covariates,
                                    modelType = "lr",
                                    addIntercept = TRUE,
                                    checkSorting = TRUE,
                                    checkRowIds = TRUE,
                                    quiet = FALSE) {
    UseMethod("newConvertToCyclopsData")
}

#' @describeIn newConvertToCyclopsData Convert data from two \code{ffdf}
#' @export
newConvertToCyclopsData.ffdf <- function(outcomes,
                                         covariates,
                                         modelType = "lr",
                                         addIntercept = TRUE,
                                         checkSorting = TRUE,
                                         checkRowIds = TRUE,
                                         quiet = FALSE){
    if ((modelType == "clr" | modelType == "cpr") & addIntercept){
        if(!quiet)
            warning("Intercepts are not allowed in conditional models, removing intercept",call.=FALSE)
        addIntercept = FALSE
    }
    if (modelType == "pr" | modelType == "cpr")
        if (any(outcomes$time <= 0))
            stop("time cannot be non-positive",call.=FALSE)

    if (modelType == "cox"){
        if (is.null(outcomes$stratumId)){
            # This does not work without adding ffbase to search path:
            # outcomes$stratumId = 0
            # covariates$stratumId = 0
            # So we do:
            outcomes$stratumId <- ff::ff(vmode="double", length=nrow(outcomes))
            for (i in bit::chunk(outcomes$stratumId)){
                outcomes$stratumId[i] <- 0
            }
            covariates$stratumId <- ff::ff(vmode="double", length=nrow(covariates))
            for (i in bit::chunk(covariates$stratumId)){
                covariates$stratumId[i] <- 0
            }
        }
    }

    if (checkSorting){
        if (modelType == "lr" | modelType == "pr"){
            if (!isSorted(outcomes,c("rowId"))){
                if(!quiet)
                    writeLines("Sorting outcomes by rowId")
                rownames(covariates) <- NULL #Needs to be null or the ordering of ffdf will fail
                outcomes <- outcomes[ff::ffdforder(outcomes[c("rowId")]),]
            }
            if (!isSorted(covariates,c("covariateId","rowId"))){
                if(!quiet)
                    writeLines("Sorting covariates by covariateId, rowId")
                rownames(covariates) <- NULL #Needs to be null or the ordering of ffdf will fail
                covariates <- covariates[ff::ffdforder(covariates[c("covariateId","rowId")]),]
            }
        }
        if (modelType == "clr" | modelType == "cpr"){
            if (!isSorted(outcomes,c("stratumId","rowId"))){
                if(!quiet)
                    writeLines("Sorting outcomes by stratumId and rowId")
                rownames(outcomes) <- NULL #Needs to be null or the ordering of ffdf will fail
                outcomes <- outcomes[ff::ffdforder(outcomes[c("stratumId","rowId")]),]
            }
            if (!isSorted(covariates,c("covariateId", "stratumId","rowId"))){
                if(!quiet)
                    writeLines("Sorting covariates by covariateId, stratumId and rowId")
                rownames(covariates) <- NULL #Needs to be null or the ordering of ffdf will fail
                covariates <- covariates[ff::ffdforder(covariates[c("stratumId","rowId")]),]
            }
        }
        if (modelType == "cox"){
            outcomes$minTime <- ff::ff(vmode="double", length=length(outcomes$time))
            for (i in bit::chunk(outcomes$time)){
                outcomes$minTime[i] <- 0-outcomes$time[i]
            }
            if (!isSorted(outcomes,c("stratumId", "time", "rowId"),c(TRUE, FALSE,TRUE))){
                if(!quiet)
                    writeLines("Sorting outcomes by stratumId, time (descending) and rowId")
                rownames(outcomes) <- NULL #Needs to be null or the ordering of ffdf will fail
                outcomes <- outcomes[ff::ffdforder(outcomes[c("stratumId","minTime","rowId")]),]
            }
            if (is.null(covariates$minTime)){ # If time not present, add to check if sorted
                covariates$minTime <- NULL
                covariates$time <- NULL
                covariates$y <- NULL
                covariates <- merge(covariates, outcomes, by = c("stratumId", "rowId"))
            }
            if (!isSorted(covariates, c("covariateId", "stratumId", "time", "rowId"), c(TRUE, TRUE, FALSE, TRUE))){
                if(!quiet)
                    writeLines("Sorting covariates by covariateId, stratumId, time (descending), and rowId")
                rownames(covariates) <- NULL #Needs to be null or the ordering of ffdf will fail
                covariates <- covariates[ff::ffdforder(covariates[c("covariateId", "stratumId", "minTime", "rowId")]),]
            }
        }
    }
    if (checkRowIds){
        mapped <- ffbase::ffmatch(x = covariates$rowId, table=outcomes$rowId, nomatch = 0L) > 0L
        minValue <- min(sapply(bit::chunk(mapped), function(i) {
            min(mapped[i])
        }))
        if (minValue == 0){
            if(!quiet)
                writeLines("Removing covariate values with rowIds that are not in outcomes")
            row.names(covariates) <- NULL #Needed or else next line fails
            covariates <- covariates[ffbase::ffwhich(mapped, mapped == TRUE),]
        }
    }

    dataPtr <- createSqlCyclopsData(modelType = modelType)

    loadNewSqlCyclopsDataY(dataPtr,
                           if (is.null(outcomes$stratumId) | modelType == "lr" | modelType == "pr") {NULL} else {ff::as.ram.ff(outcomes$stratumId)},
                           ff::as.ram.ff(outcomes$rowId),
                           ff::as.ram.ff(outcomes$y),
                           if (is.null(outcomes$time)) {NULL} else {ff::as.ram.ff(outcomes$time)})

    if (addIntercept & modelType != "cox")
        loadNewSqlCyclopsDataX(dataPtr, 0, NULL, NULL, name = "(Intercept)")

    for (i in bit::chunk(covariates)){
        covariatesChunk <- covariates[i,]
        covarNames <- unique(covariatesChunk$covariateId)
        loadNewSeqlCyclopsDataMultipleX(dataPtr,
                                        covariatesChunk$covariateId,
                                        covariatesChunk$rowId,
                                        covariatesChunk$covariateValue,
                                        name = covarNames,
                                        append = TRUE)
    }
    if (modelType == "pr" || modelType == "cpr")
        finalizeSqlCyclopsData(dataPtr, useOffsetCovariate = -1)
    return(dataPtr)

}

#' @describeIn newConvertToCyclopsData Convert data from two \code{data.frame}
#' @export
newConvertToCyclopsData.data.frame <- function(outcomes,
                                               covariates,
                                               modelType = "lr",
                                               addIntercept = TRUE,
                                               checkSorting = TRUE,
                                               checkRowIds = TRUE,
                                               quiet = FALSE){
    if ((modelType == "clr" | modelType == "cpr") & addIntercept){
        if(!quiet)
            warning("Intercepts are not allowed in conditional models, removing intercept",call.=FALSE)
        addIntercept = FALSE
    }
    if (modelType == "pr" | modelType == "cpr")
        if (any(outcomes$time <= 0))
            stop("time cannot be non-positive",call.=FALSE)

    if (modelType == "lr" | modelType == "pr"){
        outcomes$stratumId <- NULL
        covariates$stratumId <- NULL
    }
    if (modelType == "cox" & is.null(outcomes$stratumId)){
        outcomes$stratumId <- 0
    }

    if (checkSorting){
        if (modelType == "lr" | modelType == "pr"){
            if (!isSorted(outcomes,c("rowId"))){
                if(!quiet)
                    writeLines("Sorting outcomes by rowId")
                outcomes <- outcomes[order(outcomes$rowId),]
            }
            if (!isSorted(covariates,c("covariateId", "rowId"))){
                if(!quiet)
                    writeLines("Sorting covariates by covariateId and rowId")
                covariates <- covariates[order(covariates$covariateId, covariates$rowId),]
            }
        }

        if (modelType == "clr" | modelType == "cpr"){
            if (!isSorted(outcomes,c("stratumId","rowId"))){
                if(!quiet)
                    writeLines("Sorting outcomes by stratumId and rowId")
                outcomes <- outcomes[order(outcomes$stratumId,outcomes$rowId),]
            }
            if (!isSorted(covariates,c("covariateId", "stratumId","rowId"))){
                if(!quiet)
                    writeLines("Sorting covariates by covariateId, stratumId, and rowId")
                covariates <- covariates[order(covariates$covariateId, covariates$stratumId,covariates$rowId),]
            }
        }
        if (modelType == "cox"){
            if (!isSorted(outcomes,c("stratumId", "time", "rowId"),c(TRUE, FALSE,TRUE))){
                if(!quiet)
                    writeLines("Sorting outcomes by stratumId, time (descending) and rowId")
                outcomes <- outcomes[order(outcomes$stratumId, -outcomes$time, outcomes$rowId),]
            }
            if (is.null(covariates$time)) {
                covariates$time <- NULL
                covariates$y <- NULL
                covariates$stratumId <- NULL
                covariates <- merge(covariates, outcomes, by = c("rowId"))
            }
            if (!isSorted(covariates, c("covariateId", "stratumId", "time", "rowId"), c(TRUE, TRUE, FALSE, TRUE))){
                if(!quiet)
                    writeLines("Sorting covariates by covariateId, stratumId, time (descending), and rowId")
                covariates <- covariates[order(covariates$covariateId, covariates$stratumId, -covariates$time, covariates$rowId),]
            }
        }
    }
    if (checkRowIds){
        mapping <- match(covariates$rowId,outcomes$rowId)
        if (any(is.na(mapping))){
            if(!quiet)
                writeLines("Removing covariate values with rowIds that are not in outcomes")
            covariateRowsWithMapping <- which(!is.na(mapping))
            covariates <- covariates[covariateRowsWithMapping,]
        }
    }
    dataPtr <- createSqlCyclopsData(modelType = modelType)

    loadNewSqlCyclopsDataY(dataPtr, outcomes$stratumId, outcomes$rowId, outcomes$y, outcomes$time)

    if (addIntercept & modelType != "cox")
        loadNewSqlCyclopsDataX(dataPtr, 0, NULL, NULL, name = "(Intercept)")

    covarNames <- unique(covariates$covariateId)
    loadNewSeqlCyclopsDataMultipleX(dataPtr, covariates$covariateId, covariates$rowId, covariates$covariateValue, name = covarNames)

    if (modelType == "pr" || modelType == "cpr")
        finalizeSqlCyclopsData(dataPtr, useOffsetCovariate = -1)

    return(dataPtr)
}