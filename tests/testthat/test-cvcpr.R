library(Cyclops)
library(testthat)
library(survival)

test_that("Check conditional Poisson with cross-validation",{
    set.seed(123)
    sim <- simulateCyclopsData(nstrata=1000,
                               ncovars=10,
                               nrows=10000,
                               effectSizeSd=0.5,
                               eCovarsPerRow=2,
                               model="poisson")
    cyclopsData <- convertToCyclopsData(outcomes = sim$outcomes,
                                        covariates = sim$covariates,
                                        modelType = "cpr")
    fit <- fitCyclopsModel(cyclopsData = cyclopsData,
                           prior = createPrior("laplace",
                                               useCrossValidation = TRUE,
                                               exclude = 1),
                           control = createControl(fold = 10,
                                                   cvRepetitions = 1,
                                                   startingVariance = 0.1,
                                                   noiseLevel = "quiet",
                                                   seed = 123))
})

# broken
test_that("Check predictive log likelihood",{
    test <- read.table(header=T, sep = ",", text = "
                       start, length, event, x1, x2
                       0, 4,  1,0,0
                       0, 3,  1,2,0
                       0, 3,  0,0,1
                       0, 2,  1,0,1
                       0, 2,  1,1,1
                       0, 1,  0,1,0
                       0, 1,  1,1,0")

    gold <- coxph(Surv(length, event) ~ x1 + strata(x2), test, ties = "breslow")

    # Double the data
    test2 <- rbind(data.frame(test, index = 1:7), data.frame(test, index = 1:7))
    test2 <- test2[order(test2$index),]

    data <- createCyclopsData(Surv(length, event) ~ x1 + strata(x2), data = test2,
                              modelType = "cox")

    # Fit with the second set
    weights = rep(c(0,1), 7)
    fit <- fitCyclopsModel(data, weights = weights)

    tolerance <- 1E-4
    expect_equal(coef(fit), coef(gold), tolerance = tolerance)
    expect_equivalent(logLik(fit), logLik(gold))

    # Get predictive log likelihood of first set
    pred <- Cyclops:::.cyclopsGetNewPredictiveLogLikelihood(fit$interface, weights = 1 - weights)
    expect_equal(pred, as.numeric(logLik(gold)), tolerance)
})
