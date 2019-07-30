library(testthat)

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
