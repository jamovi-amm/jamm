# Models that lavaan cannot make converge.
#
# Bug: when the fit does not converge, jammGLM() reports the results as if it
# had: the info table says "Converged FALSE" and the effects table is filled
# with meaningless values (all zeros in the example below), with no error.
#
# Expected behaviour (what these tests check): the analysis stops with an error
# saying that the model did not converge, as it already does for models that
# cannot be estimated at all ("Model cannot be estimated: ...").
#
# Example model: SVO2 is exactly twice SVO, so their separate effects cannot be
# estimated and lavaan does not converge.
library(testthat)
test_that("the example model really does not converge in lavaan", {
    # Checks the trigger itself, independently of jamm: if this fails, the
    # example no longer reproduces a non-converging fit.
    dat <- coopmedmod
    dat$SVO2 <- dat$SVO * 2
    fit <- suppressWarnings(lavaan::sem("EXP ~ SVO + SVO2
                                         BEH ~ EXP + SVO + SVO2", data = dat))
    expect_false(lavaan::lavInspect(fit, "converged"))
})

test_that("jammGLM() stops with a clear error when the model does not converge", {
    dat <- coopmedmod
    dat$SVO2 <- dat$SVO * 2
    expect_error(
        utils::capture.output(
            jammGLM(data = dat, dep = "BEH", mediators = "EXP", covs = c("SVO", "SVO2"),
                    mediatorsTerms = list(list("SVO", "SVO2")),
                    modelTerms = list("EXP", "SVO", "SVO2"))),
        "[Cc]onverge")
})



test_that("a model that converges reports it and gives results", {
    # Guards against the fix rejecting models that are fine.
    results <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                       mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"))
    info <- results$infotab$asDF
    expect_identical(info$value[info$info %in% "Converged"], "TRUE")
    expect_false(anyNA(results$models$coefficients$asDF$est))
})
