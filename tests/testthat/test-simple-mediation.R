# Values of the simple mediation SVO -> EXP -> BEH, checked against independent
# fits: lm() for the coefficients (OLS and ML coincide for these path models)
# and lavaan::sem() for the standard errors.

test_that("indirect, component, direct and total effects equal the regression products", {
    results <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                       mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"))
    a <- stats::coef(stats::lm(EXP ~ SVO, coopmedmod))[["SVO"]]
    b <- stats::coef(stats::lm(BEH ~ EXP + SVO, coopmedmod))[["EXP"]]
    direct <- stats::coef(stats::lm(BEH ~ EXP + SVO, coopmedmod))[["SVO"]]
    total <- stats::coef(stats::lm(BEH ~ SVO, coopmedmod))[["SVO"]]
    expect_equal(effect_row(results, "Indirect", "SVO ⇒ EXP ⇒ BEH")$est, a * b, tolerance = 1e-6)
    expect_equal(effect_row(results, "Component", "SVO ⇒ EXP")$est, a, tolerance = 1e-6)
    expect_equal(effect_row(results, "Component", "EXP ⇒ BEH")$est, b, tolerance = 1e-6)
    expect_equal(effect_row(results, "Direct", "SVO ⇒ BEH")$est, direct, tolerance = 1e-6)
    expect_equal(effect_row(results, "Total", "SVO ⇒ BEH")$est, total, tolerance = 1e-6)
    expect_equal(effect_row(results, "Total", "SVO ⇒ BEH")$est, direct + a * b, tolerance = 1e-6)
})

test_that("standard errors equal an independent lavaan fit", {
    results <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                       mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"))
    fit <- lavaan::sem("EXP ~ a*SVO
                        BEH ~ b*EXP + c*SVO
                        indirect := a*b
                        total := c + a*b", data = coopmedmod, likelihood = "wishart")
    estimates <- lavaan::parameterEstimates(fit)
    se <- function(label) estimates$se[estimates$label == label]
    expect_equal(effect_row(results, "Indirect", "SVO ⇒ EXP ⇒ BEH")$se, se("indirect"), tolerance = 1e-6)
    expect_equal(effect_row(results, "Component", "SVO ⇒ EXP")$se, se("a"), tolerance = 1e-6)
    expect_equal(effect_row(results, "Direct", "SVO ⇒ BEH")$se, se("c"), tolerance = 1e-6)
    expect_equal(effect_row(results, "Total", "SVO ⇒ BEH")$se, se("total"), tolerance = 1e-6)
})

test_that("z, p and standard confidence intervals are consistent with estimate and SE", {
    results <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                       mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"),
                       ciWidth = 90)
    table <- results$models$coefficients$asDF
    expect_equal(table$z, table$est / table$se, tolerance = 1e-6)
    expect_equal(table$pvalue, 2 * stats::pnorm(-abs(table$z)), tolerance = 1e-6)
    critical <- stats::qnorm(0.95)
    expect_equal(table$ci.lower, table$est - critical * table$se, tolerance = 1e-6)
    expect_equal(table$ci.upper, table$est + critical * table$se, tolerance = 1e-6)
})

test_that("beta coefficients equal the coefficients of standardized variables", {
    results <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                       mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"))
    standardized <- as.data.frame(scale(coopmedmod[, c("SVO", "EXP", "BEH")]))
    a <- stats::coef(stats::lm(EXP ~ SVO, standardized))[["SVO"]]
    b <- stats::coef(stats::lm(BEH ~ EXP + SVO, standardized))[["EXP"]]
    expect_equal(effect_row(results, "Component", "SVO ⇒ EXP")$std.all, a, tolerance = 1e-6)
    expect_equal(effect_row(results, "Component", "EXP ⇒ BEH")$std.all, b, tolerance = 1e-6)
    expect_equal(effect_row(results, "Indirect", "SVO ⇒ EXP ⇒ BEH")$std.all, a * b, tolerance = 1e-6)
})

test_that("R-squared equals the regression R-squared of each equation", {
    results <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                       mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"))
    expect_equal(results$models$r2$asDF$r2,
                 c(summary(stats::lm(EXP ~ SVO, coopmedmod))$r.squared,
                   summary(stats::lm(BEH ~ EXP + SVO, coopmedmod))$r.squared),
                 tolerance = 1e-6)
})

test_that("R-squared confidence intervals contain the estimate and stay within 0 and 1", {
    # EXP has an R-squared of about 0.0002, whose interval must include it.
    results <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                       mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"))
    values <- results$models$r2$asDF
    expect_true(all(values$ci.lower >= 0 & values$ci.upper <= 1))
    expect_true(all(values$ci.lower <= values$r2 & values$r2 <= values$ci.upper),
                info = paste(utils::capture.output(print(values)), collapse = "\n"))
})

test_that("effect labels use one spacing convention for every effect type", {
    results <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                       mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"))
    effects <- results$models$coefficients$asDF$effect
    expect_identical(effects, gsub("\\s*⇒\\s*", " ⇒ ", effects))
})
