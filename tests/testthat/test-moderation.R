# Moderated mediation: SVO moderates both paths (and the direct path) of
# cprime -> EXP -> BEH. Conditional effects are checked against lm() slopes
# computed on the centered variables (jamovi's default scaling).

test_that("conditional effects at mean and plus or minus 1 SD equal the lm slopes", {
    results <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = c("cprime", "SVO"),
                       mediatorsTerms = list(list("cprime", "SVO", c("cprime", "SVO"))),
                       modelTerms = list("EXP", "cprime", "SVO", c("EXP", "SVO"), c("cprime", "SVO")),
                       mod_terms = list(list("SVO")))
    centered <- coopmedmod
    centered$cprime <- centered$cprime - mean(centered$cprime)
    centered$SVO <- centered$SVO - mean(centered$SVO)
    centered$EXP <- centered$EXP - mean(centered$EXP)
    a <- stats::coef(stats::lm(EXP ~ cprime * SVO, centered))
    b <- stats::coef(stats::lm(BEH ~ EXP + cprime + SVO + EXP:SVO + cprime:SVO, centered))
    s <- stats::sd(centered$SVO)
    settings <- list("SVO = Mean - 1 SD" = -s, "SVO = Mean" = 0, "SVO = Mean + 1 SD" = s)
    for (label in names(settings)) {
        w <- settings[[label]]
        aPath <- a[["cprime"]] + a[["cprime:SVO"]] * w
        bPath <- b[["EXP"]] + b[["EXP:SVO"]] * w
        direct <- b[["cprime"]] + b[["cprime:SVO"]] * w
        expect_equal(effect_row(results, "Indirect", "cprime ⇒ EXP ⇒ BEH", label)$est,
                     aPath * bPath, tolerance = 1e-6, info = label)
        expect_equal(effect_row(results, "Component", "cprime ⇒ EXP", label)$est,
                     aPath, tolerance = 1e-6, info = label)
        expect_equal(effect_row(results, "Component", "EXP ⇒ BEH", label)$est,
                     bPath, tolerance = 1e-6, info = label)
        expect_equal(effect_row(results, "Direct", "cprime ⇒ BEH", label)$est,
                     direct, tolerance = 1e-6, info = label)
        expect_equal(effect_row(results, "Total", "cprime ⇒ BEH", label)$est,
                     direct + aPath * bPath, tolerance = 1e-6, info = label)
    }
})

test_that("moderated output shows the condition column and the interactions table", {
    results <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = c("cprime", "SVO"),
                       mediatorsTerms = list(list("cprime", "SVO", c("cprime", "SVO"))),
                       modelTerms = list("EXP", "cprime", "SVO", c("EXP", "SVO"), c("cprime", "SVO")),
                       mod_terms = list(list("SVO")))
    expect_true(results$models$coefficients$getColumn("condition")$visible)
    expect_true(results$models$interactions$visible)
    centered <- coopmedmod
    centered$cprime <- centered$cprime - mean(centered$cprime)
    centered$SVO <- centered$SVO - mean(centered$SVO)
    centered$EXP <- centered$EXP - mean(centered$EXP)
    a <- stats::coef(stats::lm(EXP ~ cprime * SVO, centered))
    b <- stats::coef(stats::lm(BEH ~ EXP + cprime + SVO + EXP:SVO + cprime:SVO, centered))
    interactions <- results$models$interactions$asDF
    expect_identical(interactions$lhs, c("EXP", "BEH", "BEH"))
    expect_identical(interactions$rhs, c("cprime:SVO", "EXP:SVO", "cprime:SVO"))
    expect_equal(interactions$est, c(a[["cprime:SVO"]], b[["EXP:SVO"]], b[["cprime:SVO"]]),
                 tolerance = 1e-6)
})

test_that("moderators' main effects are left out of the effects table", {
    results <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = c("cprime", "SVO"),
                       mediatorsTerms = list(list("cprime", "SVO", c("cprime", "SVO"))),
                       modelTerms = list("EXP", "cprime", "SVO", c("EXP", "SVO"), c("cprime", "SVO")),
                       mod_terms = list(list("SVO")))
    expect_false(any(grepl("^SVO", results$models$coefficients$asDF$effect)))
})

test_that("a categorical moderator conditions on each level and on their average", {
    # In coopmedmod, cprime is a coding of prime, so SVO is the predictor here.
    dat <- coopmedmod
    dat$prime <- factor(dat$prime, levels = c(0, 1), labels = c("neutral", "coop"))
    results <- jammGLM(data = dat, dep = "BEH", mediators = "EXP", covs = "SVO", factors = "prime",
                       mediatorsTerms = list(list("SVO", "prime", c("SVO", "prime"))),
                       modelTerms = list("EXP", "SVO", "prime", c("EXP", "prime"), c("SVO", "prime")),
                       mod_terms = list(list("prime")))
    expect_identical(unique(results$models$coefficients$asDF$condition),
                     c("prime = neutral", "prime = coop", "prime = Average"))
    # Within one level, the conditional slope is the slope in that group.
    coop <- dat[dat$prime == "coop", ]
    expect_equal(effect_row(results, "Component", "SVO ⇒ EXP", "prime = coop")$est,
                 stats::coef(stats::lm(EXP ~ SVO, coop))[["SVO"]], tolerance = 1e-6)
})

test_that("moderated untested unsaturated model shows the direct effect fixed to 0 in every condition", {
    results <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = c("cprime", "SVO"),
                       mediatorsTerms = list(list("cprime", "SVO", c("cprime", "SVO"))),
                       modelTerms = list("EXP", "SVO", c("EXP", "SVO")),
                       mod_terms = list(list("SVO")))
    for (label in c("SVO = Mean - 1 SD", "SVO = Mean", "SVO = Mean + 1 SD")) {
        direct <- effect_row(results, "Direct", "cprime ⇒ BEH", label)
        expect_identical(direct$est, 0, info = label)
        expect_true(all(is.na(direct[, c("se", "ci.lower", "ci.upper", "z", "pvalue")])), info = label)
        indirect <- effect_row(results, "Indirect", "cprime ⇒ EXP ⇒ BEH", label)
        total <- effect_row(results, "Total", "cprime ⇒ BEH", label)
        expect_equal(total$est, indirect$est, tolerance = 1e-6, info = label)
        expect_equal(total$se, indirect$se, tolerance = 1e-6, info = label)
    }
})
