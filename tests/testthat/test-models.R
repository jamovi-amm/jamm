# Different models and kinds of variables: several independent variables,
# parallel and serial mediators, factors, ordinal variables, formula input.

test_that("two independent variables each get their own indirect, direct and total effect", {
    results <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = c("cprime", "SVO"),
                       mediatorsTerms = list(list("cprime", "SVO")),
                       modelTerms = list("EXP", "cprime", "SVO"))
    a <- stats::coef(stats::lm(EXP ~ cprime + SVO, coopmedmod))
    b <- stats::coef(stats::lm(BEH ~ EXP + cprime + SVO, coopmedmod))
    total <- stats::coef(stats::lm(BEH ~ cprime + SVO, coopmedmod))
    for (iv in c("cprime", "SVO")) {
        expect_equal(effect_row(results, "Indirect", paste(iv, "⇒ EXP ⇒ BEH"))$est,
                     a[[iv]] * b[["EXP"]], tolerance = 1e-6)
        expect_equal(effect_row(results, "Direct", paste(iv, "⇒ BEH"))$est, b[[iv]], tolerance = 1e-6)
        expect_equal(effect_row(results, "Total", paste(iv, "⇒ BEH"))$est, total[[iv]], tolerance = 1e-6)
    }
    expect_identical(results$models$r2$asDF$lhs, c("EXP", "BEH"))
})

test_that("parallel mediators get one indirect effect each and the total adds them up", {
    # A second mediator, partly caused by EXP.
    dat <- coopmedmod
    set.seed(20261118)
    dat$M2 <- 0.5 * dat$EXP + stats::rnorm(nrow(dat), sd = 5)
    results <- jammGLM(data = dat, dep = "BEH", mediators = c("EXP", "M2"), covs = "SVO",
                       mediatorsTerms = list(list("SVO"), list("SVO")),
                       modelTerms = list("EXP", "M2", "SVO"))
    aEXP <- stats::coef(stats::lm(EXP ~ SVO, dat))[["SVO"]]
    aM2 <- stats::coef(stats::lm(M2 ~ SVO, dat))[["SVO"]]
    b <- stats::coef(stats::lm(BEH ~ EXP + M2 + SVO, dat))
    expect_equal(effect_row(results, "Indirect", "SVO ⇒ EXP ⇒ BEH")$est, aEXP * b[["EXP"]], tolerance = 1e-6)
    expect_equal(effect_row(results, "Indirect", "SVO ⇒ M2 ⇒ BEH")$est, aM2 * b[["M2"]], tolerance = 1e-6)
    expect_equal(effect_row(results, "Total", "SVO ⇒ BEH")$est,
                 stats::coef(stats::lm(BEH ~ SVO, dat))[["SVO"]], tolerance = 1e-6)
    expect_identical(results$models$r2$asDF$lhs, c("EXP", "M2", "BEH"))
    expect_false(any(grepl("EXP ⇒ M2", results$models$coefficients$asDF$effect)))
})

test_that("serial mediators add the chained indirect effect", {
    dat <- coopmedmod
    set.seed(20261118)
    dat$M2 <- 0.5 * dat$EXP + stats::rnorm(nrow(dat), sd = 5)
    results <- jammGLM(data = dat, dep = "BEH", mediators = c("EXP", "M2"), covs = "SVO",
                       mediatorsTerms = list(list("SVO"), list("SVO", "EXP")),
                       modelTerms = list("EXP", "M2", "SVO"))
    a1 <- stats::coef(stats::lm(EXP ~ SVO, dat))
    a2 <- stats::coef(stats::lm(M2 ~ SVO + EXP, dat))
    b <- stats::coef(stats::lm(BEH ~ EXP + M2 + SVO, dat))
    expect_equal(effect_row(results, "Indirect", "SVO ⇒ EXP ⇒ M2 ⇒ BEH")$est,
                 a1[["SVO"]] * a2[["EXP"]] * b[["M2"]], tolerance = 1e-6)
    expect_equal(effect_row(results, "Indirect", "SVO ⇒ EXP ⇒ BEH")$est,
                 a1[["SVO"]] * b[["EXP"]], tolerance = 1e-6)
    expect_equal(effect_row(results, "Indirect", "SVO ⇒ M2 ⇒ BEH")$est,
                 a2[["SVO"]] * b[["M2"]], tolerance = 1e-6)
    effects <- results$models$coefficients$asDF
    indirect <- sum(effects$est[effects$type == "Indirect"])
    expect_equal(effect_row(results, "Total", "SVO ⇒ BEH")$est, b[["SVO"]] + indirect, tolerance = 1e-6)
})

test_that("a two-level factor gives one contrast whose effects match lm with the same coding", {
    dat <- coopmedmod
    dat$prime <- factor(dat$prime, levels = c(0, 1), labels = c("neutral", "coop"))
    results <- jammGLM(data = dat, dep = "BEH", mediators = "EXP", factors = "prime",
                       mediatorsTerms = list(list("prime")), modelTerms = list("EXP", "prime"))
    # jamm's default 'simple' coding: level 2 minus level 1, centered.
    coding <- list(prime = c(-0.5, 0.5))
    a <- stats::coef(stats::lm(EXP ~ prime, dat, contrasts = coding))[["prime1"]]
    b <- stats::coef(stats::lm(BEH ~ EXP + prime, dat, contrasts = coding))
    expect_equal(effect_row(results, "Indirect", "prime1 ⇒ EXP ⇒ BEH")$est, a * b[["EXP"]], tolerance = 1e-6)
    expect_equal(effect_row(results, "Direct", "prime1 ⇒ BEH")$est, b[["prime1"]], tolerance = 1e-6)
    expect_identical(results$models$contrastCodeTable$asDF$clab, "coop - neutral")
    expect_true(results$models$contrastCodeTable$visible)
})

test_that("a three-level factor gives one indirect effect per contrast", {
    dat <- coopmedmod
    dat$group <- factor(rep(c("a", "b", "c"), length.out = nrow(dat)))
    results <- jammGLM(data = dat, dep = "BEH", mediators = "EXP", factors = "group",
                       mediatorsTerms = list(list("group")), modelTerms = list("EXP", "group"))
    coding <- list(group = stats::contr.treatment(3) - 1 / 3)
    a <- stats::coef(stats::lm(EXP ~ group, dat, contrasts = coding))
    b <- stats::coef(stats::lm(BEH ~ EXP + group, dat, contrasts = coding))
    # lm names the contrast columns after the compared level (group2, group3);
    # jamm numbers them (group1, group2).
    expect_equal(effect_row(results, "Indirect", "group1 ⇒ EXP ⇒ BEH")$est,
                 a[["group2"]] * b[["EXP"]], tolerance = 1e-6)
    expect_equal(effect_row(results, "Indirect", "group2 ⇒ EXP ⇒ BEH")$est,
                 a[["group3"]] * b[["EXP"]], tolerance = 1e-6)
    expect_identical(results$models$contrastCodeTable$asDF$clab, c("b - a", "c - a"))
})

test_that("two factors and a covariate estimate together and match lm", {
    dat <- coopmedmod
    dat$prime <- factor(dat$prime, levels = c(0, 1), labels = c("neutral", "coop"))
    dat$group <- factor(rep(c("a", "b", "c"), length.out = nrow(dat)))
    results <- jammGLM(data = dat, dep = "BEH", mediators = "EXP", factors = c("prime", "group"),
                       covs = "SVO", mediatorsTerms = list(list("prime", "group", "SVO")),
                       modelTerms = list("EXP", "prime", "group", "SVO"))
    info <- results$infotab$asDF
    expect_identical(info$value[info$info %in% "Converged"], "TRUE")
    coding <- list(prime = c(-0.5, 0.5), group = stats::contr.treatment(3) - 1 / 3)
    a <- stats::coef(stats::lm(EXP ~ prime + group + SVO, dat, contrasts = coding))
    b <- stats::coef(stats::lm(BEH ~ EXP + prime + group + SVO, dat, contrasts = coding))
    expect_equal(effect_row(results, "Component", "prime1 ⇒ EXP")$est, a[["prime1"]], tolerance = 1e-6)
    expect_equal(effect_row(results, "Component", "group2 ⇒ EXP")$est, a[["group3"]], tolerance = 1e-6)
    expect_equal(effect_row(results, "Direct", "SVO ⇒ BEH")$est, b[["SVO"]], tolerance = 1e-6)
    effects <- results$models$coefficients$asDF
    expect_false(any(grepl("^group ⇒|^prime ⇒", effects$effect)))
    expect_equal(sum(effects$type == "Indirect"), 4)
})

test_that("full mediation shows the direct effect fixed to 0 and the total equal to the indirect", {
    results <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                       mediatorsTerms = list(list("SVO")), modelTerms = list("EXP"))
    a <- stats::coef(stats::lm(EXP ~ SVO, coopmedmod))[["SVO"]]
    b <- stats::coef(stats::lm(BEH ~ EXP, coopmedmod))[["EXP"]]
    indirect <- effect_row(results, "Indirect", "SVO ⇒ EXP ⇒ BEH")
    expect_equal(indirect$est, a * b, tolerance = 1e-6)
    # The direct path is not estimated: it is shown as 0, with no SE, interval or test.
    direct <- effect_row(results, "Direct", "SVO ⇒ BEH")
    expect_identical(direct$est, 0)
    expect_identical(direct$std.all, 0)
    expect_true(all(is.na(direct[, c("se", "ci.lower", "ci.upper", "z", "pvalue")])))
    notes <- vapply(results$models$coefficients$notes, function(note) note$note, character(1))
    expect_true(any(grepl("fixed to 0", notes)))
    total <- effect_row(results, "Total", "SVO ⇒ BEH")
    expect_equal(total$est, indirect$est, tolerance = 1e-6)
    expect_equal(total$se, indirect$se, tolerance = 1e-6)
})

test_that("an independent variable without a direct path keeps the others' effects", {
    results <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = c("cprime", "SVO"),
                       mediatorsTerms = list(list("cprime", "SVO")), modelTerms = list("EXP", "SVO"))
    fit <- lavaan::sem("EXP ~ a1*cprime + a2*SVO
                        BEH ~ b*EXP + c*SVO
                        totalCprime := a1*b
                        totalSVO := c + a2*b", data = coopmedmod, likelihood = "wishart")
    estimates <- lavaan::parameterEstimates(fit)
    expect_identical(effect_row(results, "Direct", "cprime ⇒ BEH")$est, 0)
    expect_equal(effect_row(results, "Direct", "SVO ⇒ BEH")$est,
                 estimates$est[estimates$label == "c"], tolerance = 1e-6)
    expect_equal(effect_row(results, "Total", "cprime ⇒ BEH")$se,
                 estimates$se[estimates$label == "totalCprime"], tolerance = 1e-6)
    expect_equal(effect_row(results, "Total", "SVO ⇒ BEH")$se,
                 estimates$se[estimates$label == "totalSVO"], tolerance = 1e-6)
})

test_that("models with every direct path estimated have no fixed-effect note", {
    results <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                       mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"))
    expect_length(results$models$coefficients$notes, 0)
})

test_that("variable names that start like another variable's are shown correctly", {
    # SVO2 is encoded with the same prefix as SVO.
    dat <- coopmedmod
    set.seed(3)
    dat$SVO2 <- stats::rnorm(nrow(dat))
    results <- jammGLM(data = dat, dep = "BEH", mediators = "EXP", covs = c("SVO", "SVO2"),
                       mediatorsTerms = list(list("SVO", "SVO2")), modelTerms = list("EXP", "SVO", "SVO2"))
    effects <- gsub("\\s+", "", results$models$coefficients$asDF$effect)
    expect_true("SVO2⇒EXP⇒BEH" %in% effects)
    expect_false(any(grepl("SVOMg", effects)))
    expect_true("SVO2 ⇒ EXP ⇒ BEH" %in% results$infotab$asDF$value)
})

test_that("an ordinal dependent variable from jamovi is analysed with its numeric values", {
    # jamovi passes an ordinal integer variable as a factor with a "values" attribute.
    dat <- coopmedmod
    dat$numBEH <- round(dat$BEH / 10)
    dat$ordBEH <- factor(dat$numBEH)
    attr(dat$ordBEH, "values") <- as.integer(levels(dat$ordBEH))
    ordinal <- jammGLM(data = dat, dep = "ordBEH", mediators = "EXP", covs = "SVO",
                       mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"))
    numeric <- jammGLM(data = dat, dep = "numBEH", mediators = "EXP", covs = "SVO",
                       mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"))
    expect_equal(ordinal$models$coefficients$asDF$est, numeric$models$coefficients$asDF$est,
                 tolerance = 1e-6)
})

test_that("the formula interface gives the same model as the explicit arguments", {
    explicit <- jammGLM(data = coopmedmod, dep = "BEH", mediators = "EXP", covs = "SVO",
                        mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"))
    fromFormulas <- jammGLM(formula = list(EXP ~ SVO, BEH ~ EXP + SVO), data = coopmedmod)
    fromString <- jammGLM(formula = "EXP ~ SVO; BEH ~ EXP + SVO", data = coopmedmod)
    expect_equal(fromFormulas$models$coefficients$asDF, explicit$models$coefficients$asDF)
    expect_equal(fromString$models$coefficients$asDF, explicit$models$coefficients$asDF)
    expect_error(jammGLM(formula = list("EXP ~ SVO", "~ SVO"), data = coopmedmod),
                 "Invalid formula entry 2")
})
