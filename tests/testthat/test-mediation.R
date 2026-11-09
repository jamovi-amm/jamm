test_that("simple mediation matches independent regression path products", {
    dataset <- coopmedmod_data(c("cprime", "EXP", "BEH"))
    first <- stats::lm(EXP ~ cprime, data = dataset)
    outcome <- stats::lm(BEH ~ EXP + cprime, data = dataset)
    total <- stats::lm(BEH ~ cprime, data = dataset)

    result <- jammGLM(
        data = dataset, dep = "BEH", mediators = "EXP", covs = "cprime",
        mediatorsTerms = list(list("cprime")), modelTerms = list("EXP", "cprime"),
        scaling = list(list(var = "cprime", type = "none"), list(var = "EXP", type = "none")),
        tableOptions = c("beta", "component", "regression")
    )
    effects <- result$models$coefficients$asDF
    indirect <- unname(stats::coef(first)["cprime"] * stats::coef(outcome)["EXP"])
    direct <- unname(stats::coef(outcome)["cprime"])

    expect_equal(effects$est[effects$type == "Indirect"], indirect, tolerance = 1e-6)
    expect_equal(effects$est[effects$type == "Direct"], direct, tolerance = 1e-6)
    expect_equal(effects$est[effects$type == "Total"], indirect + direct, tolerance = 1e-6)
    expect_equal(effects$est[effects$type == "Total"],
                 unname(stats::coef(total)["cprime"]), tolerance = 1e-6)
    expect_equal(effects$est[effects$type == "Component"],
                 unname(c(stats::coef(first)["cprime"], stats::coef(outcome)["EXP"])), tolerance = 1e-6)
    expect_true(all(is.finite(effects$se)))
    expect_true(all(effects$ci.lower <= effects$est & effects$est <= effects$ci.upper))

    r2 <- result$models$r2$asDF
    expect_equal(r2$r2[match(c("EXP", "BEH"), r2$lhs)],
                 c(summary(first)$r.squared, summary(outcome)$r.squared), tolerance = 1e-6)
    expect_gt(nrow(result$regressions$full$regression$asDF), 0)
    expect_gt(nrow(result$regressions$mediator_regressions$get(key = "EXP")$regression$asDF), 0)
})

test_that("moderated mediation estimates effects at each moderator level", {
    dataset <- coopmedmod_data(c("cprime", "SVO", "EXP", "BEH"))
    continuous <- c("cprime", "SVO", "EXP")
    reference <- dataset
    for (variable in continuous)
        reference[[variable]] <- reference[[variable]] - mean(reference[[variable]])
    first <- stats::lm(EXP ~ cprime * SVO, data = reference)
    outcome <- stats::lm(BEH ~ EXP * SVO + cprime * SVO, data = reference)

    result <- jammGLM(
        data = dataset, dep = "BEH", mediators = "EXP", covs = c("cprime", "SVO"),
        mediatorsTerms = list(list("cprime", "SVO", c("cprime", "SVO"))),
        modelTerms = list("EXP", "cprime", "SVO", c("EXP", "SVO"), c("cprime", "SVO")),
        moderatorsTerms = list(list("SVO")),
        scaling = lapply(continuous, function(variable) list(var = variable, type = "centered")),
        simpleScale = "mean_sd", cvalue = 1, simpleScaleLabels = "labels"
    )
    effects <- result$models$coefficients$asDF
    levels <- mean(reference$SVO) + c(-1, 0, 1) * stats::sd(reference$SVO)

    # Evaluate (a + aW * W) * (b + bW * W), not just a * b at W = 0.
    firstSlope <- unname(stats::coef(first)["cprime"] + stats::coef(first)["cprime:SVO"] * levels)
    secondSlope <- unname(stats::coef(outcome)["EXP"] + stats::coef(outcome)["EXP:SVO"] * levels)
    direct <- unname(stats::coef(outcome)["cprime"] + stats::coef(outcome)["SVO:cprime"] * levels)
    indirect <- firstSlope * secondSlope
    indirectRows <- effects[effects$type == "Indirect", ]

    expect_equal(indirectRows$condition,
                 c("SVO = Mean - 1 SD", "SVO = Mean", "SVO = Mean + 1 SD"))
    expect_equal(indirectRows$est, indirect, tolerance = 1e-6)
    expect_equal(effects$est[effects$type == "Direct"], direct, tolerance = 1e-6)
    expect_equal(effects$est[effects$type == "Total"], indirect + direct, tolerance = 1e-6)
    expect_equal(effects$est[effects$type == "Component"],
                 as.vector(rbind(firstSlope, secondSlope)), tolerance = 1e-6)
    expect_equal(length(unique(indirectRows$est)), 3L)
    expect_true(all(is.finite(effects$se)))
    expect_true(all(effects$ci.lower <= effects$est & effects$est <= effects$ci.upper))
    expect_equal(nrow(result$models$interactions$asDF), 3L)
})
