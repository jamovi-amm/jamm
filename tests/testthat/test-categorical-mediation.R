test_that("categorical moderation includes coding-invariant centered-factor effects", {
    dataset <- coopmedmod_data(c("prime", "SVO", "EXP", "BEH"))
    # Unequal group sizes distinguish the centered factor from sample weighting.
    dataset <- dataset[dataset$prime == 0 | seq_len(nrow(dataset)) <= 70, ]
    dataset$prime <- factor(dataset$prime)
    groupSlopes <- lapply(split(dataset, dataset$prime), function(group) {
        first <- stats::lm(EXP ~ SVO, data = group)
        outcome <- stats::lm(BEH ~ EXP + SVO, data = group)
        c(first = unname(stats::coef(first)["SVO"]),
          second = unname(stats::coef(outcome)["EXP"]),
          direct = unname(stats::coef(outcome)["SVO"]))
    })
    slopes <- do.call(rbind, groupSlopes)
    average <- colMeans(slopes)
    indirect <- average[["first"]] * average[["second"]]

    for (coding in c("simple", "deviation", "dummy")) {
        result <- jammGLM(
            data = dataset, dep = "BEH", mediators = "EXP", covs = "SVO", factors = "prime",
            contrasts = list(list(var = "prime", type = coding)),
            scaling = list(list(var = "SVO", type = "centered"), list(var = "EXP", type = "centered")),
            mediatorsTerms = list(list("SVO", "prime", c("SVO", "prime"))),
            modelTerms = list("EXP", "SVO", "prime", c("EXP", "prime"), c("SVO", "prime")),
            moderatorsTerms = list(list("prime"))
        )
        effects <- result$models$coefficients$asDF
        centered <- effects[effects$condition == "prime = Average", ]
        expect_equal(unique(effects$condition), c("prime = 0", "prime = 1", "prime = Average"))
        expect_equal(centered$est[centered$type == "Indirect"], indirect, tolerance = 1e-6)
        expect_equal(centered$est[centered$type == "Direct"], average[["direct"]], tolerance = 1e-6)
        expect_equal(centered$est[centered$type == "Total"], indirect + average[["direct"]], tolerance = 1e-6)
        expect_equal(centered$est[centered$type == "Component"],
                     unname(average[c("first", "second")]), tolerance = 1e-6)
        expect_true(all(is.finite(centered$se)))
        expect_true(all(centered$ci.lower <= centered$est & centered$est <= centered$ci.upper))
        expect_equal(effects$est[effects$type == "Indirect" & effects$condition != "prime = Average"],
                     unname(slopes[, "first"] * slopes[, "second"]), tolerance = 1e-6)
    }
})
