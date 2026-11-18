test_that("factor expansion replaces every factor without retaining earlier terms", {
    terms <- list("first", "second", "third", c("first", "second"),
                  c("first", "second", "third"), "continuous")
    sizes <- c(first = 1L, second = 2L, third = 1L)
    expected <- list(
        paste0("first", FACTOR_SYMBOL, 1),
        paste0("second", FACTOR_SYMBOL, 1), paste0("second", FACTOR_SYMBOL, 2),
        paste0("third", FACTOR_SYMBOL, 1),
        c(paste0("first", FACTOR_SYMBOL, 1), paste0("second", FACTOR_SYMBOL, 1)),
        c(paste0("first", FACTOR_SYMBOL, 1), paste0("second", FACTOR_SYMBOL, 2)),
        c(paste0("first", FACTOR_SYMBOL, 1), paste0("second", FACTOR_SYMBOL, 1),
          paste0("third", FACTOR_SYMBOL, 1)),
        c(paste0("first", FACTOR_SYMBOL, 1), paste0("second", FACTOR_SYMBOL, 2),
          paste0("third", FACTOR_SYMBOL, 1)), "continuous"
    )
    for (order in list(names(sizes), rev(names(sizes)))) {
        expanded <- Syntax$private_methods$.factorlist(terms, sizes[order])
        expect_equal(expanded, expected)
        expect_false(any(unlist(expanded) %in% names(sizes)))
    }
})

test_that("multiple-factor mediation converges and matches independent regressions", {
    dataset <- coopmedmod_data(c("prime", "SVO", "EXP", "BEH"))
    dataset$prime <- factor(dataset$prime)
    for (levelCount in c(2L, 3L)) {
        dataset$group <- factor(rep(letters[seq_len(levelCount)], length.out = nrow(dataset)))
        for (coding in c("simple", "dummy", "deviation")) {
            options <- jammGLMOptions$new(
                dep = "BEH", mediators = "EXP", covs = "SVO", factors = c("prime", "group"),
                contrasts = list(list(var = "prime", type = coding), list(var = "group", type = coding)),
                scaling = list(list(var = "EXP", type = "none"), list(var = "SVO", type = "none")),
                mediatorsTerms = list(list("SVO", "prime", "group", c("prime", "group"))),
                modelTerms = list("EXP", "SVO", "prime", "group", c("prime", "group"))
            )
            results <- jammGLMResults$new(options)
            analysisObject <- list(options = options, results = results, data = dataset)
            datamatic <- Datamatic$new(analysisObject)
            runner <- Runner$new(analysisObject, datamatic)
            cleaned <- datamatic$cleandata(dataset, runner$estimator$interactions)
            runner$estimate(cleaned)
            expect_true(runner$ok)
            expect_true(lavaan::lavInspect(runner$estimator$model, "converged"))
            parameters <- lavaan::parameterEstimates(runner$estimator$model)
            regressions <- parameters[parameters$op == "~", ]
            expect_false(any(regressions$rhs %in% tob64(c("prime", "group"))))
            for (outcome in c("EXP", "BEH")) {
                rows <- regressions[regressions$lhs == tob64(outcome), ]
                reference <- stats::lm.fit(
                    cbind(Intercept = 1, as.matrix(cleaned[rows$rhs])), cleaned[[tob64(outcome)]]
                )
                expect_equal(rows$est, unname(reference$coefficients[-1]), tolerance = 1e-6)
                expect_true(any(abs(rows$est) > 1e-6))
            }
            analysis <- jammGLMClass$new(options = options, data = dataset)
            expect_no_error(analysis$run())
            info <- analysis$results$infotab$asDF
            expect_identical(info$value[match("Converged", info$info)], "TRUE")
        }
    }
})
