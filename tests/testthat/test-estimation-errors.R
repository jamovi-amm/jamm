test_that("failed moderated fits report the fitting error, not a NULL slot error", {
    dataset <- coopmedmod_data(c("cprime", "SVO", "EXP", "BEH"))
    options <- jammGLMOptions$new(
        dep = "BEH", mediators = "EXP", covs = c("cprime", "SVO"),
        mediatorsTerms = list(list("cprime", "SVO", c("cprime", "SVO"))),
        modelTerms = list("EXP", "cprime", "SVO", c("EXP", "SVO"), c("cprime", "SVO")),
        mod_terms = list(list("SVO"))
    )

    # Cover both an initial failure and failure after a successful fit.
    for (previousFit in c(FALSE, TRUE)) {
        results <- jammGLMResults$new(options)
        analysisObject <- list(options = options, results = results, data = dataset)
        datamatic <- Datamatic$new(analysisObject)
        runner <- Runner$new(analysisObject, datamatic)
        if (previousFit) {
            cleaned <- datamatic$cleandata(dataset, runner$estimator$interactions)
            runner$estimate(cleaned)
            expect_true(runner$ok)
            expect_s4_class(runner$estimator$model, "lavaan")
            expect_gt(length(runner$estimator$tab_info), 0)
        }

        # A constant moderator reliably triggers a genuine lavaan fitting error.
        invalid <- dataset
        invalid$SVO <- 1
        cleaned <- datamatic$cleandata(invalid, runner$estimator$interactions)
        invisible(capture.output(
            runner$estimate(cleaned)
        ))
        expect_false(runner$ok)
        expect_match(results$issues$content, "Model cannot be estimated")
        expect_match(results$issues$content, "no variance")
        expect_true(results$issues$visible)
        expect_null(runner$estimator$model)
        expect_false(runner$estimator$ok)

        cleaned <- datamatic$cleandata(dataset, runner$estimator$interactions)
        runner$estimate(cleaned)
        expect_true(runner$ok)
        expect_true(runner$estimator$ok)
        expect_s4_class(runner$estimator$model, "lavaan")
    }

    # The analysis must leave the notice visible without running tables or plots.
    analysis <- jammGLMClass$new(options = options, data = invalid)
    invisible(capture.output(analysis$run()))
    expect_match(analysis$results$issues$content, "Model cannot be estimated")
    expect_match(analysis$results$issues$content, "no variance")
    expect_true(analysis$results$issues$visible)
})

test_that("failed fits preserve earlier lavaan diagnostics in the error box", {
    dataset <- coopmedmod_data(c("cprime", "EXP", "BEH"))
    options <- jammGLMOptions$new(
        dep = "BEH", mediators = "EXP", covs = "cprime",
        mediatorsTerms = list(list("cprime")), modelTerms = list("EXP", "cprime")
    )
    diagnostic <- paste(
        "lavaan->lav_samplestats_icov():",
        "sample covariance matrix is not positive-definite"
    )
    with_mocked_bindings({
        analysis <- jammGLMClass$new(options = options, data = dataset)
        expect_no_warning(invisible(capture.output(analysis$run())))
        expect_true(analysis$results$issues$visible)
        expect_match(analysis$results$issues$content, diagnostic, fixed = TRUE)
        expect_match(analysis$results$issues$content,
                     "Final error: object 'icov' not found", fixed = TRUE)
        notes <- vapply(analysis$results$infotab$notes, function(note) note$note, character(1))
        expect_true(diagnostic %in% notes)
    }, lavaan = function(...) {
        warning(diagnostic, call. = FALSE)
        stop("object 'icov' not found", call. = FALSE)
    }, .package = "lavaan")
})
