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
        # A failed fit stops the analysis, so jamovi reports it as an analysis error.
        # lavaan prints its variable table on this failure; keep test output clean.
        expect_error(invisible(capture.output(runner$estimate(cleaned))),
                     "Model cannot be estimated.*no variance")
        expect_null(runner$estimator$model)
        expect_false(runner$estimator$ok)

        cleaned <- datamatic$cleandata(dataset, runner$estimator$interactions)
        runner$estimate(cleaned)
        expect_true(runner$ok)
        expect_true(runner$estimator$ok)
        expect_s4_class(runner$estimator$model, "lavaan")
    }

    # jamovi runs with noThrow = TRUE: the error is stored on the results and
    # no table or plot is filled from the failed fit.
    analysis <- jammGLMClass$new(options = options, data = invalid)
    invisible(capture.output(analysis$run(noThrow = TRUE)))
    expect_identical(analysis$status, "error")
    expect_match(analysis_error(analysis), "Model cannot be estimated")
    expect_match(analysis_error(analysis), "no variance")
    expect_length(analysis$results$models$coefficients$asDF$est[!is.na(analysis$results$models$coefficients$asDF$est)], 0)
})

test_that("failed fits preserve earlier lavaan diagnostics in the error message", {
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
        expect_no_warning(analysis$run(noThrow = TRUE))
        expect_identical(analysis$status, "error")
        expect_match(analysis_error(analysis), diagnostic, fixed = TRUE)
        expect_match(analysis_error(analysis), "Final error: object 'icov' not found", fixed = TRUE)
        # The warning raised before the failure is also kept in the notice.
        expect_match(notice_content(analysis$results), diagnostic, fixed = TRUE)
    }, lavaan = function(...) {
        warning(diagnostic, call. = FALSE)
        stop("object 'icov' not found", call. = FALSE)
    }, .package = "lavaan")
})
