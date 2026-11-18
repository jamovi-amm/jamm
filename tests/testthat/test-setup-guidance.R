test_that("incomplete models display the readiness reason in the setup value column", {
    dataset <- coopmedmod_data(c("cprime", "EXP", "BEH", "SVO"))
    defaults <- list(
        dep = "BEH", mediators = "EXP", covs = "cprime",
        mediatorsTerms = list(list("cprime")), modelTerms = list("EXP", "cprime")
    )
    cases <- list(
        list(changes = list(dep = NULL), reason = "A dependent variable is needed."),
        list(changes = list(mediators = NULL), reason = "At least one mediator variable is needed."),
        list(changes = list(covs = NULL), reason = "At least one independent variable is needed."),
        list(changes = list(mediatorsTerms = list()),
             reason = "Predictors are not specified for 1 mediator variable."),
        list(changes = list(mediators = c("EXP", "SVO"), mediatorsTerms = list()),
             reason = "Predictors are not specified for 2 mediator variables."),
        list(changes = list(moderatorsTerms = list(list("cprime"))),
             reason = "At least one independent variable must remain outside the moderators."),
        list(changes = list(modelTerms = list()), reason = "The full model is not specified.")
    )

    for (case in cases) {
        arguments <- defaults
        arguments[names(case$changes)] <- case$changes
        options <- do.call(jammGLMOptions$new, arguments)
        status <- readiness(options)
        expect_false(status$ready)
        expect_true(status$report)
        expect_identical(status$reason, case$reason)

        analysis <- jammGLMClass$new(options = options, data = dataset)
        analysis$run()
        setup <- analysis$results$infotab$asDF
        expect_equal(nrow(setup), 1)
        expect_identical(setup$info, "Setup")
        expect_identical(setup$value, case$reason)
        expect_null(analysis$.__enclos_env__$private$.runner_machine)
    }
})

test_that("complete models do not report missing setup", {
    dataset <- coopmedmod_data(c("cprime", "EXP", "BEH"))
    options <- jammGLMOptions$new(
        dep = "BEH", mediators = "EXP", covs = "cprime",
        mediatorsTerms = list(list("cprime")), modelTerms = list("EXP", "cprime")
    )
    expect_true(readiness(options)$ready)
    expect_false(readiness(options)$report)
    expect_null(readiness(options)$reason)
    analysis <- jammGLMClass$new(options = options, data = dataset)
    analysis$run()
    expect_false("Setup" %in% analysis$results$infotab$asDF$info)
})
