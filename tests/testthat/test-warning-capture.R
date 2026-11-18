test_that("warning capture retains every warning by default", {
    messages <- paste("Distinct warning", seq_len(8))
    captured <- expect_no_warning(try_hard({
        for (message in messages)
            warning(message, call. = FALSE)
        42
    }))
    expect_identical(unlist(captured$warning, use.names = FALSE), messages)
    expect_identical(captured$obj, 42)
    expect_false(captured$error)
    expect_false(try_hard(NULL)$warning)
    expect_identical(try_hard(warning("Single warning"))$warning, "Single warning")

    limited <- try_hard({
        for (message in messages)
            warning(message, call. = FALSE)
    }, max_warn = 2)
    expect_identical(unlist(limited$warning, use.names = FALSE),
                     c(messages[1:2], "Additional warnings are present."))
})

test_that("fitting and parameter-extraction warnings reach the results together", {
    dataset <- coopmedmod_data(c("cprime", "EXP", "BEH"))
    options <- jammGLMOptions$new(
        dep = "BEH", mediators = "EXP", covs = "cprime",
        mediatorsTerms = list(list("cprime")), modelTerms = list("EXP", "cprime")
    )
    originalFit <- lavaan::lavaan
    originalParameters <- lavaan::parameterestimates
    messages <- paste("Parameter extraction warning", seq_len(8))
    with_mocked_bindings({
        analysis <- jammGLMClass$new(options = options, data = dataset)
        expect_no_warning(analysis$run())
        notes <- vapply(analysis$results$infotab$notes, function(note) note$note, character(1))
        expect_true(all(c("Fitting warning", messages) %in% notes))
        expect_false("Additional warnings are present." %in% notes)
        expect_false(analysis$results$issues$visible)
    }, lavaan = function(...) {
        warning("Fitting warning", call. = FALSE)
        originalFit(...)
    }, parameterestimates = function(...) {
        for (message in messages)
            warning(message, call. = FALSE)
        originalParameters(...)
    }, .package = "lavaan")
})

test_that("models without indirect paths warn without invalid total definitions", {
    dataset <- coopmedmod_data(c("cprime", "EXP", "BEH"))
    options <- jammGLMOptions$new(
        dep = "BEH", mediators = "EXP", covs = "cprime",
        mediatorsTerms = list(list("cprime")), modelTerms = list("cprime")
    )
    results <- jammGLMResults$new(options)
    analysisObject <- list(options = options, results = results, data = dataset)
    datamatic <- Datamatic$new(analysisObject)
    runner <- expect_no_error(Runner$new(analysisObject, datamatic))
    notes <- vapply(results$infotab$notes, function(note) note$note, character(1))
    expect_identical(unname(notes), WARNS[["noindirect"]])
    expect_length(runner$estimator$indirect_names, 0)
    expect_length(runner$estimator$total_names, 0)
    cleaned <- datamatic$cleandata(dataset, runner$estimator$interactions)
    expect_no_warning(runner$estimate(cleaned))
    expect_true(runner$ok)
    expect_s4_class(runner$estimator$model, "lavaan")
    analysis <- jammGLMClass$new(options = options, data = dataset)
    expect_no_error(analysis$run())
    expect_false(analysis$results$issues$visible)
    notes <- vapply(analysis$results$infotab$notes, function(note) note$note, character(1))
    expect_true(WARNS[["noindirect"]] %in% notes)
})
