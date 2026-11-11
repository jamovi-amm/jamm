test_that("model classes use Scaffold and dispatch warnings to result tables", {
    dataset <- coopmedmod_data(c("cprime", "EXP", "BEH"))
    options <- jammGLMOptions$new(
        dep = "BEH", mediators = "EXP", covs = "cprime",
        mediatorsTerms = list(list("cprime")), modelTerms = list("EXP", "cprime")
    )
    results <- jammGLMResults$new(options)
    analysisObject <- list(options = options, results = results, data = dataset)
    datamatic <- Datamatic$new(analysisObject)
    runner <- Runner$new(analysisObject, datamatic)
    estimator <- runner$estimator
    plotter <- Plotter$new(analysisObject, datamatic, estimator, results$pathgroup)

    for (modelObject in list(datamatic, estimator, plotter)) {
        expect_identical(modelObject$analysis, analysisObject)
        expect_identical(modelObject$options, options)
        expect_identical(modelObject$vars, c("BEH", "EXP", "cprime"))
    }

    estimator$warning <- list(topic = "infotab", message = "Ordinary warning text")
    expect_equal(results$infotab$notes[[1]]$note, "Ordinary warning text")
    estimator$warning <- list(topic = "infotab", message = "Ordinary warning text")
    expect_length(results$infotab$notes, 1)

    estimator$warning <- list(topic = "models_r2", message = "R-squared warning")
    expect_equal(results$models$r2$notes[[1]]$note, "R-squared warning")

    encodedWarning <- paste("Check", jmvcore::toB64("cprime"))
    estimator$warning <- list(topic = "infotab", message = encodedWarning)
    expect_equal(results$infotab$notes[[2]]$note, "Check cprime")

    plotter$warning <- list(topic = "pathgroup_notes", message = "Diagram warning")
    expect_equal(results$pathgroup$notes$notes[[1]]$note, "Diagram warning")
})

test_that("Base64 decoding preserves prose and still decodes variable identifiers", {
    for (message in c("Ordinary warning text", "Model cannot be estimated: no variance",
                      "X is a variable", "Warning: café", "", "X%%%", "X_w")) {
        expect_identical(.fromb64.default(message), message)
    }
    variables <- c("BEH", "cprime", "café", "a+b", "X")
    decoded <- vapply(variables, function(variable) {
        .fromb64.default(jmvcore::toB64(variable))
    }, character(1), USE.NAMES = FALSE)
    expect_identical(decoded, variables)
})
