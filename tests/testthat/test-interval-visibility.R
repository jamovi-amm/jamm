test_that("interval columns are visible for every enabled confidence interval type", {
    for (ciType in c("standard", "bca", "perc", "norm", "none")) {
        options <- jammGLMOptions$new(ciType = ciType)
        results <- jammGLMResults$new(options)
        results$regressions$mediator_regressions$addItem("mediator")
        mediator <- results$regressions$mediator_regressions$get(key = "mediator")
        tables <- list(
            results$models$r2,
            results$models$interactions,
            results$models$coefficients,
            results$regressions$overall$regression,
            mediator$regression,
            results$regressions$full$regression
        )

        for (tableIndex in seq_along(tables)) {
            columns <- c("ci.lower", "ci.upper")
            for (columnName in columns) {
                expect_identical(
                    tables[[tableIndex]]$getColumn(columnName)$visible,
                    ciType != "none",
                    info = paste(ciType, tableIndex, columnName)
                )
            }
        }
    }
})

test_that("'bca' (the value saved by jAMM 1.x) requests lavaan's bias-corrected interval", {
    dataset <- coopmedmod_data(c("cprime", "EXP", "BEH"))
    requested <- character()
    originalParameters <- lavaan::parameterestimates
    for (ciType in c("bca", "perc", "norm")) {
        options <- jammGLMOptions$new(
            dep = "BEH", mediators = "EXP", covs = "cprime", ciType = ciType, bootN = 50,
            mediatorsTerms = list(list("cprime")), modelTerms = list("EXP", "cprime")
        )
        with_mocked_bindings({
            analysis <- jammGLMClass$new(options = options, data = dataset)
            analysis$run()
        }, parameterestimates = function(..., boot.ci.type) {
            requested <<- c(requested, boot.ci.type)
            originalParameters(..., boot.ci.type = boot.ci.type)
        }, .package = "lavaan")
        coefficients <- analysis$results$models$coefficients$asDF
        expect_false(anyNA(coefficients$ci.lower), info = ciType)
    }
    expect_identical(unique(requested), c("bca.simple", "perc", "norm"))
})
