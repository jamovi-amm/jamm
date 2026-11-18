test_that("interval columns are visible for every enabled confidence interval type", {
    for (ciType in c("standard", "bca.simple", "perc", "norm", "none")) {
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
