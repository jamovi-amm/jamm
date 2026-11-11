test_that("SmartTable treats zero-row data frames as no rows", {
    results <- jammGLMResults$new(jammGLMOptions$new())
    smartTable <- SmartTable$new(results$infotab)
    private <- smartTable$.__enclos_env__$private
    empty <- data.frame(info = character(), value = character())

    expect_null(private$.listify(empty))
    private$.fill(results$infotab, empty)
    expect_equal(results$infotab$rowCount, 0)
})

test_that("SmartTable still normalizes nonempty data frames and preserves metadata", {
    results <- jammGLMResults$new(jammGLMOptions$new())
    smartTable <- SmartTable$new(results$infotab)
    private <- smartTable$.__enclos_env__$private
    input <- data.frame(info = factor(c("first", "second")), value = c(1, 2),
                        row.names = c("one", "two"))
    attr(input, "keys") <- c("key1", "key2")
    attr(input, "titles") <- list(info = "Information")

    rows <- private$.listify(input)
    expect_length(rows, 2)
    expect_identical(names(rows), rownames(input))
    expect_identical(rows[[1]]$info, "first")
    expect_identical(rows[[1]]$value, 1)
    expect_identical(rows[[2]]$info, "second")
    expect_identical(rows[[2]]$value, 2)
    expect_identical(attr(rows, "keys"), attr(input, "keys"))
    expect_identical(attr(rows, "titles"), attr(input, "titles"))
})

test_that("SmartTable normalizes vector rows regardless of column names or types", {
    table <- jmvcore::Table$new(options = jammGLMOptions$new(), name = "arbitrary")
    smartTable <- SmartTable$new(table)
    private <- smartTable$.__enclos_env__$private
    input <- list(
        text = c(label = "alpha", code = "A"),
        numeric = c(estimate = 1.25, uncertainty = 0.2),
        integer = c(count = 2L, total = 4L),
        logical = c(enabled = TRUE, selected = FALSE),
        mixed = list(label = "beta", estimate = 2, enabled = TRUE)
    )
    attr(input, "keys") <- names(input)
    attr(input, "titles") <- list(estimate = "Estimate")

    rows <- private$.listify(input)
    expect_length(rows, length(input))
    expect_identical(names(rows), names(input))
    for (rowName in names(input)) {
        expect_type(rows[[rowName]], "list")
        expect_identical(rows[[rowName]], as.list(input[[rowName]]))
    }
    expect_identical(attr(rows, "keys"), attr(input, "keys"))
    expect_identical(attr(rows, "titles"), attr(input, "titles"))
})

test_that("SmartTable fills arbitrary tables from vectors and lists interchangeably", {
    table <- jmvcore::Table$new(
        options = jammGLMOptions$new(), name = "arbitrary",
        columns = list(
            list(name = "metric", title = "Metric", type = "text"),
            list(name = "value", title = "Value", type = "text")
        )
    )
    smartTable <- SmartTable$new(table)
    private <- smartTable$.__enclos_env__$private

    private$.fill(table, list(c(metric = "alpha", value = "one"),
                             list(metric = "beta", value = "two")))
    expect_equal(table$rowCount, 2)
    expect_equal(table$asDF$metric, c("alpha", "beta"))
    expect_equal(table$asDF$value, c("one", "two"))

    private$.fill(table, list(list(metric = "gamma", value = "three"),
                             c(metric = "delta", value = "four")))
    expect_equal(table$rowCount, 2)
    expect_equal(table$asDF$metric, c("gamma", "delta"))
    expect_equal(table$asDF$value, c("three", "four"))
})
