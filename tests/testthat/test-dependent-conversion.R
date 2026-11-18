clean_dependent_variable <- function(outcome) {
    dataset <- data.frame(outcome = outcome)
    options <- jammGLMOptions$new(dep = "outcome")
    analysis <- list(options = options, results = jammGLMResults$new(options), data = dataset)
    datamatic <- Datamatic$new(analysis)
    datamatic$cleandata(dataset)[[tob64("outcome")]]
}

test_that("numeric dependent variables retain their values", {
    expect_identical(clean_dependent_variable(c(1, 2, 3)), c(1, 2, 3))
    expect_identical(clean_dependent_variable(1:3), 1:3)
})

test_that("nonnumeric factor dependent variables are rejected", {
    expect_error(
        clean_dependent_variable(factor(c("A", "B", "C"))),
        "The variable cannot be converted to numeric.",
        fixed = TRUE
    )
})
