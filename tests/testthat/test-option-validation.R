test_that("conditioning offsets have a positive lower bound", {
    for (offset in c(-1, 0, 0.099)) {
        options <- jammGLMOptions$new(cvalue = offset)
        expect_error(options$check(checkValues = TRUE), "must be between")
    }
    for (offset in c(0.1, 1, 2)) {
        options <- jammGLMOptions$new(cvalue = offset)
        expect_no_error(options$check(checkValues = TRUE))
        expect_equal(options$cvalue, offset)
    }
})

test_that("bootstrap counts must be whole numbers of at least 50", {
    for (count in c(0, 49)) {
        options <- jammGLMOptions$new(bootN = count)
        expect_error(options$check(checkValues = TRUE), "must be between")
    }
    for (count in c(50.5, 999.5)) {
        options <- jammGLMOptions$new(bootN = count)
        expect_error(options$check(checkValues = TRUE), "must be an integer")
    }
    for (count in c(50, 1000, 1000L)) {
        options <- jammGLMOptions$new(bootN = count)
        expect_no_error(options$check(checkValues = TRUE))
        expect_equal(options$bootN, count)
    }
})

test_that("the R interface rejects invalid offsets and fractional bootstrap counts", {
    dataset <- coopmedmod_data(c("cprime", "EXP", "BEH"))
    arguments <- list(
        data = dataset, dep = "BEH", mediators = "EXP", covs = "cprime",
        mediatorsTerms = list(list("cprime")), modelTerms = list("EXP", "cprime")
    )
    expect_error(do.call(jammGLM, c(arguments, list(cvalue = 0))), "must be between")
    expect_error(do.call(jammGLM, c(arguments, list(bootN = 999.5))), "must be an integer")
})
