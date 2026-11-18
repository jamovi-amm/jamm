test_that("jamovi warnings use a native notice on generic results", {
    options <- jmvcore::Options$new()
    results <- jmvcore::Group$new(options, name = "results")
    existing <- jmvcore::Html$new(options, name = "existing")
    results$add(existing)
    dispatch <- Dispatch$new(results)

    dispatch$warnings <- list(topic = "jamovi", message = "First warning")
    notice <- results$get(".estWarning")
    expect_s3_class(notice, "Notice")
    expect_identical(notice$type, jmvcore::NoticeType$WARNING)
    expect_identical(notice$content, "First warning")
    expect_identical(results$items[[1]], notice)
    expect_identical(results$items[[2]], existing)
    expect_true(notice$visible)

    dispatch$warnings <- list(topic = "jamovi", message = "Second warning")
    dispatch$warnings <- list(topic = "jamovi", message = "First warning")
    expect_length(results$items, 2)
    expect_identical(notice$content, "First warning\n\nSecond warning")

    anotherDispatch <- Dispatch$new(results)
    anotherDispatch$warnings <- list(topic = "jamovi", message = "Third warning")
    expect_identical(notice$content, "First warning\n\nSecond warning\n\nThird warning")
    anotherDispatch$warnings <- list(topic = "jamovi", message = "Updated warning", key = "updated")
    anotherDispatch$warnings <- list(topic = "jamovi", message = "Replacement warning", key = "updated")
    expect_match(notice$content, "Replacement warning", fixed = TRUE)
    expect_false(grepl("Updated warning", notice$content, fixed = TRUE))
})

test_that("jamovi warnings decode messages and work with empty results", {
    options <- jmvcore::Options$new()
    results <- jmvcore::Group$new(options, name = "results")
    dispatch <- Dispatch$new(results)
    dispatch$warnings <- list(topic = "jamovi", message = FALSE)
    dispatch$warnings <- list(topic = "jamovi", message = NULL)
    expect_length(results$items, 0)
    dispatch$warnings <- list(topic = "jamovi", message = jmvcore::toB64("cprime"))
    expect_length(results$items, 1)
    expect_identical(results$get(".estWarning")$content, "cprime")
})

test_that("jamovi warnings use ordinary R warnings for the R interface", {
    options <- jmvcore::Options$new()
    results <- jmvcore::Group$new(options, name = "results")
    dispatch <- Dispatch$new(results)
    dispatch$interface <- "R"
    expect_warning(dispatch$warnings <- list(topic = "jamovi", message = "R warning"),
                   "R warning", fixed = TRUE)
    expect_length(results$items, 0)
})
