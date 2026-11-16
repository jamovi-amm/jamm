test_that("a moderator cannot consume the last independent variable", {
    options <- list(
        dep = "BEH", mediators = "EXP", covs = "cprime",
        mediatorsTerms = list(list("cprime")), modelTerms = list("EXP", "cprime"),
        moderatorsTerms = list(list("cprime"))
    )
    status <- readiness(options)
    expect_false(status$ready)
    expect_true(status$report)
    expect_match(status$reason, "outside the moderators")

    options$covs <- c("cprime", "SVO")
    expect_true(readiness(options)$ready)
    options$moderatorsTerms <- list(list("cprime"), list("SVO"))
    expect_false(readiness(options)$ready)
    options$factors <- "prime"
    expect_true(readiness(options)$ready)
})

test_that("moderator UI rejects transfers even after its snapshot is refreshed", {
    skip_if_not_installed("V8")
    eventsPath <- test_path("..", "..", "jamovi", "js", "jamm.events.js")
    skip_if_not(file.exists(eventsPath), "UI sources are only available in the source checkout")
    engine <- V8::v8()
    engine$eval("var module = { exports: {} };")
    engine$source(eventsPath)
    engine$source(test_path("fixtures", "moderator-selection.js"))
    expect_true(engine$get("checksPassed"))
})
