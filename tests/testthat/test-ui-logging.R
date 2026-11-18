test_that("UI diagnostics are silent by default and use the logging helper", {
    eventsPath <- test_path("..", "..", "jamovi", "js", "jamm.events.js")
    skip_if_not(file.exists(eventsPath), "UI sources are only available in the source checkout")
    source <- readLines(eventsPath)
    expect_equal(sum(grepl("console.log(", source, fixed = TRUE)), 1)

    skip_if_not_installed("V8")
    engine <- V8::v8()
    engine$eval(paste(
        "var module = { exports: {} };",
        "var debugMessages = [];",
        "var console = { log: function(message) { debugMessages.push(message); } };"
    ))
    engine$source(eventsPath)
    engine$eval(paste(
        "log('Diagnostic');",
        "module.exports.onUpdate_modelSupplier({});",
        "module.exports.onEvent_nothing({});"
    ))
    engine$source(test_path("fixtures", "moderator-selection.js"))
    expect_true(engine$get("checksPassed"))
    expect_equal(engine$eval("debugMessages.length"), "0")
})
