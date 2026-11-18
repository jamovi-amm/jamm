# LEGACY (jAMM < 2.0): tests for the `moderatorsTerms` -> `mod_terms` migration.
# Delete this file together with R/legacy.R when the deprecation takes effect.

legacy_model <- list(
    dep = "BEH", mediators = "EXP", covs = c("cprime", "SVO"),
    mediatorsTerms = list(list("cprime", "SVO", c("cprime", "SVO"))),
    modelTerms = list("EXP", "cprime", "SVO", c("EXP", "SVO"), c("cprime", "SVO"))
)

run_options <- function(...) {
    options <- do.call(jammGLMOptions$new, c(legacy_model, list(...)))
    analysis <- jammGLMClass$new(options = options, data = coopmedmod_data(c("cprime", "SVO", "EXP", "BEH")))
    analysis$run()
    analysis
}

test_that("a jAMM 1.x analysis with moderatorsTerms gives the same results as mod_terms", {
    current <- run_options(mod_terms = list(list("SVO")))
    legacy <- run_options(moderatorsTerms = list(list("SVO")))
    coefficients <- legacy$results$models$coefficients
    expect_true(coefficients$getColumn("condition")$visible)
    expect_true(any(grepl("SVO", coefficients$asDF$condition)))
    expect_equal(coefficients$asDF, current$results$models$coefficients$asDF)
    expect_identical(legacy$options$mod_terms, list(list("SVO")))
    expect_length(unlist(legacy$options$moderatorsTerms), 0)
})

test_that("mod_terms wins when both options are set", {
    both <- run_options(mod_terms = list(list("SVO")), moderatorsTerms = list(list("cprime")))
    expect_identical(both$options$mod_terms, list(list("SVO")))
    expect_length(unlist(both$options$moderatorsTerms), 0)
})

test_that("generated syntax never mentions the legacy option", {
    legacy <- run_options(moderatorsTerms = list(list("SVO")))
    source <- legacy$asSource()
    expect_false(grepl("moderatorsTerms", source, fixed = TRUE))
    expect_match(source, "mod_terms", fixed = TRUE)
})

test_that("jammGLM() accepts the deprecated moderatorsTerms argument with a warning", {
    dataset <- coopmedmod_data(c("cprime", "SVO", "EXP", "BEH"))
    arguments <- c(list(data = dataset), legacy_model)
    current <- do.call(jammGLM, c(arguments, list(mod_terms = list(list("SVO")))))
    expect_warning(
        legacy <- do.call(jammGLM, c(arguments, list(moderatorsTerms = list(list("SVO"))))),
        "'moderatorsTerms' is deprecated"
    )
    expect_equal(legacy$models$coefficients$asDF, current$models$coefficients$asDF)
})

test_that("the panel moves moderatorsTerms into mod_terms and empties it", {
    skip_if_not_installed("V8")
    eventsPath <- source_file("jamovi", "js", "jamm.events.js")
    skip_if_not(file.exists(eventsPath), "UI sources are only available in the source checkout")
    engine <- V8::v8()
    engine$eval("var module = { exports: {} };")
    engine$source(eventsPath)
    engine$eval("
        var copy = function(value) { return JSON.parse(JSON.stringify(value)); };
        var control = function(value) {
            return { current: value, value: function() { return this.current; },
                     setValue: function(value) { this.current = copy(value); } };
        };
        var context = { workspace: { initializing: false },
                        cloneArray: function(value, fallback) { return copy(value === undefined ? fallback : value); } };
        var run = function(legacy, current) {
            var ui = { moderatorsTerms: control(legacy), mod_terms: control(current) };
            migrateLegacyModerators(ui, context);
            return JSON.stringify([ui.mod_terms.value(), ui.moderatorsTerms.value(), context.workspace.initializing]);
        };
    ")
    # Legacy moderators move into the empty current option.
    expect_identical(engine$eval("run([[], [['SVO']]], [[], []])"), '[[[],[["SVO"]]],[[]],false]')
    # Existing current moderators are kept, and the legacy value is still emptied.
    expect_identical(engine$eval("run([['SVO']], [['cprime']])"), '[[["cprime"]],[[]],false]')
    # Nothing to migrate: nothing changes.
    expect_identical(engine$eval("run([[]], [['cprime']])"), '[[["cprime"]],[[]],false]')
    # A jamovi version without the hidden control must not break the panel.
    expect_identical(engine$eval("(function() { migrateLegacyModerators({ mod_terms: control([[]]) }, context); return 'ok'; })()"), "ok")
})
