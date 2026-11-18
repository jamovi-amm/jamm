# ============================================================================
# JAMOVI-ONLY GUARDS
#
# The other test files call jammGLM(), as an R user would. The bugs guarded
# here cannot show up that way: they only happen inside jamovi, because jamovi
#   - restores the previous results and reuses them unless an option listed in
#     a table's clearWith changed (stale results),
#   - runs the analysis with run(noThrow = TRUE) and shows errors and
#     warnings in its own way,
#   - redraws and exports images from their saved state, without .run(),
#   - drives the options panel with jamovi/jammglm.u.yaml and the JavaScript
#     in jamovi/js/jamm.events.js,
#   - reopens files saved by older versions of jAMM.
# These tests therefore read the jamovi source files and use the analysis
# classes directly. Run them with devtools::test() or
# devtools::test_active_file(); R CMD check runs them too.
# ============================================================================

# Path to a file of the source checkout. Under devtools::test() it is two levels
# up from tests/testthat; under R CMD check the sources are unpacked in
# jamm.Rcheck/00_pkg_src/jamm. Skips the test when the file is not available.
jamovi_source <- function(...) {
    candidates <- c(test_path("..", "..", ...), test_path("..", "..", "00_pkg_src", "jamm", ...))
    found <- candidates[file.exists(candidates)]
    if (length(found) == 0)
        skip("jamovi sources are only available in the source checkout")
    found[[1]]
}

read_jamovi_yaml <- function(name) {
    skip_if_not_installed("yaml")
    yaml::read_yaml(jamovi_source("jamovi", name))
}

option_names <- function() vapply(read_jamovi_yaml("jammglm.a.yaml")$options, function(o) o$name, "")

events_engine <- function() {
    skip_if_not_installed("V8")
    engine <- V8::v8()
    engine$eval("var module = { exports: {} };")
    engine$source(jamovi_source("jamovi", "js", "jamm.events.js"))
    engine
}

# In these tests the analysis is created as jamovi creates it:
# jammGLMOptions$new() with the panel values, then jammGLMClass$new(). Unlike
# jammGLM(), this sets no `.interface = "R"`, so errors and warnings take the
# jamovi route.

# ---- stale results (clearWith) ----------------------------------------------

# Copy the previous results into a freshly initialised analysis the way jamovi
# does (jmvcore Table$fromProtoBuf): cells are restored by row unless one of the
# changed options is in that table's clearWith.
restore_previous <- function(table, previous, changed) {
    if (any(changed %in% table$.__enclos_env__$private$.clearWith))
        return(invisible())
    for (row in seq_along(table$rowKeys)) {
        match <- which(vapply(previous$rowKeys, identical, logical(1), table$rowKeys[[row]]))
        if (length(match) > 0)
            table$setRow(rowNo = row, values = lapply(previous$getRow(rowNo = match[[1]]), function(cell) cell$value))
    }
}

test_that("changing an option never leaves the previous numbers on screen", {
    tables <- list(
        r2 = function(r) r$models$r2, coefficients = function(r) r$models$coefficients,
        info = function(r) r$infotab, overall = function(r) r$regressions$overall$regression,
        fullAnova = function(r) r$regressions$full$anova, full = function(r) r$regressions$full$regression)
    # The panel values before the change.
    previousOptions <- list(dep = "BEH", mediators = "EXP", covs = c("cprime", "SVO"),
                            mediatorsTerms = list(list("cprime", "SVO")),
                            modelTerms = list("EXP", "cprime", "SVO"),
                            scaling = list(list(var = "cprime", type = "centered"),
                                           list(var = "SVO", type = "centered"),
                                           list(var = "EXP", type = "centered")),
                            tableOptions = list("beta", "component", "regression"))
    # One option changed at a time.
    changes <- list(
        ciWidth = 90,
        mediatorsTerms = list(list("cprime")),
        modelTerms = list("EXP", "cprime", "SVO", c("cprime", "SVO")),
        scaling = list(list(var = "cprime", type = "standardized"),
                       list(var = "SVO", type = "standardized"),
                       list(var = "EXP", type = "standardized")))
    for (changed in names(changes)) {
        before <- jammGLMClass$new(options = do.call(jammGLMOptions$new, previousOptions),
                                   data = coopmedmod)
        before$run()
        newOptions <- previousOptions
        newOptions[[changed]] <- changes[[changed]]
        restored <- jammGLMClass$new(options = do.call(jammGLMOptions$new, newOptions),
                                     data = coopmedmod)
        restored$init()
        for (get in tables)
            restore_previous(get(restored$results), get(before$results), changed)
        restored$run()
        fresh <- jammGLMClass$new(options = do.call(jammGLMOptions$new, newOptions),
                                  data = coopmedmod)
        fresh$run()
        for (name in names(tables))
            expect_equal(tables[[name]](restored$results)$asDF, tables[[name]](fresh$results)$asDF,
                         info = paste(changed, "->", name))
    }
})

test_that("every table clears on every option that changes its values", {
    results <- read_jamovi_yaml("jammglm.r.yaml")
    options <- option_names()
    elements <- list()
    walk <- function(element, path) {
        elements[[path]] <<- element
        if (is.list(element$items)) # an Array's items is a string such as "(factors)"
            for (child in element$items) walk(child, paste(path, child$name, sep = "/"))
        if (is.list(element$template)) walk(element$template, paste(path, "template", sep = "/"))
    }
    for (element in results$items) walk(element, element$name)
    required <- c("data", "dep", "mediators", "factors", "covs", "modelTerms", "mediatorsTerms",
                  "mod_terms", "contrasts", "scaling", "ciType", "ciWidth", "bootN",
                  "simpleScale", "cvalue", "percvalue", "simpleScaleLabels", "tableOptions")
    fitted <- c("infotab", "models/r2", "models/interactions", "models/coefficients",
                "regressions/overall/anova", "regressions/overall/regression",
                "regressions/mediator_regressions/template/anova",
                "regressions/mediator_regressions/template/regression",
                "regressions/full/anova", "regressions/full/regression")
    for (path in fitted)
        expect_length(setdiff(required, unlist(elements[[path]]$clearWith)), 0)
    for (path in names(elements))
        expect_length(setdiff(unlist(elements[[path]]$clearWith), c(options, "data")), 0)
})

# ---- how jamovi shows errors and warnings ---------------------------------------

test_that("a failed fit becomes a jamovi analysis error, not a half-filled page", {
    # A constant predictor has no variance.
    dat <- coopmedmod
    dat$SVO <- 1
    options <- jammGLMOptions$new(dep = "BEH", mediators = "EXP", covs = "SVO",
                                  mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"))
    analysis <- jammGLMClass$new(options = options, data = dat)
    invisible(utils::capture.output(analysis$run(noThrow = TRUE)))
    expect_identical(analysis$status, "error")
    expect_match(analysis$results$.__enclos_env__$private$.error, "Model cannot be estimated")
    expect_true(all(is.na(analysis$results$models$coefficients$asDF$est)))
})

test_that("analysis warnings appear as a jamovi notice", {
    options <- jammGLMOptions$new(dep = "BEH", mediators = "EXP", covs = "SVO",
                                  mediatorsTerms = list(list("SVO")), modelTerms = list("SVO"))
    analysis <- jammGLMClass$new(options = options, data = coopmedmod)
    expect_no_warning(analysis$run(noThrow = TRUE))
    expect_identical(analysis$status, "complete")
    notice <- analysis$results$get(".estWarning")
    expect_s3_class(notice, "Notice")
    expect_match(notice$content, "Indirect effects cannot be computed")
})

test_that("in jamovi, a model that does not converge is an analysis error", {
    # Same example as test-convergence.R: SVO2 is exactly twice SVO.
    dat <- coopmedmod
    dat$SVO2 <- dat$SVO * 2
    options <- jammGLMOptions$new(dep = "BEH", mediators = "EXP", covs = c("SVO", "SVO2"),
                                  mediatorsTerms = list(list("SVO", "SVO2")),
                                  modelTerms = list("EXP", "SVO", "SVO2"))
    analysis <- jammGLMClass$new(options = options, data = dat)
    invisible(utils::capture.output(analysis$run(noThrow = TRUE)))
    expect_identical(analysis$status, "error")
    error <- analysis$results$.__enclos_env__$private$.error
    # lavaan 0.7: "did not converge"; lavaan 0.6 (jamovi's): "not positive-definite".
    expect_true(is.character(error) && grepl("Model cannot be estimated.*(converge|positive.definite)", error),
                info = "the analysis error should say why the model cannot be estimated")
    expect_true(all(is.na(analysis$results$models$coefficients$asDF$est)))
})

# ---- images redrawn and exported from saved state ---------------------------------

test_that("diagrams redraw from a saved state in an analysis that never ran", {
    options <- jammGLMOptions$new(dep = "BEH", mediators = "EXP", covs = "SVO",
                                  mediatorsTerms = list(list("SVO")), modelTerms = list("EXP", "SVO"),
                                  path_diagram = TRUE)
    before <- jammGLMClass$new(options = options, data = coopmedmod)
    before$run()
    saved <- list(model = before$results$pathgroup$pathmodel$state,
                  path = before$results$pathgroup$diagrams$items[[1]]$state)
    saved <- unserialize(serialize(saved, NULL))
    # jamovi: new analysis object, results restored from the .omv, no .run().
    reopened <- jammGLMClass$new(options = options, data = coopmedmod)
    reopened$init()
    reopened$results$pathgroup$pathmodel$setState(saved$model)
    reopened$results$pathgroup$diagrams$items[[1]]$setState(saved$path)
    expect_true(render_image(reopened$results$pathgroup$pathmodel)$drawn)
    expect_true(render_image(reopened$results$pathgroup$diagrams$items[[1]])$drawn)
})

# ---- the options panel ---------------------------------------------------------

test_that("every panel control is bound to an existing option and value", {
    ui <- read_jamovi_yaml("jammglm.u.yaml")
    options <- read_jamovi_yaml("jammglm.a.yaml")$options
    names(options) <- vapply(options, function(o) o$name, "")
    controls <- list()
    walk <- function(element) {
        if (!is.null(element$name)) controls[[length(controls) + 1]] <<- element
        for (child in c(element$children, element$template$children, element$columns)) walk(child)
    }
    walk(ui)
    for (control in controls) {
        option <- if (is.null(control$optionName)) control$name else control$optionName
        if (control$type %in% c("Supplier", "Label", "LayoutBox", "CollapseBox") || is.null(control$type))
            next
        if (!option %in% names(options))
            next # e.g. ListBox template columns (var, type) and supplier helpers
        if (!is.null(control$optionPart)) {
            values <- vapply(options[[option]]$options, function(o) if (is.list(o)) o$name else o, "")
            expect_true(control$optionPart %in% values, info = paste(option, control$optionPart))
        }
    }
    expect_false("moderatorsTerms" %in% vapply(controls, function(c) c$name, ""))
})

test_that("enable rules refer to existing controls and option values", {
    ui <- readLines(jamovi_source("jamovi", "jammglm.u.yaml"))
    rules <- unique(trimws(sub("^[[:space:]]*enable:[[:space:]]*", "", ui[grepl("^[[:space:]]*enable:", ui)])))
    names <- trimws(sub("^[[:space:]]*-?[[:space:]]*name:[[:space:]]*", "", ui[grepl("^[[:space:]]*-?[[:space:]]*name:", ui)]))
    options <- read_jamovi_yaml("jammglm.a.yaml")$options
    names(options) <- vapply(options, function(o) o$name, "")
    for (rule in rules) {
        terms <- regmatches(rule, gregexpr("[A-Za-z_][A-Za-z0-9_.]*(:[A-Za-z0-9_.]+)?", rule))[[1]]
        for (term in terms) {
            parts <- strsplit(term, ":", fixed = TRUE)[[1]]
            expect_true(parts[[1]] %in% c(names, names(options)), info = rule)
            if (length(parts) == 2) {
                values <- vapply(options[[parts[[1]]]]$options, function(o) if (is.list(o)) o$name else o, "")
                expect_true(parts[[2]] %in% values, info = rule)
                # jamovi's client-side parser does not accept a dot in a value name.
                expect_false(grepl(".", parts[[2]], fixed = TRUE), info = rule)
            }
        }
    }
})

test_that("a moderator cannot take the last independent variable in the panel", {
    engine <- events_engine()
    engine$source(test_path("fixtures", "moderator-selection.js"))
    expect_true(engine$get("checksPassed"))
})

test_that("panel diagnostics are switched off", {
    engine <- events_engine()
    engine$eval("var messages = []; console.log = function(m) { messages.push(m); };")
    engine$eval("module.exports.onUpdate_modelSupplier({}); module.exports.onEvent_nothing({});")
    expect_identical(engine$eval("messages.length"), "0")
})

# ---- files saved by older jAMM versions --------------------------------------------

test_that("jAMM 1.x option values are still accepted", {
    options <- read_jamovi_yaml("jammglm.a.yaml")$options
    names(options) <- vapply(options, function(o) o$name, "")
    # 1.2.4 saved bias-corrected intervals as 'bca' and moderators in moderatorsTerms.
    ciTypes <- vapply(options$ciType$options, function(o) o$name, "")
    expect_true(all(c("standard", "bca", "perc", "none") %in% ciTypes))
    legacy <- options$moderatorsTerms
    expect_true(isTRUE(legacy$hidden))
    expect_identical(legacy$type, options$mod_terms$type)
    expect_identical(legacy$template, options$mod_terms$template)
})

test_that("a jAMM 1.x analysis reopened in jamovi keeps its moderators", {
    currentOptions <- jammGLMOptions$new(
        dep = "BEH", mediators = "EXP", covs = c("cprime", "SVO"),
        mediatorsTerms = list(list("cprime", "SVO", c("cprime", "SVO"))),
        modelTerms = list("EXP", "cprime", "SVO", c("EXP", "SVO"), c("cprime", "SVO")),
        mod_terms = list(list("SVO")))
    current <- jammGLMClass$new(options = currentOptions, data = coopmedmod)
    current$run()
    # A 1.x file: moderators in the hidden legacy option, mod_terms left empty.
    legacyOptions <- jammGLMOptions$new(
        dep = "BEH", mediators = "EXP", covs = c("cprime", "SVO"),
        mediatorsTerms = list(list("cprime", "SVO", c("cprime", "SVO"))),
        modelTerms = list("EXP", "cprime", "SVO", c("EXP", "SVO"), c("cprime", "SVO")),
        moderatorsTerms = list(list("SVO")))
    legacy <- jammGLMClass$new(options = legacyOptions, data = coopmedmod)
    legacy$run()
    expect_equal(legacy$results$models$coefficients$asDF, current$results$models$coefficients$asDF)
})

test_that("opening the panel moves jAMM 1.x moderators into the current option", {
    engine <- events_engine()
    engine$eval("
        var copy = function(value) { return JSON.parse(JSON.stringify(value)); };
        var control = function(value) {
            return { current: value, value: function() { return this.current; },
                     setValue: function(value) { this.current = copy(value); } };
        };
        var context = { workspace: {}, cloneArray: function(v, f) { return copy(v === undefined ? f : v); } };
        var run = function(legacy, current) {
            var ui = { moderatorsTerms: control(legacy), mod_terms: control(current) };
            migrateLegacyModerators(ui, context);
            return JSON.stringify([ui.mod_terms.value(), ui.moderatorsTerms.value()]);
        };")
    expect_identical(engine$eval("run([[], [['SVO']]], [[], []])"), '[[[],[["SVO"]]],[[]]]')
    expect_identical(engine$eval("run([['SVO']], [['cprime']])"), '[[["cprime"]],[[]]]')
    expect_identical(engine$eval("(function() { migrateLegacyModerators({ mod_terms: control([[]]) }, context); return 'ok'; })()"), "ok")
})

# ---- module metadata ---------------------------------------------------------------

test_that("every citation used by the results has a reference entry", {
    references <- names(read_jamovi_yaml("00refs.yaml")$refs)
    results <- readLines(jamovi_source("jamovi", "jammglm.r.yaml"))
    citations <- trimws(sub("^[[:space:]]*refs:", "", results[grepl("^[[:space:]]+refs:", results)]))
    expect_length(setdiff(citations, references), 0)
})

test_that("the menu entry is defined in the analysis file and matches the manifest", {
    analysis <- read_jamovi_yaml("jammglm.a.yaml")
    manifest <- read_jamovi_yaml("0000.yaml")$analyses[[1]]
    for (field in c("menuGroup", "menuSubgroup", "menuTitle"))
        expect_identical(manifest[[field]], analysis[[field]], info = field)
    expect_identical(read_jamovi_yaml("0000.yaml")$version,
                     as.character(utils::packageVersion("jamm")))
})
